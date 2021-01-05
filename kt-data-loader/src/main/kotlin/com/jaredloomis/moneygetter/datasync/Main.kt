package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import org.kodein.di.instance
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util.*
import java.util.concurrent.*
import kotlin.math.min
import kotlin.system.exitProcess

fun main(args: Array<String>) {
  CLIRunner().main(args)
}

class CLIRunner : CliktCommand() {
  private val log = LoggerFactory.getLogger(javaClass)

  private val queryConfigFile: File by option(help = "Path to query config file")
    .file(mustBeReadable = true)
    .required()
  // TODO make functional again
  private val batchSize: Int by option(help = "# of tickers per data source query")
    .int()
    .default(Int.MAX_VALUE)
  private val concurrentQueries: Int by option(help = "# of data source queries to execute at the same time")
    .int()
    .default(6)
  private val plan: Boolean by option(help = "No-op - print out planned queries")
    .flag(default = false)

  private val mapper by di.instance<ObjectMapper>()
  private val props by di.instance<Properties>()
  private val db by di.instance<StockDatabase>()

  // NOTE: Must be updated every time a new indicator class is added
  private val INDICATOR_MODEL_CLASSES = emptyMap<String, Class<IsIndicatorSample>>()
    .plus(Pair(PriceSample.INDICATOR_ID, PriceSample::class.java))
    .plus(Pair(SentimentSample.INDICATOR_ID, SentimentSample::class.java))

  private var indicatorConfigCache: IndicatorConfig? = null

  override fun run() {
    val executor = createExecutorService()

    // Kill child processes if this process is killed (?)
    Runtime.getRuntime().addShutdownHook(Thread {
      executor.shutdownNow()
      executor.awaitTermination(10, TimeUnit.SECONDS)
    })

    val queryConfig = loadQueryConfig()
    log.debug("Running With Config $queryConfigFile:\n$queryConfig")

    // Create query plan
    val fetchPlans = getScheduler().createFetchPlan(queryConfig.queries.asSequence(), batchSize=batchSize)

    // If --plan flag is present, print plan and exit
    if(plan) {
      fetchPlans.forEach { log.info(it.toString()) }
      exitProcess(0)
    }

    // Execute queries
    fetchPlans.forEach { fetchPlan ->
      val processBuilder = ProcessBuilder(fetchPlan.getCommand())
        .directory(Paths.get("..").toFile())
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .redirectError(ProcessBuilder.Redirect.to(Paths.get("data-source.error.log").toFile()))
      executor.execute {
        log.info("Starting Process ${processBuilder.command()}")
        val proc = processBuilder.start()
        proc.inputStream.bufferedReader().lines().forEach { output ->
          val outputTeaser = output.substring(0, min(output.length, 250)) + " ..."
          log.info("Received output from ${processBuilder.command()}: $outputTeaser")
          log.debug("Output: $output")
          processOutputLine(fetchPlan.indicator.name, output)
        }
        proc.onExit().join()
      }
    }

    executor.shutdown()
    executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)

    if(executor.isShutdown) {
      log.info("Successfully completed")
    } else {
      log.error("Executor didn't shutdown")
    }
  }

  private fun processOutputLine(indicatorID: String, line: String): List<IndicatorSample> {
    try {
      // Read to JSON
      val json = mapper.readTree(line)
      val errors = LinkedList<String>()
      val rawSamples = json.elements().asSequence().mapNotNull {
        if (it.isTextual) {
          errors.push(it.asText())
          null
        } else {
          logger.debug("Output from $indicatorID:\n$line")
          mapper.treeToValue(it, INDICATOR_MODEL_CLASSES[indicatorID.split('-')[0]])
        }
      }.toList()

      // Create an indicator sample
      val samples = rawSamples.map { it.asSample() }

      // Insert sample into the DB
      samples.forEach { sample ->
        log.info("Inserting sample $sample")
        db.indicatorSampleQueries.insert(
          sample.sample_id,
          sample.stock_id,
          sample.indicator_id,
          sample.fetch_source_id,
          sample.start_time,
          sample.end_time,
          sample.fetch_time,
          sample.indicator_value,
          sample.indicator_raw
        )
      }

      return samples
    } catch(ex: Exception) {
      log.error("Couldn't process output line of $indicatorID: $line\n$ex")
      throw ex
    }
  }

  private fun createExecutorService(): ExecutorService {
    return ThreadPoolExecutor(1, concurrentQueries, 30L, TimeUnit.MINUTES, LinkedBlockingQueue<Runnable>())
  }

  private fun loadQueryConfig(): IndicatorQueryConfig {
    return mapper.readValue(queryConfigFile, IndicatorQueryConfig::class.java)
  }

  private fun getScheduler(): IndicatorScheduler {
    return IndicatorScheduler(getIndicatorConfig().getGroups())
  }

  private fun getIndicatorConfig(): IndicatorConfig {
    if(indicatorConfigCache != null) {
      return indicatorConfigCache!!
    }

    // Get command template from config file
    val indicatorsMap = Paths.get("..").resolve(props["indicators.map.path"] as String)
    val indicatorsJson = Files.readString(indicatorsMap)
    // Create command string from template and query
    val conf = IndicatorConfig(indicatorsJson)
    indicatorConfigCache = conf
    return conf
  }
}
