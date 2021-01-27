package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import org.apache.log4j.AppenderSkeleton
import org.apache.log4j.FileAppender
import org.apache.log4j.Level
import org.apache.log4j.PatternLayout
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
  private val batchSize: Int by option(help = "# of tickers per data source query")
    .int()
    .default(Int.MAX_VALUE)
  private val concurrentQueries: Int by option(help = "# of data source queries to execute at the same time")
    .int()
    .default(6)
  private val plan: Boolean by option(help = "Don't fetch any data - just print out planned queries")
    .flag(default = false)
  private val noLog: Boolean by option(help = "Don't print out logs - just return the result. Logs are saved to a file instead")
    .flag(default = false)

  private val mapper by di.instance<ObjectMapper>()
  private val props by di.instance<Properties>()
  private val db by di.instance<StockDatabase>()
  private val indicatorCache by di.instance<IndicatorCache>()

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

    // Disable logging if requested
    if(noLog) {
      val logger4j = org.apache.log4j.Logger.getRootLogger()
      logger4j.level = Level.toLevel("ERROR")
      logger4j.removeAllAppenders()
      logger4j.addAppender(FileAppender(PatternLayout(), "kt-data-loader.log"))
    }

    val queryConfig = loadQueryConfig()
    log.debug("Running With Config $queryConfigFile:\n$queryConfig")

    // Create query plan
    val fetchPlans = getScheduler().createFetchPlanONE(queryConfig.queries.asSequence(), batchSize=batchSize)

    // If `--plan` flag is present, print plan and exits
    if(plan) {
      fetchPlans.forEach { log.info(it.toString()) }
      exitProcess(0)
    }

    // Execute queries
    fetchPlans.forEach { fetchPlan ->
      executor.execute {
        // Save some work by checking the DB for pre-existing samples
        val cachedResults = indicatorCache.fetchAllIfPossible(fetchPlan)
        processSamples(cachedResults.first, insertInDB=false)
        val simplifiedFetchPlan = cachedResults.second
        // Log cache hits
        val cacheCount = fetchPlan.argsList.size - simplifiedFetchPlan.argsList.size
        if(cacheCount > 0) {
          log.info("Fetched $cacheCount/${fetchPlan.argsList.size} samples from cache")
        }

        // Create data source command string
        val commandTemplate = getIndicatorConfig().getFetchCommandTemplate(simplifiedFetchPlan.indicatorID)
        val processBuilder = ProcessBuilder(simplifiedFetchPlan.getCommand(commandTemplate))
          .directory(Paths.get("..").toFile())
          .redirectOutput(ProcessBuilder.Redirect.PIPE)
          .redirectError(ProcessBuilder.Redirect.PIPE)

        log.info("Starting Process ${processBuilder.command()}")
        // Execute data source
        val proc = processBuilder.start()
        // Process stdout lines as JSON
        proc.inputStream.bufferedReader().lines().forEach { output ->
          val outputTeaser = output.substring(0, min(output.length, 250)) + " ..."
          log.info("Received output from ${processBuilder.command()}: $outputTeaser")
          log.debug("Full output from ${processBuilder.command()}: $output")
          processOutputLine(simplifiedFetchPlan.indicatorID, output)
        }
        // Print stderr lines as warnings
        proc.errorStream.bufferedReader().lines().forEach { err ->
          log.warn("from stderr of data source process ${processBuilder.command()}: $err")
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

      // Process samples
      processSamples(samples)

      return samples
    } catch(ex: Exception) {
      log.warn("Couldn't process output line of $indicatorID: $line\n$ex")
      throw ex
    }
  }

  private fun processSamples(samples: List<IndicatorSample>, insertInDB: Boolean=true) {
    log.info("Inserting ${samples.size} samples: $samples")
    if(noLog) {
      val sampleStr = mapper.writeValueAsString(samples)
      println(sampleStr)
    }

    if(insertInDB) {
      samples.forEach { sample ->
        try {
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
        } catch (ex: Exception) {
          if (noLog) {
            ex.printStackTrace(System.err)
          } else {
            log.error(ex.toString())
          }
        }
      }
    }
  }

  private fun createExecutorService(): ExecutorService {
    return ThreadPoolExecutor(1, concurrentQueries, 30L, TimeUnit.MINUTES, LinkedBlockingQueue())
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
