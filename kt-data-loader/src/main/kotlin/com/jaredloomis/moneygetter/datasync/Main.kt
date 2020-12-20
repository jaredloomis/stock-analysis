package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import org.kodein.di.instance
import org.slf4j.LoggerFactory
import java.io.File
import java.lang.StringBuilder
import java.nio.file.Files
import java.nio.file.Paths
import java.util.*
import java.util.concurrent.*

fun main(args: Array<String>) {
  CLIRunner().main(args)
}

class CLIRunner : CliktCommand() {
  private val log = LoggerFactory.getLogger(javaClass)

  private val configFile: File by option(help = "Path to config file")
    .file(mustBeReadable = true)
    .required()
  private val batchSize: Int? by option(help = "# of tickers per data source query")
    .int()
  private val concurrentQueries: Int by option(help = "# of data source queries to execute at the same time")
    .int()
    .default(3)

  private val mapper by di.instance<ObjectMapper>()
  private val props by di.instance<Properties>()
  private val db by di.instance<StockDatabase>()

  // NOTE: Must be updated every time a new indicator class is added
  private val INDICATOR_MODEL_CLASSES = emptyMap<String, Class<IsIndicatorSample>>()
    .plus(Pair(PriceSample.INDICATOR_ID, PriceSample::class.java))
    .plus(Pair(SentimentSample.INDICATOR_ID, SentimentSample::class.java))

  override fun run() {
    val executor = createExecutorService()

    // Kill child processes if this process is killed (?)
    Runtime.getRuntime().addShutdownHook(Thread {
      executor.shutdownNow()
      executor.awaitTermination(10, TimeUnit.SECONDS)
    })

    val config = loadQueryConfig()
    log.debug("Running With Config $configFile:\n$config")

    // Execute queries
    config.queries.forEach { indicatorQuery ->
      // Create ProcessBuilders for all Data Source queries
      val dataSourceProcessBuilders = indicatorQuery
        .batches(batchSize ?: 0)
        .map { getDataSource(it)!! }

      // And add them to the executor queue
      dataSourceProcessBuilders.forEach { processBuilder ->
        executor.execute {
          log.debug("Starting Process ${processBuilder.command()}")
          val proc = processBuilder.start()
          proc.inputStream.bufferedReader().lines().forEach { output ->
            log.debug("Output: $output")
            processOutputLine(indicatorQuery.indicatorID, output)
          }
          proc.onExit().join()
        }
      }
    }

    executor.shutdown()
    executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)
    log.info("Successfully completed.")
  }

  private fun processOutputLine(indicatorID: String, line: String): List<IndicatorSample> {
    // Read to JSON
    val json = mapper.readTree(line)
    val errors = LinkedList<String>()
    val rawSamples = json.elements().asSequence().mapNotNull {
      if (it.isTextual) {
        errors.push(it.asText())
        null
      } else {
        mapper.treeToValue(it, INDICATOR_MODEL_CLASSES[indicatorID])
      }
    }.toList()

    // Create an indicator sample
    val samples = rawSamples.map { it.asSample() }

    log.debug("Samples: $samples")

    // Insert sample into the DB
    samples.forEach { sample ->
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
  }

  private fun createExecutorService(): ExecutorService {
    return ThreadPoolExecutor(0, concurrentQueries, 30L, TimeUnit.MINUTES, LinkedBlockingQueue<Runnable>())
  }

  private fun loadQueryConfig(): IndicatorQueryConfig {
    return mapper.readValue(configFile, IndicatorQueryConfig::class.java)
  }

  private fun getDataSource(query: IndicatorQuery): ProcessBuilder? {
    // Get command template from config file
    val indicatorsMap = Paths.get("..").resolve(props["indicators.map.path"] as String)
    val indicatorsJson = Files.readString(indicatorsMap)
    // Create command string from template and query
    val cmd = IndicatorConfig(indicatorsJson).getFetchCommand(query)

    log.debug("Data Source query command: $cmd")

    return ProcessBuilder(cmd.split(" "))
      .directory(Paths.get("..").toFile())
      .redirectOutput(ProcessBuilder.Redirect.PIPE)
      .redirectError(ProcessBuilder.Redirect.to(Paths.get("error.log").toFile()))
  }
}
