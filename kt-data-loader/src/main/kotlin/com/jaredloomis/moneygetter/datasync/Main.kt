package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import org.kodein.di.instance
import org.slf4j.LoggerFactory
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util.*
import java.util.concurrent.*
import kotlin.math.min
import kotlin.system.exitProcess
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import java.util.function.Consumer

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
    .default(20)
  private val concurrentQueries: Int by option(help = "# of data source queries to execute at the same time")
    .int()
    .default(3)
  private val plan: Boolean by option(help = "Don't fetch any data - just print out planned queries")
    .flag(default = false)
  private val logLevel: String by option(help = "NONE - Don't print out logs - just return the result. Logs are saved to a file instead\nERROR - error\nINFO - info\nDEBUG - debug")
    .default("INFO")
    .check("must be one of: NONE, DEBUG, INFO, ERROR") { it == "NONE" || it == "DEBUG" || it == "INFO" || it == "ERROR" }

  private val mapper by di.instance<ObjectMapper>()
  private val props by di.instance<Properties>()
  private val db by di.instance<StockDatabase>()
  private val indicatorCache by di.instance<IndicatorCache>()

  private val noLog: Boolean
    get() = logLevel == "NONE"

  // NOTE: Must be updated every time a new indicator class is added
  private val INDICATOR_MODEL_CLASSES = emptyMap<String, Class<IsIndicatorSample>>()
    .plus(Pair(PriceSample.INDICATOR_ID, PriceSample::class.java))
    .plus(Pair(SentimentSample.INDICATOR_ID, SentimentSample::class.java))

  private var dataSourceConfigCache: DataSourceConfig? = null

  override fun run() {
    val executor = createExecutorService()

    // Kill child processes if this process is killed (?)
    Runtime.getRuntime().addShutdownHook(Thread {
      executor.shutdownNow()
      executor.awaitTermination(10, TimeUnit.SECONDS)
    })

    // Configure logging
    configureLogging()

    val queryConfig = loadQueryConfig()
    log.debug("Running With Config $queryConfigFile:\n$queryConfig")

    // Create fetch plan
    val (fetchPlans, abortedFetches) = getFetchPlanner().createFetchPlan(queryConfig.queries.asSequence(), batchSize=batchSize)
    log.info("Planning ${fetchPlans.size} fetches. ${abortedFetches.size} fetches aborted.")
    log.debug("Aborted fetches: $abortedFetches")

    // If `--plan` flag is present, print plan and exits
    if(plan) {
      fetchPlans.forEach { log.info(it.toString()) }
      exitProcess(0)
    }

    // Execute queries
    fetchPlans.forEach { fetchBatch ->
      executor.execute {
        log.info("Fetching batch $fetchBatch")
        // Save some work by checking the DB for pre-existing samples
        val cachedResults = indicatorCache.fetchAllIfPossible(fetchBatch)
        processSamples(cachedResults.first, insertInDB=false)
        val simplifiedFetchBatch = cachedResults.second
        // Log cache hits
        val cacheCount = fetchBatch.argsList.size - simplifiedFetchBatch.argsList.size
        if(cacheCount != 0) {
          log.info("Fetched $cacheCount/${fetchBatch.argsList.size} samples from cache")
        }

        // Create data source command string
        val processBuilder = ProcessBuilder(simplifiedFetchBatch.getCommand())
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
          processOutputLine(simplifiedFetchBatch.dataSource, output)
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

  private fun processOutputLine(dataSource: DataSourceSpec, line: String): List<IndicatorSample> {
    try {
      // Read to JSON
      val json = mapper.readTree(line)
      val errors = LinkedList<String>()
      val rawSamples = json.elements().asSequence().mapNotNull {
        if (it.isTextual) {
          errors.push(it.asText())
          null
        } else {
          logger.debug("Output from $dataSource:\n$line")
          mapper.treeToValue(it, INDICATOR_MODEL_CLASSES[dataSource.indicatorID.split('-')[0]])
        }
      }.toList()

      // Create an indicator sample
      val samples = rawSamples.map { it.asSample() }

      // Process samples
      processSamples(samples)

      return samples
    } catch(ex: Exception) {
      log.warn("Couldn't process output line of $dataSource: $line\n$ex")
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

  private fun getFetchPlanner(): IndicatorFetchPlanner {
    return IndicatorFetchPlanner(getIndicatorConfig().getGroups())
  }

  private fun getIndicatorConfig(): DataSourceConfig {
    if(dataSourceConfigCache != null) {
      return dataSourceConfigCache!!
    }

    // Get command template from config file
    val indicatorsMap = Paths.get("..").resolve(props["indicators.map.path"] as String)
    val indicatorsJson = Files.readString(indicatorsMap)
    // Create command string from template and query
    val conf = DataSourceConfig(indicatorsJson)
    dataSourceConfigCache = conf
    return conf
  }

  private fun configureLogging() {
    val level = Level.toLevel(logLevel.toUpperCase())
    val loggerContext = LoggerFactory.getILoggerFactory() as LoggerContext
    val loggerList = loggerContext.loggerList
    loggerList.stream().forEach(Consumer<ch.qos.logback.classic.Logger> { tmpLogger: ch.qos.logback.classic.Logger -> tmpLogger.setLevel(level) })
    /*
    val logger4j = org.apache.log4j.Logger.getRootLogger()
    logger4j.level = Level.toLevel(logLevel)
    if (noLog) {
      logger4j.level = Level.toLevel(logLevel)
      // Disable console logging if requested (log to a file in cwd)
      logger4j.removeAllAppenders()
      logger4j.addAppender(FileAppender(PatternLayout(), "kt-data-loader.log"))
    }
     */
  }
}
