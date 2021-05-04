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
import java.util.concurrent.atomic.AtomicLong
import java.util.stream.Collectors

fun main(args: Array<String>) {
  CLIRunner().main(args)
}

class CLIRunner : CliktCommand() {
  private val log = LoggerFactory.getLogger(javaClass)

  private val queryConfigFile: File by option(help = "Path to query config file")
    .file(mustBeReadable = true)
    .required()
  private val indicatorConfigFile: File by option(help = "Path to global indicator config file")
    .file(mustBeReadable = true)
    .default(
      Paths.get(javaClass.protectionDomain.codeSource.location.toURI())
           .parent
           .resolve("..").resolve("..").resolve("..")
           .resolve("config").resolve("indicators.json")
           .toFile()
      )
    //.default(File("../config/indicators.json"))
  private val batchSize: Int by option(help = "# of tickers per data source query")
    .int()
    .default(20)
  private val concurrentQueries: Int by option(help = "# of data source queries to execute at the same time")
    .int()
    .default(5)
  private val dataSourceRetries: Int by option(help = "# of retries per data source")
    .int()
    .default(3)
  private val plan: Boolean by option(help = "Don't fetch any data - just print out planned queries")
    .flag(default = false)
  private val noCacheCheck: Boolean by option(help = "Don't check the db for samples before fetching")
    .flag(default = false)
  private val logLevel: String by option(help = "Default: INFO\nOFF - Don't print out logs; just return the result. Logs are saved to a file instead.\nERROR - error\nINFO - info\nDEBUG - debug\nTRACE - trace")
    .default("INFO")
    .check("must be one of: OFF, TRACE, DEBUG, INFO, ERROR") { it == "OFF" || it == "ERROR" || it == "INFO" || it == "DEBUG" || it == "TRACE" }

  private val mapper by di.instance<ObjectMapper>()
  private val db by di.instance<StockDatabase>()
  private val indicatorCache by di.instance<IndicatorCache>()

  private val insertedSamplesCount: AtomicLong = AtomicLong(0)

  private val noLog: Boolean
    get() = logLevel == "OFF"

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

    // Create fetch batches
    val fetchBatches = createFetchBatches(queryConfig)

    // If `--plan` flag is present, print plan and exit
    if(plan) {
      fetchBatches.forEach { log.info(it.toString()) }
      exitProcess(0)
    }

    // Execute queries
    fetchBatches.forEach { fetchBatch ->
      executor.execute {
        log.info("Fetching batch $fetchBatch")
        // Using a data source... (retries / uses a backup data source if an exception occurs)
        fetchBatch.tryWithDataSource(tries = dataSourceRetries + 1) { dataSource ->
          var sampleCount = 0

          // Create data source command string
          val processBuilder = ProcessBuilder(fetchBatch.getCommand(dataSource))
            .directory(Paths.get("..").toFile())
            .redirectOutput(ProcessBuilder.Redirect.PIPE)
            .redirectError(ProcessBuilder.Redirect.PIPE)

          log.info("Using data source ${dataSource.dataSourceID}")
          log.info("Starting Process ${processBuilder.command()}")

          // Execute data source
          val proc = processBuilder.start()
          // Process stdout lines as JSON
          proc.inputStream.bufferedReader().lines().forEach { output ->
            val outputTeaser = output.substring(0, min(output.length, 250)) + " ..."
            log.info("Received output from ${processBuilder.command()}: $outputTeaser")
            log.debug("Full output from ${processBuilder.command()}: $output")
            val samples = processOutputLine(dataSource, output)
            sampleCount += samples.size
          }
          // Print stderr lines as warnings
          proc.errorStream.bufferedReader().lines().forEach { err ->
            log.warn("from stderr of data source process ${processBuilder.command()}: $err")
          }
          // Wait for process to complete
          proc.onExit().join()

          // If no samples were collected, try next data source
          if(sampleCount == 0) {
            log.info("No samples provided from ${dataSource.dataSourceID}; trying next data source (if any)")
            throw Exception()
          }
        }
      }
    }

    executor.shutdown()
    executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)

    log.info("SUMMARY: Successfully inserted ${insertedSamplesCount.get()} samples")

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
    if(samples.isEmpty()) {
      return
    }

    log.info("Processing ${samples.size} samples")
    log.debug("$samples")

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

          insertedSamplesCount.addAndGet(1)
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

  private fun createFetchBatches(queryConfig: IndicatorQueryConfig): List<IndicatorSampleFetchBatch> {
    // Create raw fetch plan
    val (plannedFetchesStrm, abortedFetches) = getFetchPlanner().createFetchPlan(queryConfig.queries.asSequence())
    // XXX: Hard limit of 100,000 samples per execution. TODO split large requests into multiple requests
    val plannedFetches = plannedFetchesStrm.limit(100000).collect(Collectors.toList())
    logger.info("Created raw fetch plan of ${plannedFetches.size} samples")
    logger.debug("First 10 samples: ${plannedFetches.take(10)}")
    // Log aborted fetches
    if(abortedFetches.isNotEmpty()) {
      log.error("${abortedFetches.size}/${plannedFetches.size+abortedFetches.size} fetches aborted.")
      log.trace("Aborted fetches: $abortedFetches")
    }

    // Check cache for existing samples, update fetch plan
    val uncachedFetches = if(!noCacheCheck) {
      plannedFetches.filter { fetch ->
        val cachedSamples = indicatorCache.fetchIfPossible(fetch)
        processSamples(cachedSamples, insertInDB = false)
        cachedSamples.isEmpty()
      }
    } else {
      plannedFetches
    }
    // Log cache hits
    val cacheCount = plannedFetches.size - uncachedFetches.size
    if (cacheCount != 0) {
      log.info("Fetched $cacheCount/${plannedFetches.size} samples from cache")
    }

    // Log fetches still needed
    log.info("Planning ${uncachedFetches.size} fetches")

    // Create fetch batches to improve performance
    val fetchBatches = getFetchPlanner().batchFetches(uncachedFetches, batchSize)
    log.info("Planning ${fetchBatches.size} fetch batches")

    return fetchBatches
  }

  private fun createExecutorService(): ExecutorService {
    return Executors.newWorkStealingPool(concurrentQueries)
    //return ThreadPoolExecutor(1, concurrentQueries, 30L, TimeUnit.MINUTES, LinkedBlockingQueue())
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
    val indicatorsMap = indicatorConfigFile.toPath() //Paths.get("..").resolve(props["indicators.map.path"] as String)
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
    loggerList.stream().forEach { logger ->
      logger.level = level
    }
  }
}
