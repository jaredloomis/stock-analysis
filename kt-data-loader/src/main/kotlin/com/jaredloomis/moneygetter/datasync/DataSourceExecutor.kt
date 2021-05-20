package com.jaredloomis.moneygetter.datasync

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import com.fasterxml.jackson.databind.ObjectMapper
import com.jaredloomis.moneygetter.datasync.DataSourceExecutorResponse.Companion.default
import org.kodein.di.instance
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.nio.file.Paths
import java.util.*
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import java.util.stream.Stream
import kotlin.math.min
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean


data class DataSourceExecutorRequest(
  val dataSourceConfig: DataSourceConfig,
  val queryConfig: IndicatorQueryConfig,
  val noCacheCheck: Boolean
)

data class DataSourceExecutorResponse(
  var error: String?,
  var abortedSamples: Long,
  var processedSamples: Long,
  var insertedSamples: Long,
) {
  companion object {
    fun default(): DataSourceExecutorResponse {
      return DataSourceExecutorResponse(null, 0, 0, 0)
    }
  }
}

class DataSourceExecutor(
  logLevel: String
) {
  var runInProgress = AtomicBoolean(false)

  private val log = createLogger(logLevel)
  private val mapper by di.instance<ObjectMapper>()
  private val db by di.instance<StockDatabase>()
  private val indicatorCache by di.instance<IndicatorCache>()
  // NOTE: Must be updated every time a new indicator class is added
  private val INDICATOR_MODEL_CLASSES = emptyMap<String, Class<IsIndicatorSample>>()
    .plus(Pair(PriceSample.INDICATOR_ID, PriceSample::class.java))
    .plus(Pair(SentimentSample.INDICATOR_ID, SentimentSample::class.java))

  private var executorService: ExecutorService? = null
  private var fetchPlanner: IndicatorFetchPlanner? = null
  private val cachedSamples: MutableList<IndicatorSample> = ArrayList()
  private val response: AtomicReference<DataSourceExecutorResponse> = AtomicReference(
    default()
  )
  private var request: DataSourceExecutorRequest? = null

  fun run(req: DataSourceExecutorRequest, sampleHandler: (List<IndicatorSample>) -> Unit): DataSourceExecutorResponse {
    // Initialize state
    runInProgress.set(true)
    request = req
    response.set(default())
    fetchPlanner = IndicatorFetchPlanner(req.dataSourceConfig.getGroups())
    executorService = getExecutorService(req.dataSourceConfig)
    cachedSamples.clear()

    log.debug("Executing Query")
    log.debug("Query Config: ${req.queryConfig}")
    log.debug("Data Source Config: ${req.dataSourceConfig}")

    // Create fetch batches
    val (fetchBatches, finalBatches) = createFetchBatches()

    // Execute queries
    fetchBatches.forEach   { executeFetchBatch(it, sampleHandler) }
    finalBatches().forEach { executeFetchBatch(it, sampleHandler) }

    // Process cached samples
    processSamples(cachedSamples, insertInDB = false)
    sampleHandler(cachedSamples)

    val res = response.get()
    log.info("SUMMARY: Successfully inserted ${res.insertedSamples} samples out of ${res.processedSamples} processed (${res.abortedSamples} aborted)")

    if(!executorService!!.isShutdown) {
      executorService!!.shutdown()
      executorService!!.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)
    }

    runInProgress.set(false)
    return response.get()
  }

  private fun executeFetchBatch(fetchBatch: IndicatorSampleFetchBatch, sampleHandler: (List<IndicatorSample>) -> Unit) {
    executorService!!.execute {
      log.info("Fetching batch $fetchBatch")
      // Using a data source... (retries / uses a backup data source if an exception occurs)
      fetchBatch.tryWithDataSource { dataSource ->
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
          sampleHandler(samples)
        }
        // Print stderr lines as warnings
        proc.errorStream.bufferedReader().lines().forEach { err ->
          log.warn("from stderr of data source process ${processBuilder.command()}: $err")
        }
        // Wait for process to complete
        proc.onExit().join()

        // If no samples were collected, try next data source
        if (sampleCount == 0) {
          log.info("No samples provided from ${dataSource.dataSourceID}; trying next data source (if any)")
          throw Exception()
        }
      }
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

    response.updateAndGet { it.processedSamples + samples.size; it }

    log.info("Processing ${samples.size} samples")
    log.debug("$samples")

    if(insertInDB) {
      samples.stream().forEach { sample ->
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

          response.updateAndGet { ++it.insertedSamples; it }
        } catch (ex: Exception) {
          log.error(ex.toString())
        }
      }
    }
  }

  private fun createFetchBatches(): Pair<Stream<IndicatorSampleFetchBatch>, () -> List<IndicatorSampleFetchBatch>> {
    // Create raw fetch plan
    val (plannedFetches, abortedFetches) = fetchPlanner!!.createFetchPlan(request!!.queryConfig.queries.asSequence())

    // Log aborted fetches
    if(abortedFetches.isNotEmpty()) {
      response.updateAndGet { it.abortedSamples = abortedFetches.size.toLong(); it }
      log.error("${abortedFetches.size} fetches aborted.")
      log.trace("Aborted fetches: $abortedFetches")
    }

    val uncachedFetches = if(!request!!.noCacheCheck) {
      plannedFetches.filter { fetch ->
        val cached = indicatorCache.fetchIfPossible(fetch)
        cachedSamples.addAll(cached)
        cached.isEmpty()
      }
    } else {
      plannedFetches
    }

    return fetchPlanner!!.streamingBatch(uncachedFetches)
  }

  var lastDataSourceConfig: DataSourceConfig? = null

  private fun getExecutorService(dataSourceConfig: DataSourceConfig): ExecutorService {
    if(executorService == null || executorService!!.isShutdown) {
      executorService = createExecutorService(dataSourceConfig)
    } else if(lastDataSourceConfig?.getConcurrentQueries() == dataSourceConfig.getConcurrentQueries()) {
      executorService!!.shutdown()
      executorService!!.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS)
      executorService = createExecutorService(dataSourceConfig)
    }
    lastDataSourceConfig = dataSourceConfig
    return executorService!!
  }

  private fun createExecutorService(dataSourceConfig: DataSourceConfig): ExecutorService {
    val executor = Executors.newWorkStealingPool(dataSourceConfig.getConcurrentQueries() ?: 5)
    // Kill child processes if this process is killed (?)
    Runtime.getRuntime().addShutdownHook(Thread {
      executor.shutdownNow()
      executor.awaitTermination(10, TimeUnit.SECONDS)
    })
    return executor
  }

  private fun createLogger(logLevel: String): Logger {
    val log = LoggerFactory.getLogger(javaClass)
    val level = Level.toLevel(logLevel.toUpperCase())
    val loggerContext = LoggerFactory.getILoggerFactory() as LoggerContext
    val loggerList = loggerContext.loggerList
    loggerList.stream().forEach { logger ->
      logger.level = level
    }
    return log
  }
}
