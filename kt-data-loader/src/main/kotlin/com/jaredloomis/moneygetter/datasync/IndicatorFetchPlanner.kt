package com.jaredloomis.moneygetter.datasync

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.time.Duration
import java.time.Instant
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.asStream

data class IndicatorSampleFetch(
  val indicatorID: String, val args: Map<String, Any>
) {
  fun getTickers(): List<String>? {
    val tickersRaw = args["tickers"]
    return tickersRaw as List<String>
    return if(tickersRaw is List<*>) {
      tickersRaw as List<String>
    } else {
      null
    }
  }

  fun getTime(): IndicatorTime? {
    return when(val t = args["time"]) {
      is IndicatorTime -> t
      //is Instant       -> t
      //is String        -> Instant.parse(t)
      null -> null
      else             -> {
        logger.error("Time was of type ${t.javaClass}: ${t.toString()}")
        null
      }
    }
  }
}

data class IndicatorSampleFetchBatch(
  val dataSource: DataSourceSpec, val argsList: List<Map<String, Any>>
) {
  val args = argsList.fold(emptyMap<String, Any>()) { acc, argSet -> acc.plus(argSet) }

  fun getTime(): IndicatorTime? {
    println(argsList)
    val timestamp = args.getOrDefault("time", null) as Instant?
    return timestamp?.let { IndicatorTime.Timestamp(it) }
  }

  fun getCommand(): List<String> {
    val cmdWithArgs = args.entries.fold(dataSource.commandTemplate) { acc, arg ->
      if(arg.key == "tickers") {
        acc.replace("\$${arg.key}", showTickers(arg.value as List<String>))
      } else {
        acc.replace("\$${arg.key}", arg.value.toString())
      }
    }

    return cmdWithArgs.split(" ")
  }

  fun withoutIndex(index: Int): IndicatorSampleFetchBatch {
    return if(index < argsList.size) {
      IndicatorSampleFetchBatch(dataSource, argsList.filterIndexed { i, _ -> i != index })
    } else {
      this
    }
  }

  override fun toString(): String {
    return "IndicatorSampleFetchBatch(dataSource=$dataSource, args=$args)"
  }
}

data class IndicatorFetchPlanner(val groups: List<DataSourceGroupSpec>) {
  private val logger: Logger = LoggerFactory.getLogger(javaClass)

  fun createFetchPlan(queries: Sequence<IndicatorQuery>, batchSize: Int=Int.MAX_VALUE): Pair<List<IndicatorSampleFetchBatch>, List<IndicatorSampleFetch>> {
    val abortedFetches: MutableList<IndicatorSampleFetch> = ArrayList()

    val sampleFetches = queries.asStream()
      // For each query, create enough samples to cover the requested timeranges
      .flatMap { query ->
        query.getSchedule().ranges().stream()
          .flatMap { timerange ->
            val startTime = timerange.first
            val endTime   = timerange.second
            val duration  = Duration.between(startTime.toInstant(), endTime.toInstant())
            val sampleRate = query.getSampleRate()
            val sampleCount = Math.max(1, duration.toMillis() / sampleRate.toMillis())

            if(duration.toMillis() < 1000) {
              Stream.of(Pair(query, endTime))
            } else {
              Stream.iterate(0L, { i -> i + 1 }).limit(sampleCount)
                .map { sampleI ->
                  val sampleTime = startTime.plus(sampleRate.multipliedBy(sampleI))
                  Pair(query, sampleTime)
                }
            }
          }
      }
      // Construct IndicatorSampleFetch for each ticker requested, for every sample time
      .flatMap { queryInstant ->
        queryInstant.first.tickers.stream()
          .map { ticker ->
            val args = queryInstant.first.arguments
              .plus(Pair("time", queryInstant.second))
              .plus(Pair("tickers", listOf(ticker)))
            IndicatorSampleFetch(queryInstant.first.indicatorID, args)
          }
      }
      .filter { it.getTickers()?.isNotEmpty() ?: false }
      // Filter out fetches that are not possible (failed fetches)
      .filter { sampleFetch ->
        val isNull = assignDataSource(sampleFetch.indicatorID, sampleFetch.getTime()!!) == null
        if(isNull) {
          abortedFetches.add(sampleFetch)
        }
        !isNull
      }
      .collect(Collectors.toList())

    // Group together similar samples into batches
    return Pair(batchFetches(sampleFetches, batchSize), abortedFetches)
  }

  private fun batchFetches(plan: List<IndicatorSampleFetch>, batchSize: Int): List<IndicatorSampleFetchBatch> {
    return plan.stream()
      // Group into batches by data source
      // XXX should we group by datasource+args?
      .reduce(
        emptyMap<DataSourceSpec, List<IndicatorSampleFetch>>(),
        { dataSourceSampleMap, fetch ->
          val dataSource = assignDataSource(fetch.indicatorID, fetch.getTime()!!)!!
          if(dataSourceSampleMap.containsKey(dataSource)) {
            dataSourceSampleMap.plus(
              Pair(dataSource, dataSourceSampleMap[dataSource]!!.plus(fetch))
            )
          } else {
            dataSourceSampleMap.plus(
              Pair(dataSource, listOf(fetch))
            )
          }
        },
        { a, b -> a.plus(b) }
      )
      // Break up large batches according to batchSize
      .flatMap { batch ->
        val dataSourceSpec = batch.key
        val samples = batch.value

        if(samples.size > batchSize) {
          samples.chunked(batchSize).map { Pair(dataSourceSpec, it) }
        } else {
          listOf(Pair(dataSourceSpec, samples))
        }
      }
      // Remove empty batches, if any
      .filter { it.second.isNotEmpty() }
      // For each batch, construct an IndicatorSampleFetchBatch
      .map { (dataSource, fetches) ->
        IndicatorSampleFetchBatch(dataSource, fetches.map { it.args })
      }
  }

  private fun assignDataSource(indicatorID: String, time: IndicatorTime): DataSourceSpec? {
    logger.debug("Data Source Groups: ${groups.toString()}")
    return flattenList(groups)
      .filter { it.provides(indicatorID) }
      .map { logger.debug(it.toString()); it }
      .filter { it.schedule.isAvailableAt(time) }
      .getOrNull(0)
  }

  /*
  private fun assignDataSource(indicatorID: String, timestamp: Instant): DataSourceSpec? {
    logger.debug("Data Source Groups: ${groups.toString()}")
    return flattenList(groups)
      .filter { it.provides(indicatorID) }
      .map { logger.debug(it.toString()); it }
      .filter { it.schedule.isAvailableAt(IndicatorTime.Timestamp(timestamp)) }
      .getOrNull(0)
  }
   */

  private fun <A> flattenList(xss: List<List<A>>): List<A> {
    val ret = ArrayList<A>()
    for(xs in xss) {
      for(x in xs) {
        ret.add(x)
      }
    }
    return ret
  }
}