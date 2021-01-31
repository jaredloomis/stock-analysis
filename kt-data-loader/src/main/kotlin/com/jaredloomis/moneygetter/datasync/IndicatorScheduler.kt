package com.jaredloomis.moneygetter.datasync

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.lang.Exception
import java.time.Duration
import java.time.Instant
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.streams.asStream

data class IndicatorSampleFetch(
  val indicatorID: String, val args: Map<String, Any>
) {
  fun getTickers(): List<String> {
    val tickersRaw = args["tickers"]
    return if(tickersRaw is String) {
      listOf(tickersRaw)
    } else if(tickersRaw is ArrayList<*>) {
      tickersRaw as List<String>
    } else {
      emptyList()
    }
  }

  fun getTime(): Instant? {
    val t = args["time"]
    return when(t) {
      is Instant -> t
      is String  -> Instant.parse(t)
      else       -> null
    }
  }
}

data class IndicatorSampleFetchBatch(
  val indicatorID: String, val argsList: List<Map<String, Any>>
) {
  val args = argsList.fold(emptyMap<String, Any>()) { acc, argSet -> acc.plus(argSet) }

  fun getCommand(template: String): List<String> {
    val cmdWithArgs = args.entries.fold(template) { acc, arg ->
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
      IndicatorSampleFetchBatch(indicatorID, argsList.filterIndexed { i, _ -> i != index })
    } else {
      this
    }
  }

  override fun toString(): String {
    return "IndicatorSampleFetchBatch(indicatorID='$indicatorID', argsList=$argsList, args=$args)"
  }
}

data class IndicatorScheduler(val groups: List<IndicatorGroupSpec>) {
  private val logger: Logger = LoggerFactory.getLogger(javaClass)

  fun createFetchPlan(queries: Sequence<IndicatorQuery>, batchSize: Int=Int.MAX_VALUE): List<IndicatorSampleFetchBatch> {
    val sampleFetches = queries.asStream()
      // For each query, create enough samples to cover the requested timerange
      .flatMap { query ->
        query.getSchedule().ranges().stream()
          .flatMap { timerange ->
            val startTime = timerange.first.toInstant()
            val endTime   = timerange.second.toInstant()
            val duration  = Duration.between(startTime, endTime)
            val sampleRate = query.getSampleRate()
            val sampleCount = Math.max(1, duration.toMillis() / sampleRate.toMillis())

            Stream.iterate(0L, { i -> i+1 }).limit(sampleCount)
              .map { sampleI ->
                val sampleTime = startTime.plus(sampleRate.multipliedBy(sampleI))
                Pair(query, sampleTime)
              }
          }
      }
      // Construct IndicatorSampleFetch for each ticker requested
      .flatMap { queryInstant ->
        queryInstant.first.tickers.stream()
          .map { ticker ->
            val args = queryInstant.first.arguments
              .plus(Pair("time", queryInstant.second))
              .plus(Pair("ticker", ticker))
            IndicatorSampleFetch(queryInstant.first.indicatorID, args)
          }
      }
      .collect(Collectors.toList())

    // Create fetch batches
    return batchFetches(sampleFetches, batchSize)
  }

  private fun batchFetches(plan: List<IndicatorSampleFetch>, batchSize: Int): List<IndicatorSampleFetchBatch> {
    return plan
      // Group by indicatorID and args
      // ie. fetches are grouped together IFF their indicator and all args (except tickers) are equal
      .groupBy { Pair(it.indicatorID, it.args.minus("tickers")) }
      // Break up large groups into batches
      .flatMap { group ->
        if(group.value.size > batchSize) {
          group.value.chunked(batchSize)
        } else {
          listOf(group.value)
        }
      }
      // Remove empty batches, if any
      .filter { it.isNotEmpty() }
      // For each batch, construct an IndicatorSampleFetchBatch
      .map { batchFetches ->
        IndicatorSampleFetchBatch(batchFetches[0].indicatorID, batchFetches.map { it.args })
      }
  }

  private fun getAvailableAt(indicatorID: String, timestamp: Instant): List<IndicatorSpec> {
    logger.info(groups.toString())
    return flattenList(groups)
      .filter { it.provides(indicatorID) }
      .map { logger.debug(it.toString()); it }
      .filter { it.schedule.isAvailableAt(IndicatorTime.Timestamp(timestamp)) }
  }

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