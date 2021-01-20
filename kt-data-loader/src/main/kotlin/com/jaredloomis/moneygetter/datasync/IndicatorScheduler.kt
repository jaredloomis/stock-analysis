package com.jaredloomis.moneygetter.datasync

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.time.Duration
import java.time.Instant
import java.util.stream.Collectors
import java.util.stream.Stream
import kotlin.math.ceil
import kotlin.streams.asStream
import kotlin.streams.toList

/**
 * A plan to fetch a sample or set of samples
 */
data class IndicatorSampleFetchPlan(val query: IndicatorQuery, val indicator: IndicatorSpec, val additionalArgs: Map<String, String>) {
  fun getCommand(): List<String> {
    val cmdWithArgs = query.arguments.entries.fold(getCommandTemplate()) { acc, arg ->
      if(arg.key == "tickers") {
        acc.replace("\$${arg.key}", showTickers(arg.value as List<String>))
      } else {
        acc.replace("\$${arg.key}", arg.value.toString())
      }
    }

    val cmdWithTime = cmdWithArgs.replace("\$time", getTimeStr())

    return cmdWithTime.split(" ")
  }

  private fun getTimeStr(): String {
    return additionalArgs.getOrDefault("time", query.arguments["time"]) as String
  }

  private fun getCommandTemplate(): String {
    return indicator.getCommandFor(query)
  }

  override fun toString(): String {
    return "IndicatorSampleFetchPlan(query=$query, indicator=$indicator, additionalArgs=$additionalArgs)"
  }
}

data class IndicatorSampleFetchPlanONE(
  val indicatorID: String, val stockID: String, val time: Instant?, val args: Map<String, String>
) {

}

data class IndicatorScheduler(val groups: List<IndicatorGroupSpec>) {
  private val logger: Logger = LoggerFactory.getLogger(javaClass)

  fun createFetchPlan(queries: Sequence<IndicatorQuery>, batchSize: Int=Int.MAX_VALUE): List<IndicatorSampleFetchPlan> {
    return queries.asStream()
      .flatMap { query ->
        // Split samples apart according to batchSize
        if(query.tickers.size > batchSize) {
          query.tickers.chunked(batchSize)
            .stream()
            .map { query.withTickers(it) }
        } else {
          Stream.of(query)
        }
      }
      .flatMap { query ->
        // For each query, create enough samples to cover the requested timerange
        query.getSchedule().ranges().stream()
          .flatMap { timerange ->
            val startTime = timerange.first.toInstant()
            val endTime   = timerange.second.toInstant()
            val duration  = Duration.between(startTime, endTime)
            val sampleRate = query.getSampleRate()
            val sampleCount = duration.toMillis() / sampleRate.toMillis()

            Stream.iterate(0L, { i -> i+1 }).limit(sampleCount)
              .map { sampleI ->
                val sampleTime = startTime.plus(sampleRate.multipliedBy(sampleI))
                Pair(query, sampleTime)
              }
          }
      }
      .map { queryInstant ->
        val indicators = getAvailableAt(queryInstant.first.indicatorID, queryInstant.second)
        if(indicators.isEmpty()) {
          logger.error("No data sources available to fetch sample: $queryInstant")
          null
        } else {
          val indicator = indicators[0]
          IndicatorSampleFetchPlan(queryInstant.first, indicator, mapOf(Pair("time", queryInstant.second.toString())))
        }
      }
      .filter { it != null }
      .map { it!! }
      .collect(Collectors.toList())
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