package com.jaredloomis.moneygetter.datasync

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.time.Duration
import java.time.Instant

data class IndicatorSampleFetchPlan(val query: IndicatorQuery, val indicator: IndicatorSpec, val instant: Instant, val cmdTemplate: String) {
  fun getCommand(): List<String> {
    val cmdWithArgs = query.arguments.entries.fold(cmdTemplate) { acc, arg ->
      if(arg.key == "tickers") {
        acc.replace("\$${arg.key}", showTickers(arg.value as List<String>))
      } else {
        acc.replace("\$${arg.key}", arg.value.toString())
      }
    }

    val cmdWithTime = cmdWithArgs.replace("\$time", instant.toString())

    return cmdWithTime.split(" ")
  }
}

data class IndicatorScheduler(val groups: List<IndicatorGroupSpec>) {
  val logger: Logger = LoggerFactory.getLogger(javaClass)

  fun createFetchPlan(queries: Sequence<IndicatorQuery>): List<IndicatorSampleFetchPlan> {
    val samples = mutableListOf<Pair<IndicatorQuery, Instant>>()

    for(query in queries) {
      for(timerange in query.getSchedule().ranges()) {
        val startTime = timerange.first.toInstant()
        val endTime   = timerange.second.toInstant()
        val duration  = Duration.between(startTime, endTime)
        val sampleRate = query.getSampleRate()
        val sampleCount = duration.toMillis() / sampleRate.toMillis()

        for(sampleI in 0..sampleCount) {
          val sampleTime = startTime.plus(sampleRate.multipliedBy(sampleI))
          if(sampleTime < endTime) {
            samples.add(Pair(query, sampleTime))
          }
        }
      }
    }

    return samples
      .mapNotNull { sample ->
        val indicators = getAvailableAt(sample.first.indicatorID, sample.second)
        if(indicators.isEmpty()) {
          logger.error("No data sources available to fetch sample: $sample")
          null
        } else {
          val indicator = indicators[0]
          IndicatorSampleFetchPlan(sample.first, indicator, sample.second, indicator.getCommandFor(sample.first))
        }
      }
      .toList()
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