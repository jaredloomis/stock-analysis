package com.jaredloomis.moneygetter.datasync

import org.kodein.di.DI
import org.kodein.di.bind
import org.kodein.di.instance
import org.kodein.di.singleton
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.time.Duration
import java.time.temporal.ChronoUnit

/**
 * Fetches cached indicators given an IndicatorSampleFetch.
 */
class IndicatorCache {
  private val log: Logger = LoggerFactory.getLogger(javaClass)
  private val db by di.instance<StockDatabase>()

  fun fetchIfPossible(fetch: IndicatorSampleFetch): List<IndicatorSample> {
    return fetchAsList(fetch)
  }

  fun fetchAsList(fetch: IndicatorSampleFetch, leeway: Duration=Duration.ofHours(1), normalizeToDays: Boolean=false): List<IndicatorSample> {
    val (startTime, endTime) = if(normalizeToDays) {
      Pair(
        fetch.getTime()?.toInstant()?.truncatedTo(ChronoUnit.DAYS).toString(),
        fetch.getTime()?.toInstant()?.truncatedTo(ChronoUnit.DAYS)?.plus(1, ChronoUnit.DAYS).toString()
      )
    } else {
      Pair(
        fetch.getTime()?.minus(leeway.dividedBy(2)).toString(),
        fetch.getTime()?.plus(leeway.dividedBy(2)).toString()
      )
    }
    val query = db.indicatorSampleQueries.find(
      fetch.indicatorID,
      fetch.getTickers()!![0],
      startTime,
      endTime
    )
    log.trace("Querying from DB: $fetch - $query(${fetch.indicatorID}, ${fetch.getTickers()!![0]}, $startTime, $endTime)")
    return query.executeAsList()
  }
}

val indicatorCacheModule = DI.Module("IndicatorCache") {
  bind<IndicatorCache>() with singleton { IndicatorCache() }
}
