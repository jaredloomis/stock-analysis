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
 * Fetches cached indicators given an IndicatorSampleFetchPlan
 */
class IndicatorCache {
  private val log: Logger = LoggerFactory.getLogger(javaClass)
  private val db by di.instance<StockDatabase>()

  fun fetchAllIfPossible(batch: IndicatorSampleFetchBatch): Pair<List<IndicatorSample>, IndicatorSampleFetchBatch> {
    val samples = ArrayList<IndicatorSample>(0)
    var retBatch = batch
    for((i, args) in batch.argsList.withIndex()) {
      val cached = fetchAsList(IndicatorSampleFetch(batch.indicatorID, args))
      if(cached.isNotEmpty()) {
        log.info("Found existing sample in DB for $batch")
        samples.addAll(cached)
        retBatch = retBatch.withoutIndex(i)
      }
    }

    return Pair(samples, retBatch)
  }

  fun fetchAsList(fetch: IndicatorSampleFetch, leeway: Duration=Duration.ofHours(2), normalizeToDays: Boolean=true): List<IndicatorSample> {
    val (startTime, endTime) = if(normalizeToDays) {
      Pair(
        fetch.getTime()?.truncatedTo(ChronoUnit.DAYS).toString(),
        fetch.getTime()?.truncatedTo(ChronoUnit.DAYS)?.plus(1, ChronoUnit.DAYS).toString()
      )
    } else {
      Pair(
        fetch.getTime()?.minus(leeway.dividedBy(2)).toString(),
        fetch.getTime()?.plus(leeway.dividedBy(2)).toString()
      )
    }
    val query = db.indicatorSampleQueries.find(
      fetch.indicatorID,
      fetch.getTickers()[0],
      startTime,
      endTime
    )
    log.debug("Querying from DB: $fetch - $query(${fetch.indicatorID}, ${fetch.getTickers()[0]}, $startTime, $endTime)")
    return query.executeAsList()
  }
}

val indicatorCacheModule = DI.Module("IndicatorCache") {
  bind<IndicatorCache>() with singleton { IndicatorCache() }
}
