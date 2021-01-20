package com.jaredloomis.moneygetter.datasync

import org.kodein.di.instance

/**
 * Fetches cached indicators given an IndicatorSampleFetchPlan
 */
class IndicatorCache {
  private val db by di.instance<StockDatabase>()

  fun fetch(plan: IndicatorSampleFetchPlan): List<IndicatorSample> {
    val queryTime = plan.query.getSchedule().startTime().toString()
    plan.query.tickers.forEach { ticker ->
      db.indicatorSampleQueries.find(plan.query.indicatorID, ticker, queryTime, queryTime)
    }
  }
}