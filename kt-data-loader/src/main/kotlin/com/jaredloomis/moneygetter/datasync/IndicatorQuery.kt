package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty
import org.kodein.di.instance
import java.time.Duration

/**
 * An {@code IndicatorQuery} is an indicator ID, along with arguments to pass to the indicator's data source(s).
 */
data class IndicatorQuery(
  @JsonProperty("indicator")  val indicatorID: String,
  @JsonProperty("arguments")  val arguments: MutableMap<String, Any>,
  @JsonProperty("schedule")   val scheduleRaw: List<String>? = listOf("now", "now"),
  @JsonProperty("sampleRate") val sampleRate: Long? = 86400000
) {
  private val watchlist by di.instance<Set<Stock>>("stocks.watchlist")

  var tickers: List<String>
    get()     = arguments.getOrDefault("tickers", emptyList<String>()) as List<String>
    set(strs) = arguments.set("tickers", strs)

  init {
    // Expand '*' to mean 'all stock tickers'
    val all = tickers.any { it == "*" }
    if(all) {
      tickers = tickers
        .filter { it == "*" }
        .plus(getStarTickers().map { it.ticker })
    }
  }

  fun getSchedule(): DataSourceSchedule {
    return if(scheduleRaw != null) {
      DataSourceSchedule(scheduleRaw.map { IndicatorTime.fromString(it) })
    } else {
      DataSourceSchedule(listOf(IndicatorTime.Now, IndicatorTime.Now))
    }
  }

  fun getSampleRate(): Duration {
    val rateNum = arguments.getOrDefault("sampleRate", null) as Long?
    return if(rateNum != null) {
      Duration.ofMillis(rateNum)
    } else {
      Duration.ofDays(1)
    }
  }

  fun withTickers(tickers: List<String>): IndicatorQuery {
    val ret = clone()
    ret.tickers = tickers
    return ret
  }

  fun clone(): IndicatorQuery {
    return IndicatorQuery(indicatorID, HashMap(arguments), scheduleRaw, sampleRate)
  }

  operator fun get(s: String): Any? {
    return arguments[s]
  }

  private fun getStarTickers(): Set<Stock> {
    return watchlist
  }
}

data class IndicatorQueryConfig(
  @JsonProperty("queries") val queries: List<IndicatorQuery>
)
