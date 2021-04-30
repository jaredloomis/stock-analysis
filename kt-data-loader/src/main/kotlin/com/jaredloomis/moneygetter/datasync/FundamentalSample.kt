package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.JsonNode
import org.kodein.di.instance
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import java.util.UUID
import java.util.Date
import java.util.concurrent.TimeUnit

data class FundamentalSample(
  @JsonProperty("ticker") val ticker: String,
  @JsonProperty("period") val periodList: List<Date>,
  @JsonProperty("type") val type: FundamentalSampleType,
  @JsonProperty("value") val value: Double,
  @JsonProperty("dataSourceID") val dataSourceID: String,
  @JsonProperty("fetchTime") val fetchTime: Instant,
  @JsonProperty("raw") val raw: JsonNode
): IsIndicatorSample {
  companion object {
    const val INDICATOR_ID = "fundamental"
  }

  val periodStart: Instant
    get() = periodList[0].toInstant()
  val periodEnd: Instant
    get() = periodStart
      .plus(23, ChronoUnit.HOURS)
      .plus(59, ChronoUnit.MINUTES)

  private val formatter by di.instance<DateTimeFormatter>()

  override fun asSample(): IndicatorSample {
    return IndicatorSample(
      UUID.randomUUID().toString(),
      ticker, // TODO: find ID of stock associated with ticker
      getIndicatorID(),
      dataSourceID,
      formatter.format(periodStart),
      formatter.format(periodEnd),
      formatter.format(fetchTime),
      value,
      raw.toString()
    )
  }

  override fun getIndicatorID(): String {
    return "sentiment"
  }
}

enum class FundamentalSampleType {
  MARKET_CAP
}