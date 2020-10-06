package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.JsonNode
import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import java.util.UUID
import java.util.Date
import java.util.concurrent.TimeUnit

data class SentimentSample(
  @JsonProperty("sentimentTicker") val ticker: String,
  @JsonProperty("sentimentPeriod") val periodList: List<Date>,
  @JsonProperty("sentimentMaster") val masterSentiment: Double,
  @JsonProperty("sentimentBuzz") val buzz: Double,
  @JsonProperty("sentimentApiId") val apiID: String,
  @JsonProperty("sentimentFetchDate") val fetchDate: Date,
  @JsonProperty("sentimentRaw") val raw: JsonNode
): IsIndicatorSample {
  companion object {
    const val INDICATOR_ID = "sentiment"
  }

  val periodStart: Instant
    get() = periodList[0].toInstant()
  val periodEnd: Instant
    get() = periodStart
      .plus(23, ChronoUnit.HOURS)
      .plus(59, ChronoUnit.MINUTES)

  override fun asSample(): IndicatorSample {
    val formatter = DateTimeFormatter.ISO_INSTANT

    return IndicatorSample(
      UUID.randomUUID().toString(),
      ticker, // TODO: find ID of stock associated with ticker
      getIndicatorID(),
      apiID,
      formatter.format(periodStart),
      formatter.format(periodEnd),
      masterSentiment
    )
  }

  override fun getIndicatorID(): String {
    return "sentiment"
  }
}