package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.JsonNode
import java.util.*

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

  val periodStart: Date
    get() = periodList[0]
  val periodEnd: Date
    get() = periodList[0]

  override fun asSample(): IndicatorSample {
    return IndicatorSample(
      UUID.randomUUID().toString(),
      ticker, // TODO: find ID of stock associated with ticker
      getIndicatorID(),
      apiID,
      periodStart.toString(),
      periodEnd.toString(),
      masterSentiment
    );
  }

  override fun getIndicatorID(): String {
    return "sentiment"
  }
}