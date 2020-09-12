package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.JsonNode
import java.time.Instant
import java.util.UUID

data class PriceSample(
  @JsonProperty("quoteTicker") val ticker: String,
  @JsonProperty("quoteValue") val value: Double,
  @JsonProperty("quoteTime") val timestring: String,
  @JsonProperty("quoteApiId") val sourceApiId: String,
  @JsonProperty("quoteRaw") val raw: JsonNode
): IsIndicatorSample {
  companion object {
    const val INDICATOR_ID = "price"
  }

  val time: Instant
    get() = Instant.parse(timestring)

  override fun asSample(): IndicatorSample {
    return IndicatorSample(
      UUID.randomUUID().toString(),
      ticker, // TODO: find ID of stock associated with ticker
      getIndicatorID(),
      sourceApiId,
      timestring,
      timestring,
      value
    );
  }

  override fun getIndicatorID(): String {
    return "price"
  }
}