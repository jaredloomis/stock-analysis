package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty


/**
 * An {@code IndicatorSpec} is an indicator ID, along with any additional properties
 * needed to execute the indicator fetcher.
 */
data class IndicatorFetchSpec(
  @JsonProperty("indicator") val indicatorID: String,
  @JsonProperty("arguments") val arguments: MutableMap<String, Any>
) {
  val stringArguments: Map<String, String>
    get() = arguments
      .filter    { it.value is String }
      .mapValues { it.value as String }

  val tickers: List<String>
    get() = arguments.getOrDefault("tickers", emptyList<String>()) as List<String>

  operator fun get(s: String): Any? {
    return arguments[s]
  }
}

data class IndicatorFetchSpecConfig(
  @JsonProperty("indicators") val specs: List<IndicatorFetchSpec>
)
