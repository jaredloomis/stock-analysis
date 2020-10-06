package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.instance
import java.nio.file.Files
import java.nio.file.Paths


/**
 * An {@code IndicatorQuery} is an indicator ID, along with arguments to pass to the indicator's data source(s).
 */
data class IndicatorQuery(
  @JsonProperty("indicator") val indicatorID: String,
  @JsonProperty("arguments") val arguments: Map<String, Any>
) {
  private val mapper by di.instance<ObjectMapper>()

  val stringArguments: Map<String, String>
    get() = arguments
      .filter    { it.value is String }
      .mapValues { it.value as String }

  val tickers: List<String>
    get() = _getTickers()

  /**
   * Splits query into batches based on number of tickers.
   */
  fun batches(batchSize: Int): List<IndicatorQuery> {
    return if(tickers.isNotEmpty() && batchSize != 0) {
      tickers.chunked(batchSize).map { chunk ->
        IndicatorQuery(indicatorID, arguments.plus(Pair("tickers", chunk)))
      }
    } else {
      listOf(this)
    }
  }

  operator fun get(s: String): Any? {
    return arguments[s]
  }

  private fun _getTickers(): List<String> {
    val tickers = arguments.getOrDefault("tickers", emptyList<String>()) as List<String>
    val all = tickers.any { it == "*" }
    return tickers.plus(listAllTickers().map { it.first })
  }

  private fun listAllTickers(): List<Pair<String, String>> {
    val jsonText = Files.readString(Paths.get("../data/stock_list_nyse.json"))
    val jsonNode = mapper.readTree(jsonText)
    return jsonNode.asIterable()
      .map { Pair(it["ACT Symbol"].asText(), it["Company Name"].asText()) }
      .toList()
  }
}

data class IndicatorQueryConfig(
  @JsonProperty("queries") val queries: List<IndicatorQuery>
)
