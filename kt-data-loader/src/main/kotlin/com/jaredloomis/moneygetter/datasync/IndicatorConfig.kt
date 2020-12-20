package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.instance
import org.slf4j.LoggerFactory

class IndicatorConfig(val configString: String) {
  private val mapper by di.instance<ObjectMapper>()
  private val log = LoggerFactory.getLogger(javaClass)

  private var jsonCache: JsonNode? = null
  val json: JsonNode
    get() = toJson()


  fun getFetchCommand(query: IndicatorQuery): String {
    // Select first fetcher for specified indicator in the config file
    val fetchers = getFetchers(query)
    val fetcher = fetchers.next()
    log.debug("Fetcher for query ${query}:\n${fetcher}")

    // Instantiate $variables from query arguments into command string
    val cmdTemplate = fetcher["command"].asText()
    return query.arguments.entries.fold(cmdTemplate) { acc, arg ->
      if(arg.key == "tickers") {
        acc.replace("\$${arg.key}", showTickers(arg.value as List<String>))
      } else {
        acc.replace("\$${arg.key}", arg.value.toString())
      }
    }
  }

  fun getFetchers(query: IndicatorQuery): Iterator<JsonNode> {
    return json[query.indicatorID].elements().asSequence()
      .filter { it.isObject && it.has("command") }
      .iterator()
  }

  private fun toJson(): JsonNode {
    if(jsonCache == null) {
      jsonCache = mapper.readTree(configString)
    }
    return jsonCache!!
  }
}