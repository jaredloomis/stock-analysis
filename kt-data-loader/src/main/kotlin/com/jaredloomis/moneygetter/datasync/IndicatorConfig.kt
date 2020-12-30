package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.instance
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.sql.Time
import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.time.temporal.TemporalQuery
import kotlin.math.abs

val logger: Logger = LoggerFactory.getLogger("com.jaredloomis.moneygetter.datasync.IndicatorConfig.kt file")

typealias IndicatorGroupSpec = List<IndicatorSpec>
data class IndicatorSpec(val group: String, val name: String, val command: String, val schedule: IndicatorSchedule) {
  fun provides(indicatorID: String): Boolean {
    return group == indicatorID || name.contains(indicatorID)
  }

  fun getCommandFor(query: IndicatorQuery): String {
    return query.arguments.entries.fold(command) { acc, arg ->
      if(arg.key == "tickers") {
        acc.replace("\$${arg.key}", showTickers(arg.value as List<String>))
      } else {
        acc.replace("\$${arg.key}", arg.value.toString())
      }
    }
  }
}

data class IndicatorSchedule(val raw: List<IndicatorTime>) {
  fun isAvailableAt(time: IndicatorTime): Boolean {
    if(raw.size < 2 || raw.size % 2 != 0) {
      logger.error("In indicator config file, \"schedule\" must be an array of value pairs: start, end, start, end, ...")
      return false
    }

    if(isNowish(time.toInstant())) {
      return raw.chunked(2)
        .any { chunk -> chunk.size == 2 && chunk[1] == IndicatorTime.Now }
    }

    return raw.chunked(2)
      .any { time >= it[0] && time <= it[1] }
  }

  fun ranges(): List<Pair<IndicatorTime, IndicatorTime>> {
    return raw.chunked(2)
      .map {it[0] to it[1] }
      .sortedBy { it.first }
  }

  fun startTime(): Instant {
    return raw.minOf { it.toInstant() }
  }

  fun endTime(): Instant {
    return raw.maxOf { it.toInstant() }
  }

  fun plus(that: IndicatorSchedule): IndicatorSchedule {
    return IndicatorSchedule(raw.plus(that.raw))
  }

  fun isInstant(): Boolean {
    return raw.size < 2 || raw.all { it == raw[0] }
  }

  fun toInstant(): Instant? {
    return if(isInstant()) {
      if(raw.isEmpty()) {
        Instant.now()
      } else {
        raw[0].toInstant()
      }
    } else {
      null
    }
  }

  fun asArray(): List<String> {
    return raw.map { it.toString() }
  }

  companion object {
    fun isNowish(instant: Instant): Boolean {
      return abs(Instant.now().toEpochMilli() - instant.toEpochMilli()) < 60 * 1000
    }
  }
}

sealed class IndicatorTime: Comparable<IndicatorTime> {
  class Timestamp(val instant: Instant): IndicatorTime()
  object Now : IndicatorTime()

  fun toInstant(): Instant {
    return when(this) {
      is Timestamp -> this.instant
      Now -> Instant.now()
    }
  }

  override operator fun compareTo(other: IndicatorTime): Int {
    return when(this) {
      Now -> 0
      is Timestamp -> this.toInstant().compareTo(other.toInstant())
    }
  }

  override fun toString(): String {
    val timeStr = when(this) {
      Now -> "now"
      is Timestamp -> this.toInstant().toString()
    }
    return "IndicatorTime($timeStr)"
  }

  companion object {
    fun fromString(str: String): IndicatorTime {
      return if(str.trim().toLowerCase() == "now") {
        Now
      } else {
        try {
          return Timestamp(LocalDate.parse(str).atStartOfDay(ZoneId.systemDefault()).toInstant())
        } catch (ex: java.lang.Exception) {
          ex.printStackTrace()

          try {
            return Timestamp(ZonedDateTime.parse(str).toInstant())
          } catch (ex: Exception) {
            ex.printStackTrace()

            try {
              Timestamp(DateTimeFormatter.ISO_DATE_TIME.parse(str, TemporalQuery<Instant> { temporal: TemporalAccessor? -> Instant.from(temporal) }) as Instant)
            } catch (ex: Exception) {
              ex.printStackTrace()
              return Timestamp(Instant.parse(str))
            }
          }
        }
      }
    }
  }
}

class IndicatorConfig(val configString: String) {
  private val mapper by di.instance<ObjectMapper>()
  private val log = LoggerFactory.getLogger(javaClass)

  private var jsonCache: JsonNode? = null
  val json: JsonNode
    get() = toJson()

  fun getGroups(): List<IndicatorGroupSpec> {
    return json.fields().asSequence().toList()
      .filter { it.value.isObject }
      .map { grp ->
        grp.value.fields().asSequence().toList()
          .filter { it.value.isObject }
          .map { ind ->
            IndicatorSpec(
              grp.key,
              ind.key,
              ind.value["command"].asText(),
              IndicatorSchedule(ind.value["schedule"].elements().asSequence()
                .map { it.asText() }
                .map { IndicatorTime.fromString(it) }
                .toList()
              )
            )
          }
      }
  }

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