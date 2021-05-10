package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.instance
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.sql.Time
import java.time.*
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.time.temporal.TemporalQuery
import kotlin.math.abs

val logger: Logger = LoggerFactory.getLogger("com.jaredloomis.moneygetter.datasync.IndicatorConfig.kt file")

typealias DataSourceGroupSpec = List<DataSourceSpec>
data class DataSourceSpec(
  val indicatorID: String,
  val dataSourceID: String,
  val commandTemplate: String,
  val schedule: DataSourceSchedule,
  val batchSize: Int?,
  val retries: Int?
) {
  fun provides(indicatorID: String): Boolean {
    return this.indicatorID == indicatorID || dataSourceID.contains(indicatorID)
  }

  override fun toString(): String {
    return "DataSourceSpec(indicatorID='$indicatorID', dataSourceID='$dataSourceID', commandTemplate='$commandTemplate', schedule=$schedule)"
  }
}

data class DataSourceSchedule(val raw: List<IndicatorTime>) {
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

  fun plus(that: DataSourceSchedule): DataSourceSchedule {
    return DataSourceSchedule(raw.plus(that.raw))
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
      return abs(Instant.now().toEpochMilli() - instant.toEpochMilli()) < Duration.ofHours(1).toMillis()
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

  fun plus(duration: Duration): IndicatorTime {
    return Timestamp(toInstant().plus(duration))
  }

  fun minus(duration: Duration): IndicatorTime {
    return Timestamp(toInstant().minus(duration))
  }

  override operator fun compareTo(other: IndicatorTime): Int {
    return when(this) {
      Now -> 0
      is Timestamp -> this.toInstant().compareTo(other.toInstant())
    }
  }

  override fun toString(): String {
    return toInstant().toString()
    /*
    val timeStr = when(this) {
      Now -> "now"
      is Timestamp -> this.toInstant().toString()
    }
    return "IndicatorTime($timeStr)"
     */
  }

  companion object {
    fun fromString(str: String): IndicatorTime {
      return if(str.trim().toLowerCase() == "now") {
        Now
      } else {
        try {
          return Timestamp(LocalDate.parse(str).atStartOfDay(ZoneId.systemDefault()).toInstant())
        } catch (ex: java.lang.Exception) {
          try {
            return Timestamp(ZonedDateTime.parse(str).toInstant())
          } catch (ex: Exception) {
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

class DataSourceConfig(val configString: String) {
  private val mapper by di.instance<ObjectMapper>()
  private val log = LoggerFactory.getLogger(javaClass)

  private var jsonCache: JsonNode? = null
  private val json: JsonNode

    get() = toJson()

  fun getConcurrentQueries(): Int? {
    return json["concurrentQueries"]?.asInt()
  }

  fun getGroups(): List<DataSourceGroupSpec> {
    return json.fields().asSequence().toList()
      .filter { it.value.isObject }
      .map { grp ->
        grp.value.fields().asSequence().toList()
          .filter { it.value.isObject }
          .map { ind ->
            DataSourceSpec(
              grp.key,
              ind.key,
              ind.value["command"].asText(),
              DataSourceSchedule(ind.value["schedule"].elements().asSequence()
                .map { it.asText() }
                .map { IndicatorTime.fromString(it) }
                .toList()
              ),
              ind.value["batchSize"]?.asInt(),
              ind.value["retries"]?.asInt()
            )
          }
      }
  }

  fun getFetchCommandTemplate(indicatorID: String, time: IndicatorTime): String? {
    // Select first fetcher for specified indicator in the config file
    val fetchers = getFetchers(indicatorID, time).toList()
    return if(fetchers.isNotEmpty()) {
      val fetcher = fetchers.elementAt(0)
      log.debug("Fetcher for ${indicatorID}:\n${fetcher}")
      fetcher["command"].asText()
    } else {
      null
    }
  }

  private fun getFetchers(indicatorID: String, time: IndicatorTime? = null): Sequence<JsonNode> {
    val fetchers = json[indicatorID].elements().asSequence()
      .filter { it.isObject && it.has("command") }
    return if(time == null) {
      fetchers
    } else {
      fetchers
        .filter { fetcherIsAvailableAt(it, time) }
    }
  }

  private fun fetcherIsAvailableAt(fetcher: JsonNode, time: IndicatorTime): Boolean {
    val schedule = DataSourceSchedule(
      fetcher["schedule"].asSequence()
        .map { IndicatorTime.fromString(it.asText()) }
        .toList()
    )
    return schedule.isAvailableAt(time)
  }

  private fun toJson(): JsonNode {
    if(jsonCache == null) {
      jsonCache = mapper.readTree(configString)
    }
    return jsonCache!!
  }
}