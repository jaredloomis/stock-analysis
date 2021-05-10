package com.jaredloomis.moneygetter.datasync

import java.time.Duration
import java.util.function.Consumer
import java.util.function.Supplier
import java.util.stream.Stream
import kotlin.streams.asStream

data class IndicatorSampleFetch(
  val indicatorID: String, val args: Map<String, Any>
) {
  fun getTickers(): List<String>? {
    val tickersRaw = args["tickers"]
    return tickersRaw as List<String>
    return if(tickersRaw is List<*>) {
      tickersRaw as List<String>
    } else {
      null
    }
  }

  fun getTime(): IndicatorTime? {
    return when(val t = args["time"]) {
      is IndicatorTime -> t
      null             -> null
      else             -> {
        logger.error("Time was of type ${t.javaClass}: ${t.toString()}")
        null
      }
    }
  }
}

data class IndicatorSampleFetchBatch(
  val dataSources: List<DataSourceSpec>, val argsList: List<Map<String, Any?>>
) {
  val args = argsList.fold(emptyMap<String, Any?>()) { acc, argSet ->
      // Combine ticker args
      val accTickers = acc["tickers"] as List<String>?
      val newTickers = argSet["tickers"] as List<String>?
      val allTickers: List<String> =
        if(accTickers != null && newTickers != null) {
          accTickers.plus(newTickers).toSet().toList()
        } else {
          accTickers ?: newTickers ?: emptyList()
        }

    argSet
      .plus(acc)
      .plus(Pair("tickers", allTickers))
    }

  fun getCommand(dataSource: DataSourceSpec): List<String> {
    val cmdWithArgs = args.entries.fold(dataSource.commandTemplate) { acc, arg ->
      if(arg.key == "tickers") {
        acc.replace("\$${arg.key}", showTickers(arg.value as List<String>))
      } else {
        acc.replace("\$${arg.key}", arg.value.toString())
      }
    }

    return cmdWithArgs.split(" ")
  }

  fun <A> tryWithDataSource(f: (DataSourceSpec) -> A): A? {
    for(dataSource in dataSources) {
      try {
        return exponentialBackoffRetry(dataSource.retries ?: 3, initialDelay = 500) { f(dataSource) }
      } catch(ex: Exception) {
        logger.debug("tryWithCommand: data source failed; ignoring -- $ex")
        continue
      }
    }
    return null
  }

  @Throws(Exception::class)
  private fun <A> exponentialBackoffRetry(tries: Int, initialDelay: Int, f: () -> A): A {
    var ex = Exception()
    var delay = initialDelay.toDouble()
    for(i in 1..tries) {
      try {
        return f()
      } catch(exI: Exception) {
        ex = exI
        Thread.sleep(delay.toLong())
        delay *= 2
      }
    }
    throw ex
  }

  fun withoutIndex(index: Int): IndicatorSampleFetchBatch {
    return if(index < argsList.size) {
      IndicatorSampleFetchBatch(dataSources, argsList.filterIndexed { i, _ -> i != index })
    } else {
      this
    }
  }

  override fun toString(): String {
    return "IndicatorSampleFetchBatch(dataSources=$dataSources, args=$args)"
  }
}

data class IndicatorFetchPlanner(val groups: List<DataSourceGroupSpec>) {
  fun createFetchPlan(queries: Sequence<IndicatorQuery>): Pair<Stream<IndicatorSampleFetch>, List<IndicatorSampleFetch>> {
    val abortedFetches: MutableList<IndicatorSampleFetch> = ArrayList()

    val sampleFetches = queries.asStream()
      // For each query, create enough samples to cover the requested timeranges
      .flatMap { query ->
        query.getSchedule().ranges().stream()
          .flatMap { timerange ->
            val startTime = timerange.first
            val endTime   = timerange.second
            val duration  = Duration.between(startTime.toInstant(), endTime.toInstant())
            val sampleRate = query.getSampleRate()
            val sampleCount = Math.max(1, duration.toMillis() / sampleRate.toMillis())

            if(duration.toMillis() < 1000) {
              Stream.of(Pair(query, endTime))
            } else {
              Stream.iterate(0L, { i -> i + 1 }).limit(sampleCount)
                .map { sampleI ->
                  val sampleTime = startTime.plus(sampleRate.multipliedBy(sampleI))
                  Pair(query, sampleTime)
                }
            }
          }
      }
      // Construct IndicatorSampleFetch for each ticker requested, for every sample time
      .flatMap { queryInstant ->
        queryInstant.first.tickers.stream()
          .map { ticker ->
            val args = queryInstant.first.arguments
              .plus(Pair("time", queryInstant.second))
              .plus(Pair("tickers", listOf(ticker)))
            IndicatorSampleFetch(queryInstant.first.indicatorID, args)
          }
      }
      .filter { it.getTickers()?.isNotEmpty() ?: false }
      // Filter out fetches that are not possible (failed fetches)
      .filter { sampleFetch ->
        val isNull = assignDataSource(sampleFetch.indicatorID, sampleFetch.getTime()!!) == null
        if(isNull) {
          abortedFetches.add(sampleFetch)
        }
        !isNull
      }

    return Pair(sampleFetches, abortedFetches)
  }

  /**
   * Given a list of samples to fetch, group them into batches.
   * Two samples belong in the same batch if they have the same data source and the same timestamp.
   *
   * Examples of batchable fetches:
   * - Duplicates
   * - Requests for the same data source, with the same timestamp, but for differing tickers.
   */
  fun batchFetches(plan: List<IndicatorSampleFetch>, batchSize: Int): List<IndicatorSampleFetchBatch> {
    return plan.stream()
      // Group into batches by available data sources and timestamp
      .reduce(
        emptyMap<Pair<List<DataSourceSpec>, IndicatorTime>, List<IndicatorSampleFetch>>(),
        { dataSourceSampleMap, fetch ->
          val time = fetch.getTime()!!
          val dataSources = availableDataSources(fetch.indicatorID, time)
          val key = Pair(dataSources, time)
          if(dataSourceSampleMap.containsKey(key)) {
            dataSourceSampleMap.plus(
              Pair(key, dataSourceSampleMap[key]!!.plus(fetch))
            )
          } else {
            dataSourceSampleMap.plus(
              Pair(key, listOf(fetch))
            )
          }
        },
        { a, b -> a.plus(b) }
      )
      // Break up large batches according to batchSize
      .flatMap { batch ->
        val dataSourceSpec = batch.key
        val samples = batch.value

        if(samples.size > batchSize) {
          samples.chunked(batchSize).map { Pair(dataSourceSpec, it) }
        } else {
          listOf(Pair(dataSourceSpec, samples))
        }
      }
      // Remove empty batches, if any
      .filter { it.second.isNotEmpty() }
      // For each batch, construct an IndicatorSampleFetchBatch
      .map { (fetchKey, fetches) ->
        IndicatorSampleFetchBatch(fetchKey.first, fetches.map { it.args })
      }
  }

  fun streamingBatch(plan: Stream<IndicatorSampleFetch>, defaultBatchSize: Int=32): Pair<Stream<IndicatorSampleFetchBatch>, () -> IndicatorSampleFetchBatch> {
    var currentKey: Pair<List<DataSourceSpec>, IndicatorTime>? = null
    val currentArgs: MutableList<Map<String, Any>> = ArrayList()
    val batchSize = {
        if (currentKey?.first?.isNotEmpty() == true) {
          currentKey?.first?.get(0)?.batchSize ?: defaultBatchSize
        } else {
          defaultBatchSize
        }
      }
    // Return current batch, start a new one
    val startNewBatch = { sampleFetch: IndicatorSampleFetch, dataSources: List<DataSourceSpec> ->
        val ret = IndicatorSampleFetchBatch(currentKey!!.first, ArrayList(currentArgs))
        currentKey = Pair(dataSources, sampleFetch.getTime()!!)
        currentArgs.clear()
        currentArgs.add(sampleFetch.args)
        ret
      }

      val stream = plan
        .map { sampleFetch ->
          val time = sampleFetch.getTime()!!
          val dataSources = availableDataSources(sampleFetch.indicatorID, time)

          // If this sample belongs in the current batch
          if(currentKey != null && dataSources == currentKey?.first && time == currentKey!!.second) {
            // And the batch has enough room, add it to the batch
            if(currentArgs.size < batchSize()) {
              currentArgs.add(sampleFetch.args)
              null
            }
            // Otherwise, return current batch and start a new one
            else {
              startNewBatch(sampleFetch, dataSources)
            }
          }
          // If this sample doesn't belong in the current batch, return current batch and start a new one
          else if(currentKey != null) {
            startNewBatch(sampleFetch, dataSources)
          }
          // If current key is null, start the first batch
          else {
            currentKey = Pair(dataSources, time)
            currentArgs.add(sampleFetch.args)
            null
          }
        }
        .filter { it != null }
        .map { it!! }

      // Add the last batch
      return Pair(stream, {
        IndicatorSampleFetchBatch(currentKey!!.first, ArrayList(currentArgs))
      })
  }

  private fun assignDataSource(indicatorID: String, time: IndicatorTime): DataSourceSpec? {
    return availableDataSources(indicatorID, time).getOrNull(0)
  }

  private fun availableDataSources(indicatorID: String, time: IndicatorTime): List<DataSourceSpec> {
    return flattenList(groups)
      .filter { it.provides(indicatorID) && it.schedule.isAvailableAt(time) }
      .toList()
  }

  private fun <A> flattenList(xss: List<List<A>>): List<A> {
    val ret = ArrayList<A>()
    for(xs in xss) {
      for(x in xs) {
        ret.add(x)
      }
    }
    return ret
  }
}