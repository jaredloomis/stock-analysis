package com.jaredloomis.moneygetter.datasync

import org.kodein.di.instance

class IndicatorSampleQueries_ {
  val queryManager by di.instance<QueryManager>()

  fun find() {
    val args: List<Any> = listOf("", 1)
    queryManager.withQueryResults("findSamples", args) { results ->
      //results.
    }
  }
}