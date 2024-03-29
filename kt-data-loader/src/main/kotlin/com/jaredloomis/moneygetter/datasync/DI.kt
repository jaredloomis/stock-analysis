package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.DI
import org.kodein.di.bind
import org.kodein.di.singleton
import java.nio.file.Files
import java.nio.file.Paths
import java.time.format.DateTimeFormatter

val di = DI {
  import(configPropertiesModule)
  import(postgresModule)
  import(indicatorCacheModule)
  import(queryManagerModule)

  val mapper = ObjectMapper()

  bind<ObjectMapper>() with singleton {
    mapper
  }

  bind<DateTimeFormatter>() with singleton {
    DateTimeFormatter.ISO_INSTANT
  }

  bind<Set<Stock>>("stocks.sp500") with singleton {
    val jsonText = Files.readString(Paths.get("../data/github.com_datasets/S&P_500_constituents.json"))
    val jsonNode = mapper.readTree(jsonText)
    jsonNode.asIterable()
      .mapNotNull {
        try {
          val ticker = it["Symbol"].asText()
          Stock(ticker, ticker, it["Name"].asText())
        } catch(ex: Exception) {
          ex.printStackTrace()
          null
        }
      }
      .shuffled()
      .toSet()
  }

  bind<Set<Stock>>("stocks.watchlist") with singleton {
    val jsonText = Files.readString(Paths.get("../data/jaredloomis.com/watchlist.json"))
    val jsonNode = mapper.readTree(jsonText)
    jsonNode.asIterable()
      .map { it.asText() }
      .map { Stock(it, it, it) }
      .shuffled()
      .toSet()
  }

  bind<Set<Stock>>("stocks.all") with singleton {
    val jsonText = Files.readString(Paths.get("../data/eodhistoricaldata.com/US_LIST_OF_SYMBOLS.json"))
    val jsonNode = mapper.readTree(jsonText)
    jsonNode.asIterable()
      .mapNotNull {
        try {
          val ticker = it["Code"].asText()
          Stock(ticker, ticker, it["Name"].asText())
        } catch(ex: Exception) {
          ex.printStackTrace()
          null
        }
      }
      .shuffled()
      .toSet()
  }
}