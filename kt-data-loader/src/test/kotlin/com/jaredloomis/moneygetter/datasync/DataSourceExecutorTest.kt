package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import org.junit.jupiter.api.Test
import org.kodein.di.instance
import java.nio.file.Files
import java.nio.file.Paths

class DataSourceExecutorTest {
  private val mapper by di.instance<ObjectMapper>()
  private val executor = DataSourceExecutor("DEBUG")

  private var dataSourceConfigCache: DataSourceConfig? = null

  private val indicatorConfigFile =
      Paths.get("/home/jared/workspace/hobby/stock-analysis/config/indicators.json").toFile()

  @Test
  fun testCurrentPriceOfStocks_withCache() {
    repeat(3) {
      val body = """
      {
        "query": {
          "indicator": "price",
          "arguments": {
            "tickers": ["AAPL", "AMD", "MSFT"]
            }
        },
        "noCacheCheck": false
      }
    """.trimIndent()
      val req = mapper.readValue(body, Request::class.java)
      val dataSourceReq = DataSourceExecutorRequest(getDataSourceConfig(), IndicatorQueryConfig(listOf(req.query)), req.noCacheCheck
        ?: false)
      val samples: MutableList<IndicatorSample> = ArrayList()
      val executorResponse = executor.run(dataSourceReq) { println("WASABI $it"); samples.addAll(it) }
      while(executor.runInProgress.get()) {}
      assert(executorResponse.error == null)
      println(samples)
      assert(samples.size > 0)
    }
  }

  private fun getDataSourceConfig(): DataSourceConfig {
    if(dataSourceConfigCache != null) {
      return dataSourceConfigCache!!
    }

    // Get command template from config file
    val indicatorsMap = indicatorConfigFile.toPath()
    val indicatorsJson = Files.readString(indicatorsMap)
    // Create command string from template and query
    val conf = DataSourceConfig(indicatorsJson)
    dataSourceConfigCache = conf
    return conf
  }
}