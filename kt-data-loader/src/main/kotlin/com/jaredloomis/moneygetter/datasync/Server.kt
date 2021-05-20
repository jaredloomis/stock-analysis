package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.instance

import io.javalin.Javalin
import java.util.concurrent.CompletableFuture

data class Request(
  @JsonProperty("query") val query: IndicatorQuery,
  @JsonProperty("noCacheCheck") val noCacheCheck: Boolean?
)

data class Response(
  @JsonProperty("samples") val samples: MutableList<IndicatorSample>,
  @JsonProperty("executorResponse") val executorResponse: DataSourceExecutorResponse
)

class Server(val port: Int, val dataSourceConfig: DataSourceConfig) {
  private val mapper by di.instance<ObjectMapper>()
  //val executor = DataSourceExecutor("DEBUG")

  fun start() {
    val app = Javalin.create().start(port)
    app.post("/query") { ctx ->
      try {
        val executor = DataSourceExecutor("DEBUG")
        val body = ctx.bodyAsBytes()
        val req = mapper.readValue(body, Request::class.java)
        val dataSourceReq = DataSourceExecutorRequest(dataSourceConfig, IndicatorQueryConfig(listOf(req.query)), req.noCacheCheck
          ?: false)
        val samples: MutableList<IndicatorSample> = ArrayList()
        val executorResponse = executor.run(dataSourceReq) { println("WASABI $it"); samples.addAll(it) }
        val res = Response(samples, executorResponse)
        ctx.result(CompletableFuture.runAsync(({
          while (executor.runInProgress.get()) {}
          ctx.result(mapper.writeValueAsString(res))
        })))
      } catch(ex: Exception) {
        ex.printStackTrace()
      }
    }
  }
}
