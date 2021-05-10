package com.jaredloomis.moneygetter.datasync

import java.io.IOException

import com.sun.net.httpserver.HttpExchange

import com.sun.net.httpserver.HttpHandler
import java.net.InetSocketAddress

import com.sun.net.httpserver.HttpServer

class ServerHandler : HttpHandler {
  @Throws(IOException::class)
  override fun handle(t: HttpExchange) {
    val response = "This is the response"
    t.sendResponseHeaders(200, response.length.toLong())
    val os = t.responseBody
    os.write(response.toByteArray())
    os.close()
  }
}

class Server(val port: Int) {
  fun start() {
    val server = HttpServer.create(InetSocketAddress(8000), 0)
    server.createContext("/test", ServerHandler())
    server.executor = null
    server.start()
  }
}