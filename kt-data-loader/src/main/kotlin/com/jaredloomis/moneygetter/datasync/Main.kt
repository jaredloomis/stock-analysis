package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.*
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import org.kodein.di.instance
import org.slf4j.LoggerFactory
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util.*
import java.util.concurrent.*
import kotlin.math.min
import kotlin.system.exitProcess
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import java.util.concurrent.Executor
import java.util.concurrent.atomic.AtomicLong
import java.util.stream.Collectors

fun main(args: Array<String>) {
  CLIRunner().main(args)
}

class CLIRunner : CliktCommand() {
  private val log = LoggerFactory.getLogger(javaClass)

  private val queryConfigFile: File by option(help = "Path to query config file")
    .file(mustBeReadable = true)
    .required()
  private val indicatorConfigFile: File by option(help = "Path to global indicator config file")
    .file(mustBeReadable = true)
    .default(
      Paths.get("/home/jared/workspace/hobby/stock-analysis/config/indicators.json").toFile()
      /*
      Paths.get(javaClass.protectionDomain.codeSource.location.toURI())
           .parent
           .parent.parent.parent
           .resolve("config").resolve("indicators.json")
           .toFile()
       */
      )
  private val serverMode: Boolean by option(help = "Enable HTTP server mode")
    .flag(default = false)
  private val plan: Boolean by option(help = "Don't fetch any data - just print out planned queries")
    .flag(default = false)
  private val noCacheCheck: Boolean by option(help = "Don't check the db for samples before fetching")
    .flag(default = false)
  private val logLevel: String by option(help = "Default: INFO\nOFF - Don't print out logs; just return the result. Logs are saved to a file instead.\nERROR - error\nINFO - info\nDEBUG - debug\nTRACE - trace")
    .default("INFO")
    .check("must be one of: OFF, TRACE, DEBUG, INFO, ERROR") { it == "OFF" || it == "ERROR" || it == "INFO" || it == "DEBUG" || it == "TRACE" }

  private val mapper by di.instance<ObjectMapper>()

  private var dataSourceConfigCache: DataSourceConfig? = null

  override fun run() {
    when {
      serverMode -> Server(3002, getIndicatorConfig()).start()
      plan -> {}// TODO log.info(DataSourceExecutor(logLevel).)
      else -> {
        val queryConfig = loadQueryConfig()
        DataSourceExecutor(logLevel).run(
          DataSourceExecutorRequest(getIndicatorConfig(), queryConfig, noCacheCheck)
        ) { samples ->
          samples.forEach { log.debug(it.toString()) }
        }
      }
    }
  }

  private fun loadQueryConfig(): IndicatorQueryConfig {
    return mapper.readValue(queryConfigFile, IndicatorQueryConfig::class.java)
  }

  private fun getIndicatorConfig(): DataSourceConfig {
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
