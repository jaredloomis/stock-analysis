package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import com.github.ajalt.clikt.parameters.types.file
import com.github.ajalt.clikt.parameters.types.int
import org.kodein.di.*
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.util.*
import kotlin.streams.asSequence

fun main(args: Array<String>) {
  CLIRunner().main(args)
}

class CLIRunner : CliktCommand() {
  private val configFile: File by option(help = "Path to config file")
    .file(mustBeReadable = true)
    .required()

  private val batchSize: Int? by option(help = "# of data source queries to execute at a time")
    .int()

  private val mapper by di.instance<ObjectMapper>()
  private val props by di.instance<Properties>()
  private val db by di.instance<StockDatabase>()

  // NOTE: Must be updated every time a new indicator class is added
  private val INDICATOR_MODEL_CLASSES = emptyMap<String, Class<IsIndicatorSample>>()
    .plus(Pair(PriceSample.INDICATOR_ID, PriceSample::class.java))
    .plus(Pair(SentimentSample.INDICATOR_ID, SentimentSample::class.java))

  override fun run() {
    val config = loadConfig()

    println("Running With Config $config")

    config.queries.forEach { indicatorSpec ->
      // Run indicator fetcher
      val fetchProcesses = indicatorSpec.batches(batchSize ?: 0).map { getIndicatorFetcher(it)!! }
      val outputStream = fetchProcesses.map { runProcessSync(it) }.asSequence().flatten()

      // Parse each output line
      outputStream.forEach { output ->
        println("Output: $output")

        val json = mapper.readTree(output)
        val errors = LinkedList<String>()
        val rawSamples = json.elements().asSequence().mapNotNull {
          if (it.isTextual) {
            errors.push(it.asText())
            null
          } else {
            mapper.treeToValue(it, INDICATOR_MODEL_CLASSES[indicatorSpec.indicatorID])
          }
        }.toList()
        // Create an indicator sample
        val samples = rawSamples.map { it.asSample() }

        println("Samples: $samples")

        // Insert sample into the DB
        samples.forEach { sample ->
          db.indicatorSampleQueries.insert(
            sample.sample_id,
            sample.stock_id,
            sample.indicator_id,
            sample.fetch_source_id,
            sample.start_time,
            sample.end_time,
            sample.indicator_value
          )
        }
      }
    }
  }

  private fun loadConfig(): IndicatorQueryConfig {
    return mapper.readValue(configFile, IndicatorQueryConfig::class.java)
  }

  private fun getIndicatorFetcher(query: IndicatorQuery): ProcessBuilder? {
    val indicator = query.indicatorID

    // Get command template from config file
    val indicatorsMap = Paths.get("..").resolve(props["indicators.map.path"] as String)
    val indicatorsJson = Files.readString(indicatorsMap)
    val json = mapper.readTree(indicatorsJson)
    val fetchers = json[indicator].elements()
    // Select first fetcher for specified indicator in the config file
    val fetcher = fetchers.next()
    val cmdTemplate = fetcher["command"].asText()
    // Instantiate $variables from fetchSpec arguments
    val cmd = query.arguments.entries.fold(cmdTemplate) { acc, arg ->
      // XXX: special case :/
      if(arg.key == "tickers") {
        val tickers = if((arg.value as List<*>).any { it =="*" }) {
            listAllTickers().map { it.first }
          } else {
            arg.value as List<String>
          }
        acc.replace("\$${arg.key}", showListArg(tickers))
      } else {
        acc.replace("\$${arg.key}", arg.value.toString())
      }
    }

    println("Data Source query command: $cmd")

    return ProcessBuilder(cmd.split(" "))
      .directory(Paths.get("..").toFile())
      .redirectOutput(ProcessBuilder.Redirect.PIPE)
      .redirectError(ProcessBuilder.Redirect.to(Paths.get("error.log").toFile()))
  }

  private fun showListArg(xs: List<Any>): String {
    return xs.fold(StringBuilder()) { acc, x ->
      if(acc.isEmpty()) {
        acc.append(x.toString())
      } else {
        acc.append(",").append(x.toString())
      }
    }.toString()
  }

  private fun listAllTickers(): List<Pair<String, String>> {
    val jsonText = Files.readString(Paths.get("../data/stock_list_nyse.json"))
    val jsonNode = mapper.readTree(jsonText)
    return jsonNode.asIterable()
      .map { Pair(it["ACT Symbol"].asText(), it["Company Name"].asText()) }
      .toList()
  }
}

fun runProcessSync(processBuilder: ProcessBuilder): Sequence<String> {
  val proc = processBuilder.start()
  return proc.inputStream.bufferedReader().lines().asSequence()
}
