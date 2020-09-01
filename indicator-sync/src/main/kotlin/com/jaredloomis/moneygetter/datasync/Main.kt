package com.jaredloomis.moneygetter.datasync

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.options.required
import org.kodein.di.*

fun main(args: Array<String>) {
  CLIRunner().main(args)
}

class CLIRunner : CliktCommand() {
  private val indicator: String by option(help = "Indicator ID(s)").required()

  override fun run() {
    println("Indicator: $indicator")
    val db by di.instance<StockDatabase>()
    //db.stockQueries.insert(123, "AAPL", "Apple")
    println(db.stockQueries.list().executeAsList())
  }
}