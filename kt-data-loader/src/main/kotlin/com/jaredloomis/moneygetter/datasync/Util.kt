package com.jaredloomis.moneygetter.datasync

import java.lang.StringBuilder

fun showTickers(tickers: List<String>): String {
  return tickers.foldIndexed(StringBuilder()) { i, acc, ticker ->
    if(i != tickers.size-1) {
      acc.append(ticker).append(",")
    } else {
      acc.append(ticker)
    }
  }.toString()
}