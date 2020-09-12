package com.jaredloomis.moneygetter.datasync

interface IsIndicatorSample {
  fun asSample(): IndicatorSample
  fun getIndicatorID(): String
}
