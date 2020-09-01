package com.jaredloomis.moneygetter.datasync

import org.kodein.di.*

val di = DI {
  import(configPropertiesModule)
  import(postgresModule)
}