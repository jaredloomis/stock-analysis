package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.DI
import org.kodein.di.bind
import org.kodein.di.singleton
import java.time.format.DateTimeFormatter


val di = DI {
  import(configPropertiesModule)
  import(postgresModule)

  bind<ObjectMapper>() with singleton {
    val mapper = ObjectMapper()
    mapper
  }

  bind<DateTimeFormatter>() with singleton {
    DateTimeFormatter.ISO_INSTANT
  }
}