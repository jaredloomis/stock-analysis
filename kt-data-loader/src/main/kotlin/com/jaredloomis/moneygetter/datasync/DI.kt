package com.jaredloomis.moneygetter.datasync

import com.fasterxml.jackson.databind.ObjectMapper
import org.kodein.di.DI
import org.kodein.di.bind
import org.kodein.di.singleton


val di = DI {
  import(configPropertiesModule)
  import(postgresModule)

  bind<ObjectMapper>() with singleton {
    val mapper = ObjectMapper()
    //val module = SimpleModule()
    //module.addDeserializer(IndicatorSpec::class.java, IndicatorSpecDeserializer())
    //mapper.registerModule(module)
    mapper
  }
}