package com.jaredloomis.moneygetter.datasync

import org.kodein.di.*
import java.io.FileNotFoundException
import java.util.*

class ConfigProperties {
  companion object {
    val POSTGRES_URL = "db.postgres.url"
    val POSTGRES_USERNAME = "db.postgres.username"
    val POSTGRES_PASSWORD = "db.postgres.password"
  }

  private val propFileName = "config.properties"

  fun loadConfigProperties(): Properties {
    val props = Properties()

    val inputStream = javaClass.classLoader.getResourceAsStream(propFileName)
    if (inputStream != null) {
      props.load(inputStream)
    } else {
      throw FileNotFoundException("property file '$propFileName' not found in the classpath")
    }

    return props
  }
}

val configPropertiesModule = DI.Module("ConfigProperties") {
  bind<Properties>() with singleton { ConfigProperties().loadConfigProperties() }
}
