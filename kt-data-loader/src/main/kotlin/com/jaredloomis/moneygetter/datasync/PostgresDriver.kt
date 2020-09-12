package com.jaredloomis.moneygetter.datasync

import com.squareup.sqldelight.sqlite.driver.asJdbcDriver
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.kodein.di.*
import java.util.*

val postgresModule = DI.Module("Postgres") {
  bind<StockDatabase>() with singleton {
    val props = instance<Properties>()
    val datasourceConfig = HikariConfig()
    datasourceConfig.jdbcUrl = props[ConfigProperties.POSTGRES_URL] as String
    val dataSource = HikariDataSource(datasourceConfig)
    val driver = dataSource.asJdbcDriver()
    StockDatabase(driver)
  }
}
