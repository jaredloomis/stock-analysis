package com.jaredloomis.moneygetter.datasync

import org.kodein.di.DI
import org.kodein.di.bind
import org.kodein.di.instance
import org.kodein.di.singleton
import java.sql.*
import java.time.Instant
import java.util.*

interface QueryManager {
  fun <A> withQueryResults(queryName: String, args: List<Any>, f: (ResultSet) -> A): A
  fun close()
}
/*
interface QueryModelSerializer<T: QueryModel<T>> {
  fun dummy(): T
  fun fromResults(results: ResultSet): T
}

fun <A, T: QueryModel<T>> QueryManager.withQueryResultsModel(queryName: String, args: List<Any>, f: (T) -> A): A {
  return withQueryResults(queryName, args, { f(fromResults()) })
}*/

class PostgresQueryManager(val sourcePath: String, url: String, username: String, password: String): QueryManager {
  val db: Connection = DriverManager.getConnection(url, username, password)

  override fun <A> withQueryResults(queryName: String, args: List<Any>, f: (ResultSet) -> A): A {
    // Load query text
    val queryText = this::class.java.classLoader.getResource("$sourcePath/$queryName.sql")!!.readText()
    // Create & execute query
    val stmt = db.prepareStatement(queryText)
    args.forEachIndexed { i0, arg ->
      val i = i0 + 1
      when(arg) {
        is String  -> stmt.setString(i, arg)
        is Int     -> stmt.setInt(i, arg)
        is Double  -> stmt.setDouble(i, arg)
        is Float   -> stmt.setFloat(i, arg)
        is Instant -> stmt.setTimestamp(i, Timestamp(arg.toEpochMilli()))
        else       -> throw Exception("Didn't recognize arg $arg")
      }
    }
    println(stmt)
    val results = stmt.executeQuery()
    val ret = f(results)
    // Cleanup
    if(!results.isClosed) {
      results.close()
    }
    stmt.close()
    return ret
  }

  override fun close() {
    db.close()
  }
}

val queryManagerModule = DI.Module("QueryManager") {
  bind<QueryManager>() with singleton {
    val props = instance<Properties>()
    val url = props[ConfigProperties.POSTGRES_URL] as String
    PostgresQueryManager("queries/postgres", url, "postgres", "password")
  }
}
