package org.bitcoins.db

import com.typesafe.config._
import org.bitcoins.commons.config._
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import java.nio.file.{Path, Paths}
import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success, Try}

abstract class DbAppConfig extends AppConfig {

  /** Releases the thread pool associated with this AppConfig's DB */
  override def stop(): Future[Unit] = {
    Future.successful(slickDbConfig.db.close())
  }

  lazy val dbUsername: String =
    config.getString(s"bitcoin-s.$moduleName.db.user")

  lazy val dbPassword: String =
    config.getString(s"bitcoin-s.$moduleName.db.password")

  lazy val driver: DatabaseDriver = {
    val driverStr =
      getConfigString(s"bitcoin-s.$moduleName.db.driver").toLowerCase
    if (driverStr.contains("sqlite")) {
      SQLite
    } else if (driverStr.contains("postgres")) {
      PostgreSQL
    } else {
      sys.error(s"Could not find a DatabaseDriver for string=$driverStr")
    }
  }

  lazy val jdbcUrl: String = {
    driver match {
      case SQLite =>
        s""""jdbc:sqlite:"${AppConfig.safePathToString(dbPath)}/$dbName"""
      case PostgreSQL =>
        s""""jdbc:postgresql://$dbHost:$dbPort/$dbName""""
    }
  }

  /** The path where our DB is located */
  lazy val dbPath: Path = {
    val pathStrOpt =
      getConfigStringOpt(s"bitcoin-s.$moduleName.db.path")
    pathStrOpt match {
      case Some(pathStr) =>
        Paths.get(pathStr)
      case None =>
        sys.error(s"Could not find dbPath for $moduleName.db.path")
    }
  }

  /** The name of our database */
  lazy val dbName: String = {
    val name = getConfigString(s"bitcoin-s.$moduleName.db.name")

    driver match {
      case SQLite     => s"$name.sqlite"
      case PostgreSQL => name
    }
  }

  /** The host of our postgresql database */
  lazy val dbHost: String = {
    val hostOpt = getConfigStringOpt(s"bitcoin-s.$moduleName.db.host")

    hostOpt match {
      case Some(host) => host
      case None       => "localhost"
    }
  }

  /** The port number of our postgresql database */
  lazy val dbPort: Int = {
    val portOpt = config.getIntOpt(s"bitcoin-s.$moduleName.db.port")

    portOpt match {
      case Some(port) => port
      case None       => 5432 // default postgres port
    }
  }

  lazy val schemaName: Option[String] = driver match {
    case PostgreSQL =>
      Some(moduleName)
    case SQLite =>
      None
  }

  lazy val slickDbConfig: DatabaseConfig[JdbcProfile] = {
    // Create overrides if modules want to change their path or db name
    val overrideConf = ConfigFactory.parseString {
      s"""
         |bitcoin-s {
         |  $moduleName {
         |     db {
         |        path = ${AppConfig.safePathToString(dbPath)}
         |        name = $dbName
         |        user = "$dbUsername"
         |        password = "$dbPassword"
         |        url = $jdbcUrl
         |     }
         |  }
         |}
      """.stripMargin
    }

    val usedConf = overrideConf.withFallback(config)
    Try {
      val c = DatabaseConfig.forConfig[JdbcProfile](path =
                                                      s"bitcoin-s.$moduleName",
                                                    config = usedConf)

      logger.trace(s"Resolved DB config: ${ConfigOps(c.config).asReadableJson}")
      c
    } match {
      case Success(value) =>
        value
      case Failure(exception) =>
        logger.error(s"Error when loading database from config: $exception")
        logger.error(s"Configuration: ${usedConf.asReadableJson}")
        throw exception
    }
  }

  lazy val isHikariLoggingEnabled: Boolean = {
    val hikariLoggingOpt =
      config.getBooleanOpt(s"bitcoin-s.$moduleName.hikari-logging")
    hikariLoggingOpt match {
      case Some(bool) => bool
      case None       =>
        // default hikari logging off
        false
    }
  }

  /** Gets how often we should log hikari connection pool stats if None, this
    * means [[isHikariLoggingEnabled]] is not enabled
    */
  lazy val hikariLoggingInterval: Option[Duration] = {
    if (isHikariLoggingEnabled) {
      val intervalOpt =
        config.getDurationOpt(s"bitcoin-s.$moduleName.hikari-logging-interval")
      val interval = intervalOpt match {
        case Some(interval) => interval
        case None           =>
          // default to 1 minute if nothing is set
          new FiniteDuration(1, TimeUnit.MINUTES)
      }
      Some(interval)
    } else {
      None
    }

  }
}
