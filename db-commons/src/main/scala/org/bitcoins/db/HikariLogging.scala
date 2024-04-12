package org.bitcoins.db

import com.codahale.metrics.{Histogram, MetricRegistry}
import com.zaxxer.hikari.{HikariDataSource, HikariPoolMXBean}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.util._
import slick.jdbc.JdbcDataSource
import slick.jdbc.hikaricp.HikariCPJdbcDataSource
import slick.util.AsyncExecutorMXBean

import java.lang.management.ManagementFactory
import java.util.concurrent.{Executors, ScheduledFuture, TimeUnit}
import javax.management.{JMX, ObjectName}
import scala.concurrent.duration._

case class HikariLogging(
    hikariDataSource: HikariDataSource,
    moduleName: String,
    interval: Duration
) extends BitcoinSLogger
    with StartStop[HikariLogging] {

  /** Logs thread activity */
  private case class HikariActivityUpdate(
      active: Int,
      idle: Int,
      waiting: Int,
      total: Int,
      maxThreads: Int,
      activeThreads: Int,
      maxQueueSize: Int,
      queueSize: Int
  ) {

    override def toString: String = {
      s"""
         | "${moduleName}-activity-update" : { 
         |  "active" : ${active},
         |  "idle" : ${idle},
         |  "waiting" : ${waiting},
         |  "total" : ${total},
         |  "maxThreads" : ${maxThreads},
         |  "activeThreads" : ${activeThreads},
         |  "maxQueueSize" : ${maxQueueSize},
         |  "queueSize" : ${queueSize}
         |}
         |""".stripMargin.replaceAll("\\s", "")
    }
  }

  /** From the docs:
    * How long each connection is used before being returned to the pool. This is the "out of pool" or "in-use" time.
    * @see https://github.com/brettwooldridge/HikariCP/wiki/Dropwizard-Metrics
    */
  private case class HikariPoolUsageUpdate(
      `75thPercentile`: Double,
      `95thPercentile`: Double,
      `98thPercentile`: Double,
      `99thPercentile`: Double,
      `999thPercentile`: Double,
      max: Double,
      min: Double,
      median: Double,
      mean: Double
  ) {

    override def toString: String = {
      s"""
         |"${moduleName}-pool-usage" : { 
         |  "max" : ${max},
         |  "min" : ${min},
         |  "median" : ${median},
         |  "mean" : ${mean},
         |  "75thPercentile" : ${`75thPercentile`},
         |  "95thPercentile" : ${`95thPercentile`},
         |  "98thPercentile" : ${`98thPercentile`},
         |  "99thPercentile" : ${`99thPercentile`},
         |  "999thPercentile" : ${`999thPercentile`}
         |}
         |""".stripMargin.replaceAll("\\s", "")
    }
  }

  //this is needed to get the 'AsyncExecutor' bean below to register properly
  //dbConfig.database.ioExecutionContext

  private lazy val poolName = hikariDataSource.getPoolName
  private lazy val mBeanServer = ManagementFactory.getPlatformMBeanServer

  lazy val aeBeanName = new ObjectName(
    s"slick:type=AsyncExecutor,name=$poolName")

  lazy val poolBeanName = new ObjectName(
    s"com.zaxxer.hikari:type=Pool ($poolName)")

  lazy val poolConfigBeanName = new ObjectName(
    s"com.zaxxer.hikari:type=PoolConfig ($poolName)"
  )

  /** MBean uses random string incantations for
    * accessing attributes :-(
    *
    * @see [[https://github.com/brettwooldridge/HikariCP/wiki/MBean-(JMX)-Monitoring-and-Management#programmatic-access HikariCP docs]]
    */
  private lazy val objectName = new ObjectName(
    s"com.zaxxer.hikari:type=Pool ($poolName)"
  )

  /** @see https://github.com/brettwooldridge/HikariCP/wiki/MBean-(JMX)-Monitoring-and-Management
    */
  private lazy val hikariMxBean =
    JMX.newMXBeanProxy(mBeanServer, objectName, classOf[HikariPoolMXBean])

  /** @see http://slick.lightbend.com/doc/3.3.0/config.html#monitoring
    */
  private lazy val slickMxBean =
    JMX.newMXBeanProxy(mBeanServer, aeBeanName, classOf[AsyncExecutorMXBean])

  // https://github.com/brettwooldridge/HikariCP/wiki/Dropwizard-Metrics#pool-namepoolusage
  private lazy val poolUsageMetricName = s"$poolName.pool.Usage"

  private lazy val metricRegistry: MetricRegistry = Option(
    hikariDataSource.getMetricRegistry
  ) match {
    case Some(registry: MetricRegistry) =>
      registry
    case Some(other: AnyRef) =>
      val msg = s"Could not load metric registry, got $other"
      logger.error(msg)
      throw new RuntimeException(msg)
    case None =>
      val msg = "Could not load metric registry, got null!"
      logger.error(msg)
      throw new RuntimeException(msg)
  }

  private val logHikariStats: Runnable = () => {

    val usageHistogram: Histogram =
      metricRegistry.getHistograms().get(poolUsageMetricName)
    val usageSnapshot = usageHistogram.getSnapshot()

    val poolUsageUpdate = HikariPoolUsageUpdate(
      `75thPercentile` = usageSnapshot.get75thPercentile(),
      `95thPercentile` = usageSnapshot.get95thPercentile(),
      `98thPercentile` = usageSnapshot.get98thPercentile(),
      `99thPercentile` = usageSnapshot.get99thPercentile(),
      `999thPercentile` = usageSnapshot.get999thPercentile(),
      max = usageSnapshot.getMax().toDouble,
      min = usageSnapshot.getMin().toDouble,
      median = usageSnapshot.getMedian(),
      mean = usageSnapshot.getMean()
    )

    val activityUpdate = HikariActivityUpdate(
      active = hikariMxBean.getActiveConnections,
      idle = hikariMxBean.getIdleConnections,
      waiting = hikariMxBean.getThreadsAwaitingConnection,
      total = hikariMxBean.getTotalConnections,
      maxThreads = slickMxBean.getMaxThreads,
      activeThreads = slickMxBean.getActiveThreads,
      maxQueueSize = slickMxBean.getMaxQueueSize,
      queueSize = slickMxBean.getQueueSize
    )

    logger.info(poolUsageUpdate.toString)
    logger.info(activityUpdate.toString)
  }

  private var started: Boolean = false
  private var cancelOpt: Option[ScheduledFuture[?]] = None

  override def start(): HikariLogging = {
    if (!started) {
      val metricRegistry = new MetricRegistry

      mBeanServer.getMBeanInfo(aeBeanName)
      mBeanServer.getMBeanInfo(poolBeanName)
      mBeanServer.getMBeanInfo(poolConfigBeanName)

      hikariDataSource.setMetricRegistry(metricRegistry)
      val future = HikariLogging.scheduler.scheduleAtFixedRate(
        logHikariStats,
        interval.toMillis,
        interval.toMillis,
        TimeUnit.MILLISECONDS)
      cancelOpt = Some(future)
      started = true
      this
    } else {
      this
    }
  }

  override def stop(): HikariLogging = {
    cancelOpt match {
      case Some(cancel) =>
        if (!cancel.isCancelled) {
          val _: Boolean = cancel.cancel(true)
          this
        } else {
          cancelOpt = None
          this
        }
      case None =>
        this
    }
  }
}

object HikariLogging extends BitcoinSLogger {
  private[db] val scheduler = Executors.newScheduledThreadPool(1)

  /** Returns a started hikari logger if configuration is correct, else None
    * @param jdbcProfileComponent the database component we are logging for
    * @param interval how often the hikari logs should be output
    */
  def fromJdbcProfileComponent[T <: DbAppConfig](
      jdbcProfileComponent: JdbcProfileComponent[T],
      interval: Duration): Option[HikariLogging] = {
    val dataSource = jdbcProfileComponent.database.source
    val moduleName = jdbcProfileComponent.appConfig.moduleName
    dataSource match {
      case hikariSource: HikariCPJdbcDataSource =>
        val started = HikariLogging(hikariSource.ds, moduleName, interval)
          .start()
        Some(started)
      case _: JdbcDataSource =>
        val err = {
          s"JdbcProfile Component is not a Hikari source=${jdbcProfileComponent.dbConfig.profile}"
        }
        logger.error(err)
        None
    }
  }
}
