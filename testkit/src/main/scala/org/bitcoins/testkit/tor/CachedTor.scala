package org.bitcoins.testkit.tor

import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.{BitcoinSPekkoAsyncTest, TorUtil}
import org.bitcoins.tor.config.TorAppConfig

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/** A trait that holds a cached instance of a the tor daemon This is useful for
  * using with fixtures to avoid starting tor everytime a new test is run.
  */
trait CachedTor {
  self: BitcoinSPekkoAsyncTest =>

  implicit protected lazy val torConfig: TorAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig().torConf

  protected val isTorStarted: AtomicBoolean = new AtomicBoolean(false)

  protected lazy val torF: Future[Unit] = {
    val _ = isTorStarted.set(true)

    torConfig.start()
  }

  override def beforeAll(): Unit = {
    if (TorUtil.torEnabled && !isTorStarted.get()) {
      Await.result(torF, 70.seconds)
    }
    ()
  }
}
