package org.bitcoins.testkit.fixtures

import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.scalatest._

import scala.concurrent.Future

case class DLCOracleDAOs(
    rValueDAO: RValueDAO,
    eventDAO: EventDAO,
    outcomeDAO: EventOutcomeDAO)

trait DLCOracleDAOFixture extends BitcoinSFixture with EmbeddedPg {

  implicit protected def config: DLCOracleAppConfig =
    BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = DLCOracleDAOs

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(
      build = () => {
        config
          .initialize()
          .map(oracle =>
            DLCOracleDAOs(oracle.rValueDAO,
                          oracle.eventDAO,
                          oracle.eventOutcomeDAO))
      },
      destroy = () => destroyAppConfig(config)
    )(test)
  }

  private def destroyAppConfig(conf: DLCOracleAppConfig): Future[Unit] = {
    for {
      _ <- conf.dropAll()
      _ <- conf.stop()
    } yield ()
  }
}
