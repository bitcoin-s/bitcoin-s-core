package org.bitcoins.dlc.oracle.storage

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.DLCOracleAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.DLCOracleDAOFixture

class EventDAOTest extends DLCOracleDAOFixture {

  implicit override protected def config: DLCOracleAppConfig =
    BitcoinSTestAppConfig.getDLCOracleWithEmbeddedDbTestConfig(pgUrl)

  behavior of "EventDAO"

  val ecKey: ECPublicKey = ECPublicKey.freshPublicKey
  val publicKey: SchnorrPublicKey = ecKey.schnorrPublicKey
  val nonce: SchnorrNonce = ecKey.schnorrNonce

  val eventName = "dummy"
  val sigVersion: SigningVersion = SigningVersion.latest
  val message = "dummy message"

  val time: Instant = {
    // Need to do this so it is comparable to the db representation
    val now = TimeUtil.now.getEpochSecond
    Instant.ofEpochSecond(now)
  }

  val dummyRValDb: RValueDb = RValueDb(
    nonce,
    eventName,
    HDPurpose(0),
    HDCoinType.Bitcoin,
    0,
    0,
    0,
    SchnorrDigitalSignature(nonce, FieldElement.one))

  it must "create an EventDb and read it" in { daos =>
    val rValDAO = daos.rValueDAO
    val eventDAO = daos.eventDAO

    val eventDb =
      EventDb(nonce, publicKey, eventName, 1, sigVersion, time, None)

    for {
      _ <- rValDAO.create(dummyRValDb)
      _ <- eventDAO.create(eventDb)
      read <- eventDAO.read(nonce)
    } yield assert(read.contains(eventDb))
  }

  it must "create an EventDb and find all" in { daos =>
    val rValDAO = daos.rValueDAO
    val eventDAO = daos.eventDAO

    val eventDb =
      EventDb(nonce, publicKey, eventName, 1, sigVersion, time, None)

    for {
      _ <- rValDAO.create(dummyRValDb)
      _ <- eventDAO.create(eventDb)
      all <- eventDAO.findAll()
    } yield assert(all.contains(eventDb))
  }
}
