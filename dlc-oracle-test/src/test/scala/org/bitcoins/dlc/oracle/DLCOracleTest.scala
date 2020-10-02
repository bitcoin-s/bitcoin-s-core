package org.bitcoins.dlc.oracle

import java.sql.SQLException

import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.testkit.BitcoinSTestAppConfig.tmpDir
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

import scala.concurrent.Future

class DLCOracleTest extends BitcoinSFixture {

  override type FixtureParam = DLCOracle

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[DLCOracle] = () => {
      val conf = DLCOracleAppConfig(tmpDir())
      conf.initialize(AesPassword.fromString("bad"), None)
    }

    val destroy: DLCOracle => Future[Unit] = dlcOracle => {
      val conf = dlcOracle.conf
      conf.dropAll().flatMap { _ =>
        FileUtil.deleteTmpDir(conf.baseDatadir)
        conf.stop()
      }
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }

  val testOutcomes: Vector[String] = (0 to 10).map(_.toString).toVector

  behavior of "DLCOracle"

  it must "correctly initialize" in { dlcOracle: DLCOracle =>
    assert(dlcOracle.conf.seedExists())
  }

  it must "start with no events" in { dlcOracle: DLCOracle =>
    dlcOracle.listEventDbs().map { events =>
      assert(events.isEmpty)
    }
  }

  it must "start with no pending events" in { dlcOracle: DLCOracle =>
    dlcOracle.listPendingEventDbs().map { events =>
      assert(events.isEmpty)
    }
  }

  it must "create a new event and list it with pending" in {
    dlcOracle: DLCOracle =>
      val time = TimeUtil.now
      for {
        testEventDb <- dlcOracle.createNewEvent("test", time, testOutcomes)
        pendingEvents <- dlcOracle.listPendingEventDbs()
      } yield {
        assert(pendingEvents.size == 1)
        // encoding of the time can make them unequal
        val comparable =
          pendingEvents.head.copy(maturationTime = testEventDb.maturationTime)
        assert(comparable == testEventDb)
      }
  }

  it must "create a new event with a valid commitment signature" in {
    dlcOracle: DLCOracle =>
      for {
        testEventDb <-
          dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
        rValDbOpt <- dlcOracle.rValueDAO.read(testEventDb.nonce)
      } yield {
        assert(rValDbOpt.isDefined)
        val rValDb = rValDbOpt.get
        val hash = CryptoUtil.sha256(
          rValDb.nonce.bytes ++ CryptoUtil.serializeForHash(rValDb.eventName))
        assert(
          dlcOracle.publicKey.verify(hash.bytes, rValDb.commitmentSignature))
      }
  }

  it must "create multiple events with different names" in {
    dlcOracle: DLCOracle =>
      for {
        _ <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
        _ <- dlcOracle.createNewEvent("test1", TimeUtil.now, testOutcomes)
      } yield succeed
  }

  it must "fail to create multiple events with the same name" in {
    dlcOracle: DLCOracle =>
      recoverToSucceededIf[SQLException] {
        for {
          _ <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
          _ <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
        } yield ()
      }
  }

  it must "create and sign a event" in { dlcOracle: DLCOracle =>
    val outcome = testOutcomes.head
    for {
      eventDb <- dlcOracle.createNewEvent("test", TimeUtil.now, testOutcomes)
      signedEventDb <- dlcOracle.signEvent(eventDb.nonce, outcome)
      outcomeDbs <- dlcOracle.eventOutcomeDAO.findByNonce(eventDb.nonce)
      outcomeDb = outcomeDbs.find(_.message == outcome).get
      signedEvent <- dlcOracle.eventDAO.read(eventDb.nonce)
    } yield {
      val sig = signedEventDb.sigOpt.get

      assert(signedEvent.isDefined)
      assert(signedEvent.get.attestationOpt.contains(sig.sig))
      assert(dlcOracle.publicKey.verify(outcomeDb.hashedMessage.bytes, sig))
      assert(
        SchnorrDigitalSignature(signedEvent.get.nonce,
                                signedEvent.get.attestationOpt.get) == sig)
    }
  }

  it must "fail to sign an event that doesn't exist" in {
    dlcOracle: DLCOracle =>
      val dummyNonce = SchnorrNonce(ECPublicKey.freshPublicKey.bytes.tail)
      recoverToSucceededIf[RuntimeException](
        dlcOracle.signEvent(dummyNonce, "testOutcomes"))
  }
}
