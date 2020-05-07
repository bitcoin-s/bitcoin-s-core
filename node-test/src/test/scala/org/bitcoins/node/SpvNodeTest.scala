package org.bitcoins.node

import com.opentable.db.postgres.embedded.EmbeddedPostgres
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{NodeTestUtil, NodeUnitTest}
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Try

class SpvNodeTest extends NodeUnitTest {

  private val dbname = "nodedb"
  private val username = "postgres"

  val pg = EmbeddedPostgres.start()

  before {
    execute(s"CREATE DATABASE $dbname")
  }

  after {
    execute(s"DROP DATABASE $dbname")
  }

  override def afterAll(): Unit = {
    Try(pg.close())
    super.afterAll()
  }

  /** Wallet config with data directory set to user temp directory */
  implicit override protected def config: BitcoinSAppConfig = {
    val overrideConf = ConfigFactory.parseString {
      s"""
         |bitcoin-s {
         |  wallet {
         |    db {
         |      name = $dbname
         |      url = "${pg.getJdbcUrl(username, dbname)}"
         |      driver = "org.postgresql.Driver"
         |      username = $username
         |      password = ""
         |    }
         |  }
         |}
      """.stripMargin
    }

    BitcoinSTestAppConfig.getSpvTestConfig(overrideConf)
  }

  override type FixtureParam = SpvNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeConnectedToBitcoind(test)

  behavior of "SpvNode"

  it must "receive notification that a block occurred on the p2p network" in {
    spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind =>
      val spvNode = spvNodeConnectedWithBitcoind.node
      val bitcoind = spvNodeConnectedWithBitcoind.bitcoind

      val assert1F = for {
        _ <- spvNode.isConnected.map(assert(_))
        a2 <- spvNode.isInitialized.map(assert(_))
      } yield a2

      val hashF: Future[DoubleSha256DigestBE] = bitcoind.getNewAddress
        .flatMap(bitcoind.generateToAddress(1, _))
        .map(_.head)

      //sync our spv node expecting to get that generated hash
      val spvSyncF = for {
        _ <- assert1F
        _ <- hashF
        sync <- spvNode.sync()
      } yield sync

      spvSyncF.flatMap { _ =>
        NodeTestUtil
          .awaitSync(spvNode, bitcoind)
          .map(_ => succeed)
      }
  }

  it must "stay in sync with a bitcoind instance" in {
    spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind =>
      val spvNode = spvNodeConnectedWithBitcoind.node
      val bitcoind = spvNodeConnectedWithBitcoind.bitcoind

      //we need to generate 1 block for bitcoind to consider
      //itself out of IBD. bitcoind will not sendheaders
      //when it believes itself, or it's peer is in IBD
      val gen1F =
        bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(1, _))

      //this needs to be called to get our peer to send us headers
      //as they happen with the 'sendheaders' message
      //both our spv node and our bitcoind node _should_ both be at the genesis block (regtest)
      //at this point so no actual syncing is happening
      val initSyncF = gen1F.flatMap { hashes =>
        val syncF = spvNode.sync()
        for {
          _ <- syncF
          _ <- NodeTestUtil.awaitBestHash(hashes.head, spvNode)
        } yield ()
      }

      //start generating a block every 10 seconds with bitcoind
      //this should result in 5 blocks
      val startGenF = initSyncF.map { _ =>
        //generate a block every 5 seconds
        //until we have generated 5 total blocks
        genBlockInterval(bitcoind)
      }

      startGenF.flatMap { _ =>
        //we should expect 5 headers have been announced to us via
        //the send headers message.
        val has6BlocksF = RpcUtil.retryUntilSatisfiedF(
          conditionF =
            () => spvNode.chainApiFromDb().flatMap(_.getBlockCount.map(_ == 6)),
          duration = 250.millis)

        has6BlocksF.map(_ => succeed)
      }
  }

  private def execute(sql: String) = {
    println(sql)
    val conn = pg.getPostgresDatabase.getConnection
    try {
      val st = conn.createStatement()
      try {
        st.execute(sql)
      } finally st.close()

    } finally conn.close()
  }

}
