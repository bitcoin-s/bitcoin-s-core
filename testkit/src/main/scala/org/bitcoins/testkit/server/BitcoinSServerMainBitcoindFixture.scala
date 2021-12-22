package org.bitcoins.testkit.server

import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.server.BitcoinSServerMain
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.bitcoins.testkit.wallet.{
  FundWalletUtil,
  WalletTestUtil,
  WalletWithBitcoindRpc
}
import org.scalatest.FutureOutcome

import scala.concurrent.Future

/** Starts an instnace of [[BitcoinSserverMain]] that is
  * using bitcoind as a backend
  */
trait BitcoinSServerMainBitcoindFixture
    extends BitcoinSFixture
    with EmbeddedPg
    with CachedBitcoindNewest {

  override type FixtureParam = ServerWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[ServerWithBitcoind] = () => {
      println(s"START SERVERWITHBITCOIND")
      for {
        bitcoind <- cachedBitcoindWithFundsF
        _ = println(s"1 SERVERWITHBITCOIND")
        config = BitcoinSServerMainUtil.buildBitcoindBitcoinSAppConfig(bitcoind)
        server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)
        _ <- server.start()
        _ = println(s"2 SERVERWITHBITCOIND")
        //need to create account 2 to use FundWalletUtil.fundWalletWithBitcoind
        wallet <- server.walletConf.createHDWallet(bitcoind, bitcoind, bitcoind)
        _ <- wallet.start()
        account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)

        //needed for fundWalletWithBitcoind
        _ <- wallet.createNewAccount(hdAccount = account1,
                                     kmParams = wallet.keyManager.kmParams)

        _ <- FundWalletUtil.fundWalletWithBitcoind(
          WalletWithBitcoindRpc(wallet, bitcoind))
      } yield {
        println(s"done setup SERVERWITHBITCOIND")
        ServerWithBitcoind(bitcoind, server)
      }
    }

    val destroy: ServerWithBitcoind => Future[Unit] = { serverWithBitcoind =>
      val stopF = serverWithBitcoind.server.stop()
      for {
        _ <- stopF
        _ = println(s"SERVERWITHBITCOIND")
        _ <- BitcoinSServerMainUtil
          .destroyBitcoinSAppConfig(serverWithBitcoind.server.conf)
      } yield {
        println(s"SERVERWITHBITCOIND DESTROY APPCONFIG")
        ()
      }
    }

    makeDependentFixture(builder, destroy)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[EmbeddedPg].afterAll()
    super[BitcoinSFixture].afterAll()
  }
}
