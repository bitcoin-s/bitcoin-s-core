package org.bitcoins.testkit.wallet

import com.typesafe.config.ConfigFactory
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models.{
  ContractOraclePair,
  SingleContractInfo
}
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.FutureOutcome

trait DualDLCWalletTestCachedBitcoind
    extends BitcoinSWalletTestCachedBitcoindNewest {
  import BitcoinSWalletTest._

  implicit protected def config2: BitcoinSAppConfig = {
    val config = BitcoinSWalletTest.buildBip39PasswordWithExtraConfig(
      getBIP39PasswordOpt(),
      // needed until DLC spec supports non segwit v0 outputs
      Some(BaseWalletTest.segwitWalletConf)
    )
    val randomHex = CryptoUtil.randomBytes(3).toHex
    // with postgres, we need unique wallet names as postgres wallets
    // share the same database. They have a unique schema with the database
    // based on wallet name which is why we set this here.
    val walletNameConfig =
      ConfigFactory.parseString(s"bitcoin-s.wallet.walletName=$randomHex")
    val extraConfig = config.withFallback(walletNameConfig)
    BaseWalletTest.getFreshConfig(() => pgUrl(), Vector(extraConfig))
  }

  implicit protected def wallet2AppConfig: WalletAppConfig = {
    config2.walletConf
  }

  implicit protected def dlc2AppConfig: DLCAppConfig = {
    config2.dlcConf
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.walletConf)
    AppConfig.throwIfDefaultDatadir(config2.walletConf)
    AppConfig.throwIfDefaultDatadir(getFreshConfig.dlcConf)
    AppConfig.throwIfDefaultDatadir(config2.dlcConf)
    super.beforeAll()
  }

  /** Creates two segwit wallets that are funded with some bitcoin, these
    * wallets are NOT peered with a bitcoind so the funds in the wallets are not
    * tied to an underlying blockchain
    */
  def withDualFundedDLCWallets(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        for {
          bitcoind <- cachedBitcoindWithFundsF
          walletA <-
            FundWalletUtil.createFundedDLCWalletWithBitcoind(bitcoind)
          walletB <- FundWalletUtil.createFundedDLCWalletWithBitcoind(
            bitcoind
          )(config2, system)
        } yield (walletA, walletB, bitcoind),
      destroy = { (fundedWallets: (FundedDLCWallet, FundedDLCWallet, ?)) =>
        for {
          _ <- destroyDLCWallet(fundedWallets._1.wallet)
          _ <- destroyDLCWallet(fundedWallets._2.wallet)
        } yield ()
      }
    )(test)
  }

  /** Creates 2 funded segwit wallets that have a DLC initiated */
  def withDualDLCWallets(
      test: OneArgAsyncTest,
      contractOraclePair: ContractOraclePair
  ): FutureOutcome = {
    makeDependentFixture(
      build = () => {
        val bitcoindF = cachedBitcoindWithFundsF

        val walletAF = bitcoindF.flatMap { bitcoind =>
          FundWalletUtil.createFundedDLCWalletWithBitcoind(bitcoind)
        }
        val walletBF = for {
          bitcoind <- bitcoindF

          // its important to map on this otherwise we generate blocks in parallel
          // causing a reorg inside of createFundedDLCWallet
          _ <- walletAF
          walletB <- FundWalletUtil.createFundedDLCWalletWithBitcoind(
            bitcoind
          )(config2, system)
        } yield { walletB }

        for {
          walletA <- walletAF
          walletB <- walletBF
          amt = expectedDefaultAmt / Satoshis(2)
          contractInfo = SingleContractInfo(amt.satoshis, contractOraclePair)
          (dlcWalletA, dlcWalletB) <-
            DLCWalletUtil.initDLC(walletA, walletB, contractInfo)
          bitcoind <- bitcoindF
        } yield (dlcWalletA, dlcWalletB, bitcoind)
      },
      destroy = {
        (dlcWallets: (InitializedDLCWallet, InitializedDLCWallet, ?)) =>
          for {
            _ <- destroyDLCWallet(dlcWallets._1.wallet)
            _ <- destroyDLCWallet(dlcWallets._2.wallet)
          } yield ()
      }
    )(test)
  }
}
