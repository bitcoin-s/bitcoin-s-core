package org.bitcoins.testkit.wallet

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.{NeutrinoHDWalletApi, WalletApi}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.dlc.wallet.{DLCAppConfig, DLCWallet}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.{BitcoinSAppConfig, BitcoindRpcBackendUtil}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.{
  account1Amt,
  defaultAcctAmts
}
import org.bitcoins.testkit.wallet.FundWalletUtil.{
  FundedTestWallet,
  FundedWallet
}
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.bitcoins.testkitcore.util.TransactionTestUtil
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.util.WalletUtil

import scala.concurrent.{ExecutionContext, Future}

trait FundWalletUtil extends BitcoinSLogger {

  /** Funds the given wallet with money from the given bitcoind */
  def fundWalletWithBitcoind[T <: WalletWithBitcoind[? <: BitcoindRpcClient]](
      pair: T
  )(implicit ec: ExecutionContext): Future[T] = {
    val (wallet, bitcoind) = (pair.wallet, pair.bitcoind)

    val defaultAccount = pair.walletConfig.defaultAccount
    val fundedDefaultAccountWalletF =
      FundWalletUtil.fundAccountForWalletWithBitcoind(
        amts = defaultAcctAmts,
        account = defaultAccount,
        wallet = wallet,
        bitcoind = bitcoind
      )

    val hdAccount1 = WalletTestUtil.getHdAccount1(pair.walletConfig)
    val fundedAccount1WalletF = for {
      fundedDefaultAcct <- fundedDefaultAccountWalletF

      fundedAcct1 <- FundWalletUtil.fundAccountForWalletWithBitcoind(
        amts = account1Amt,
        account = hdAccount1,
        wallet = fundedDefaultAcct,
        bitcoind = bitcoind
      )
    } yield fundedAcct1

    fundedAccount1WalletF.map(_ => pair)
  }

  def fundAccountForWallet(
      amts: Vector[CurrencyUnit],
      account: HDAccount,
      wallet: WalletApi
  )(implicit ec: ExecutionContext): Future[WalletApi] = {

    val addressesF: Future[Vector[BitcoinAddress]] = Future.sequence {
      Vector.fill(3)(wallet.accountHandling.getNewAddress(account))
    }

    // construct three txs that send money to these addresses
    // these are "fictional" transactions in the sense that the
    // outpoints do not exist on a blockchain anywhere
    val txsF = for {
      addresses <- addressesF
    } yield {
      addresses.zip(amts).map { case (addr, amt) =>
        val output =
          TransactionOutput(value = amt, scriptPubKey = addr.scriptPubKey)
        val outpoint = TransactionGenerators.outPoint.sample.get
        TransactionTestUtil.buildTransactionTo(output, outPoint = outpoint)
      }
    }

    val fundedWalletF =
      txsF.flatMap(txs =>
        FutureUtil.sequentially(txs)(tx =>
          wallet.transactionProcessing.processTransaction(tx, None)))

    fundedWalletF.map(_ => wallet)
  }

  def fundAccountForWalletWithBitcoind(
      amts: Vector[CurrencyUnit],
      account: HDAccount,
      wallet: NeutrinoHDWalletApi,
      bitcoind: BitcoindRpcClient
  )(implicit ec: ExecutionContext): Future[NeutrinoHDWalletApi] = {

    val addressesF: Future[Vector[BitcoinAddress]] = Future.sequence {
      Vector.fill(3)(wallet.accountHandling.getNewAddress(account))
    }

    val txAndHashF = for {
      addresses <- addressesF
      addressAmountMap = addresses.zip(amts).toMap
      (tx, blockHash) <- fundAddressesWithBitcoind(addressAmountMap, bitcoind)
      blockHashWithConfs <- WalletUtil.getBlockHashWithConfs(bitcoind,
                                                             blockHash)
      _ <- wallet.transactionProcessing.processTransaction(tx,
                                                           blockHashWithConfs)
    } yield (tx, blockHash)

    txAndHashF.map(_ => wallet)
  }

  def fundAddressesWithBitcoind(
      addressAmountMap: Map[BitcoinAddress, CurrencyUnit],
      bitcoind: BitcoindRpcClient
  )(implicit
      ec: ExecutionContext): Future[(Transaction, DoubleSha256DigestBE)] = {

    val txAndHashF = for {
      txId <- bitcoind.sendMany(addressAmountMap)
      tx <- bitcoind.getRawTransactionRaw(txId)
      hashes <- bitcoind.generate(6)
    } yield (tx, hashes.head)

    txAndHashF
  }

  /** Funds a bitcoin-s wallet with 3 utxos with 1, 2 and 3 bitcoin in the utxos
    */
  def fundWallet(
      wallet: WalletApi,
      walletConfig: WalletAppConfig
  )(implicit ec: ExecutionContext): Future[FundedTestWallet] = {

    val defaultAccount = walletConfig.defaultAccount
    val fundedDefaultAccountWalletF = FundWalletUtil.fundAccountForWallet(
      amts = BitcoinSWalletTest.defaultAcctAmts,
      account = defaultAccount,
      wallet = wallet
    )

    val hdAccount1 = WalletTestUtil.getHdAccount1(walletConfig)
    val fundedAccount1WalletF = for {
      fundedDefaultAcct <- fundedDefaultAccountWalletF
      fundedAcct1 <- FundWalletUtil.fundAccountForWallet(
        amts = BitcoinSWalletTest.account1Amt,
        account = hdAccount1,
        fundedDefaultAcct
      )
    } yield fundedAcct1

    // sanity check to make sure we have money
    for {
      fundedWallet <- fundedAccount1WalletF
      accountHandling = fundedWallet.accountHandling
      balance <- accountHandling.getBalance(defaultAccount)
      _ = require(
        balance == BitcoinSWalletTest.expectedDefaultAmt,
        s"Funding wallet fixture failed to fund the wallet, got balance=${balance} expected=${BitcoinSWalletTest.expectedDefaultAmt}"
      )

      account1Balance <- accountHandling.getBalance(hdAccount1)
      _ = require(
        account1Balance == BitcoinSWalletTest.expectedAccount1Amt,
        s"Funding wallet fixture failed to fund account 1, " +
          s"got balance=${hdAccount1} expected=${BitcoinSWalletTest.expectedAccount1Amt}"
      )

    } yield FundedWallet(fundedWallet, walletConfig)
  }
}

object FundWalletUtil extends FundWalletUtil {

  trait FundedTestWallet {
    def wallet: WalletApi
  }

  /** This is a wallet that was two funded accounts Account 0 (default account)
    * has utxos of 1,2,3 bitcoin in it (6 btc total) Account 1 has a utxos of
    * 0.2,0.3,0.5 bitcoin in it (0.6 total)
    */
  case class FundedWallet(wallet: WalletApi, walletConfig: WalletAppConfig)
      extends FundedTestWallet

  case class FundedDLCWallet(
      wallet: DLCWallet,
      walletConfig: WalletAppConfig,
      dlcConfig: DLCAppConfig)
      extends FundedTestWallet

  /** This creates a wallet that was two funded accounts Account 0 (default
    * account) has utxos of 1,2,3 bitcoin in it (6 btc total) Account 1 has a
    * utxos of 0.2,0.3,0.5 bitcoin in it (1 btc total)
    */
  def createFundedWallet(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit
      config: WalletAppConfig,
      system: ActorSystem
  ): Future[FundedWallet] = {
    import system.dispatcher
    for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi
      )
      funded <- FundWalletUtil.fundWallet(wallet, config)
    } yield FundedWallet(funded.wallet, config)
  }

  def createFundedDLCWallet(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit
      config: BitcoinSAppConfig,
      system: ActorSystem
  ): Future[FundedDLCWallet] = {
    import system.dispatcher
    for {
      wallet <- BitcoinSWalletTest.createDLCWallet2Accounts(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi
      )
      funded <- FundWalletUtil.fundWallet(wallet, config.walletConf)
    } yield {
      FundedDLCWallet(funded.wallet.asInstanceOf[DLCWallet],
                      config.walletConf,
                      config.dlcConf)
    }
  }

  def createFundedDLCWalletWithBitcoind(bitcoind: BitcoindRpcClient)(implicit
      config: BitcoinSAppConfig,
      system: ActorSystem
  ): Future[FundedDLCWallet] = {
    import system.dispatcher
    for {
      tmp <- BitcoinSWalletTest.createDLCWallet2Accounts(
        nodeApi = bitcoind,
        chainQueryApi = bitcoind
      )
      wallet = BitcoindRpcBackendUtil.createDLCWalletWithBitcoindCallbacks(
        bitcoind,
        tmp,
        None
      )(system)
      funded1 <- fundAccountForWalletWithBitcoind(
        BitcoinSWalletTest.defaultAcctAmts,
        wallet.walletConfig.defaultAccount,
        wallet,
        bitcoind
      )
      hdAccount1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
      funded <- fundAccountForWalletWithBitcoind(
        BitcoinSWalletTest.account1Amt,
        hdAccount1,
        funded1,
        bitcoind
      )
    } yield {
      FundedDLCWallet(funded.asInstanceOf[DLCWallet],
                      wallet.walletConfig,
                      wallet.dlcConfig)
    }
  }
}
