package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.WalletApi
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionConstants,
  TransactionOutput
}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.chain.MockChainQueryApi
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.bitcoins.testkitcore.Implicits._
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.bitcoins.testkitcore.util.TransactionTestUtil
import org.scalatest.FutureOutcome
import org.scalatest.compatible.Assertion

import scala.concurrent.Future

class ProcessTransactionTest extends BitcoinSWalletTest {
  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNewWallet(test)(getFreshWalletAppConfig)
  }

  behavior of "Wallet.processTransaction"

  /** Verifies that executing the given action doesn't change wallet state */
  private def checkUtxosAndBalance(
      wallet: WalletApi
  )(action: => Future[?]): Future[Assertion] =
    for {
      oldTransactions <- wallet.transactionProcessing.listTransactions()
      oldUtxos <- wallet.utxoHandling.getUtxos()
      oldUnconfirmed <- wallet.getUnconfirmedBalance()
      oldConfirmed <- wallet.getBalance()
      _ <- action // by name
      newTransactions <- wallet.transactionProcessing.listTransactions()
      newUtxos <- wallet.utxoHandling.getUtxos()
      newUnconfirmed <- wallet.getUnconfirmedBalance()
      newConfirmed <- wallet.getBalance()

    } yield {
      assert(oldConfirmed == newConfirmed)
      assert(oldUnconfirmed == newUnconfirmed)
      // make utxos comparable
      val comparableOldUtxos =
        oldUtxos.map(_.copyWithId(0)).sortBy(_.outPoint.hex)
      val comparableNewUtxos =
        newUtxos.map(_.copyWithId(0)).sortBy(_.outPoint.hex)
      assert(comparableOldUtxos == comparableNewUtxos)
      assert(oldTransactions == newTransactions)
    }

  it must "change state when processing a transaction with a block hash" in {
    wallet =>
      for {
        address <- wallet.getNewAddress()
        output = TransactionOutput(Bitcoins.one, address.scriptPubKey)
        outPoint = TransactionGenerators.outPoint.sampleSome
        tx =
          TransactionTestUtil.buildTransactionTo(output, outPoint)

        _ <- wallet.transactionProcessing.processTransaction(tx, None)
        oldConfirmed <- wallet.getConfirmedBalance()
        oldUnconfirmed <- wallet.getUnconfirmedBalance()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet) {
          wallet.transactionProcessing.processTransaction(tx, None)
        }

        _ <- wallet.transactionProcessing.processTransaction(
          tx,
          Some(MockChainQueryApi.blockHashWithConfs)
        )
        newConfirmed <- wallet.getConfirmedBalance()
        newUnconfirmed <- wallet.getUnconfirmedBalance()
        utxosPostAdd <- wallet.utxoHandling.getUtxos()

        // repeating the action should not make a difference

        _ <- checkUtxosAndBalance(wallet) {
          wallet.transactionProcessing.processTransaction(
            tx,
            Some(MockChainQueryApi.blockHashWithConfs))
        }
      } yield {
        val ourOutputs =
          tx.outputs.filter(_.scriptPubKey == address.scriptPubKey)

        assert(utxosPostAdd.length == ourOutputs.length)
        assert(newConfirmed != oldConfirmed)
        assert(newUnconfirmed != oldUnconfirmed)
      }
  }

  it must "not change state when processing the same transaction twice" in {
    wallet =>
      for {
        address <- wallet.getNewAddress()
        tx =
          TransactionGenerators
            .transactionTo(address.scriptPubKey)
            .sampleSome

        _ <- wallet.transactionProcessing.processTransaction(tx, None)
        oldConfirmed <- wallet.getConfirmedBalance()
        oldUnconfirmed <- wallet.getUnconfirmedBalance()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet) {
          wallet.transactionProcessing.processTransaction(tx, None)
        }

        _ <- wallet.transactionProcessing.processTransaction(tx, None)
        newConfirmed <- wallet.getConfirmedBalance()
        newUnconfirmed <- wallet.getUnconfirmedBalance()
        utxosPostAdd <- wallet.utxoHandling.getUtxos()

        // repeating the action should not make a difference
        _ <- checkUtxosAndBalance(wallet) {
          wallet.transactionProcessing.processTransaction(tx, None)
        }
      } yield {
        val ourOutputs =
          tx.outputs.filter(_.scriptPubKey == address.scriptPubKey)

        assert(utxosPostAdd.length == ourOutputs.length)
        assert(newConfirmed == oldConfirmed)
        assert(newUnconfirmed == oldUnconfirmed)
      }
  }

  it must "not change state when processing an unrelated transaction" in {
    wallet =>
      val unrelated = TransactionGenerators.transaction.sampleSome
      for {
        _ <- checkUtxosAndBalance(wallet) {
          wallet.transactionProcessing.processTransaction(unrelated, None)
        }

        balance <- wallet.getBalance()
        unconfirmed <- wallet.getUnconfirmedBalance()

      } yield {
        assert(balance == 0.sats)
        assert(unconfirmed == 0.sats)
      }
  }

  it must "spend and receive funds in the same transaction where the funding utxo is reserved" in {
    wallet =>
      val fundingAddressF = wallet.getNewAddress()
      val receivingAddressF = wallet.getNewAddress()
      val amount = Bitcoins.one

      val amtWithFee = amount + Satoshis(175) // for fee

      // build funding tx
      val fundingTxF: Future[(Transaction, UInt32)] = for {
        fundingAddr <- fundingAddressF
        output = TransactionOutput(amtWithFee, fundingAddr.scriptPubKey)
        fundingTx = TransactionGenerators.buildCreditingTransaction(
          TransactionConstants.version,
          output,
          TransactionGenerators.outPoint.sampleSome
        )
      } yield fundingTx

      val processedFundingTxF: Future[Unit] = for {
        (fundingTx, _) <- fundingTxF
        // make sure wallet is empty
        balance <- wallet.getBalance()
        _ = assert(balance == Bitcoins.zero)
        processed <- wallet.transactionProcessing.processTransaction(fundingTx,
                                                                     None)
        balance <- wallet.getBalance()
        _ = assert(balance == amtWithFee)
      } yield processed

      // build spending tx
      val spendingTxF = for {
        receivingAddress <- receivingAddressF
        _ <- processedFundingTxF
        destinations = Vector(
          TransactionOutput(amount, receivingAddress.scriptPubKey)
        )
        rawTxHelper <- wallet.fundTxHandling.fundRawTransaction(
          destinations = destinations,
          feeRate = SatoshisPerByte.one,
          fromTagOpt = None,
          markAsReserved = true
        )
        _ <- wallet.transactionProcessing.processTransaction(
          transaction = rawTxHelper.signedTx,
          blockHashWithConfsOpt = None
        )
        balance <- wallet.getBalance()
      } yield assert(balance == amount)

      spendingTxF
  }

  it must "get the unconfirmed balance of an account" in { (wallet: Wallet) =>
    val account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
    for {
      address <- wallet.getNewAddress()
      tx =
        TransactionGenerators
          .transactionTo(address.scriptPubKey)
          .sampleSome

      _ <- wallet.transactionProcessing.processTransaction(tx, None)
      accountBalance <- wallet.accountHandling.getUnconfirmedBalance(account1)
    } yield {
      assert(accountBalance == CurrencyUnits.zero)
    }
  }
}
