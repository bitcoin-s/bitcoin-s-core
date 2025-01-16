package org.bitcoins.wallet

import org.bitcoins.core.api.wallet.*
import org.bitcoins.core.currency.*
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.EmptyScriptSignature
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.wallet.fee.*
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256DigestBE}
import org.bitcoins.feeprovider.RandomFeeProvider
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.FeeUnitGen
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{OutgoingTransactionDAO, SpendingInfoDAO}
import org.bitcoins.wallet.util.WalletUtil
import org.scalatest.{Assertion, FutureOutcome}
import scodec.bits.ByteVector

import scala.concurrent.Future

class WalletSendingTest extends BitcoinSWalletTest {

  override type FixtureParam = FundedWallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFundedWallet(test)(getFreshWalletAppConfig)

  behavior of "Wallet"

  val testAddress: BitcoinAddress =
    BitcoinAddress("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq")

  val amountToSend: Bitcoins = Bitcoins(0.5)

  it should "correctly send to an address" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    for {
      tx <- wallet.sendFundsHandling.sendToAddress(testAddress,
                                                   amountToSend,
                                                   None)
    } yield {
      assert(
        tx.outputs.contains(
          TransactionOutput(amountToSend, testAddress.scriptPubKey)
        )
      )
    }
  }

  val addresses = Vector(
    BitcoinAddress("bcrt1qlhctylgvdsvaanv539rg7hyn0sjkdm23y70kgq"),
    BitcoinAddress("bcrt1qf4rju7adz5hpuymkfwvg5s94mydc8llk94v74w"),
    BitcoinAddress("bcrt1q9h9wkz6ad49szfl035wh3qdacuslkp6j9pfp4j"),
    BitcoinAddress("bcrt1q9scvqvyf3ssed8zqnfgk5zttnneatg2aszu5q9")
  )

  val amounts = Vector(
    Bitcoins(0.5),
    Bitcoins(0.25),
    Bitcoins(1),
    Bitcoins(3)
  )

  it should "correctly send to multiple addresses" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    for {
      tx <- wallet.sendFundsHandling.sendToAddresses(addresses, amounts, None)
    } yield {
      val expectedOutputs = addresses.zip(amounts).map { case (addr, amount) =>
        TransactionOutput(amount, addr.scriptPubKey)
      }
      assert(expectedOutputs.diff(tx.outputs).isEmpty)
    }
  }

  it should "fail to send to multiple addresses with an incomplete amount of amounts" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      val sendToAddressesF =
        wallet.sendFundsHandling.sendToAddresses(addresses, amounts.tail, None)

      recoverToSucceededIf[IllegalArgumentException] {
        sendToAddressesF
      }
  }

  it should "fail to send to multiple addresses with an incomplete amount of addresses" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      val sendToAddressesF =
        wallet.sendFundsHandling.sendToAddresses(addresses.tail, amounts, None)

      recoverToSucceededIf[IllegalArgumentException] {
        sendToAddressesF
      }
  }

  it should "correctly send to multiple outputs" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    val expectedOutputs = addresses.zip(amounts).map { case (addr, amount) =>
      TransactionOutput(amount, addr.scriptPubKey)
    }

    for {
      tx <- wallet.sendFundsHandling.sendToOutputs(expectedOutputs, None)
    } yield {
      assert(expectedOutputs.diff(tx.outputs).isEmpty)
    }
  }

  def testOpReturnCommitment(
      wallet: WalletApi,
      hashMessage: Boolean
  )(implicit walletConfig: WalletAppConfig): Future[Assertion] = {
    val message = "ben was here"
    for {
      tx <- wallet.sendFundsHandling.makeOpReturnCommitment(
        message = message,
        hashMessage = hashMessage,
        feeRateOpt = None
      )
      outgoingTxDAO = OutgoingTransactionDAO()(executionContext, walletConfig)
      outgoingTxDbOpt <- outgoingTxDAO.read(tx.txIdBE)
    } yield {
      val opReturnOutputOpt = tx.outputs.find(_.value == 0.satoshis)
      assert(opReturnOutputOpt.isDefined, "Missing output with 0 value")
      val opReturnOutput = opReturnOutputOpt.get

      val messageBytes = if (hashMessage) {
        CryptoUtil.sha256(ByteVector(message.getBytes)).bytes
      } else {
        ByteVector(message.getBytes)
      }

      val expectedAsm =
        Seq(
          OP_RETURN,
          BytesToPushOntoStack(messageBytes.size),
          ScriptConstant(messageBytes)
        )

      assert(opReturnOutput.scriptPubKey.asm == expectedAsm)

      assert(outgoingTxDbOpt.isDefined, "Missing outgoing tx in database")
      val outgoingTxDb = outgoingTxDbOpt.get

      assert(outgoingTxDb.sentAmount == 0.satoshis)
      val changeOutput = tx.outputs.find(_.value > 0.satoshis).get
      assert(
        outgoingTxDb.actualFee + changeOutput.value == outgoingTxDb.inputAmount
      )
    }
  }

  it should "correctly make a hashed OP_RETURN commitment" in { fundedWallet =>
    testOpReturnCommitment(fundedWallet.wallet, hashMessage = true)(
      fundedWallet.walletConfig)
  }

  it should "correctly make an unhashed OP_RETURN commitment" in {
    fundedWallet =>
      testOpReturnCommitment(fundedWallet.wallet, hashMessage = false)(
        fundedWallet.walletConfig)
  }

  it should "fail to make an OP_RETURN commitment that is too long" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      recoverToSucceededIf[IllegalArgumentException] {
        wallet.sendFundsHandling.makeOpReturnCommitment(
          "This message is much too long and is over 80 bytes, the limit for OP_RETURN. It should cause an error.",
          hashMessage = false,
          None
        )
      }
  }

  it should "fail to send to a different network address" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    val sendToAddressesF =
      wallet.sendFundsHandling.sendToAddress(
        BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
        Satoshis(1000),
        None
      )

    recoverToSucceededIf[IllegalArgumentException] {
      sendToAddressesF
    }
  }

  it should "fail to send to different network addresses" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    val addrs = Vector(
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"),
      BitcoinAddress("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa")
    )

    val sendToAddressesF =
      wallet.sendFundsHandling.sendToAddresses(addrs, amounts, None)

    recoverToSucceededIf[IllegalArgumentException] {
      sendToAddressesF
    }
  }

  it should "correctly send from outpoints" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    val spendingInfoDAO =
      SpendingInfoDAO()(executionContext, fundedWallet.walletConfig)
    for {
      allOutPoints <- spendingInfoDAO.findAllOutpoints()
      // use half of them
      outPoints = allOutPoints.drop(allOutPoints.size / 2)
      tx <- wallet.sendFundsHandling.sendFromOutPoints(outPoints,
                                                       testAddress,
                                                       amountToSend,
                                                       None)
    } yield {
      assert(
        outPoints.forall(outPoint =>
          tx.inputs.exists(_.previousOutput == outPoint)),
        "Every outpoint was not included included"
      )
      assert(tx.inputs.size == outPoints.size, "An extra input was added")

      val expectedOutput =
        TransactionOutput(amountToSend, testAddress.scriptPubKey)
      assert(
        tx.outputs.contains(expectedOutput),
        "Did not contain expected output"
      )
    }
  }

  it should "correctly send entire outpoints" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    for {
      allUtxos <- wallet.utxoHandling.getUtxos()
      // use half of them
      utxos = allUtxos.drop(allUtxos.size / 2)
      outPoints = utxos.map(_.outPoint)
      tx <- wallet.sendFundsHandling.sendFromOutPoints(outPoints,
                                                       testAddress,
                                                       None)
    } yield {
      val expectedFeeRate =
        wallet.feeRateApi.asInstanceOf[RandomFeeProvider].lastFeeRate.get
      assert(
        outPoints.forall(outPoint =>
          tx.inputs.exists(_.previousOutput == outPoint)),
        "Every outpoint was not included included"
      )
      assert(tx.inputs.size == outPoints.size, "An extra input was added")

      val inputAmount = utxos.map(_.output.value).sum
      val numInputs = outPoints.size

      val defaultRange = expectedFeeRate.scaleFactor * numInputs

      val (actualFeeRate, range) = expectedFeeRate match {
        case _: SatoshisPerByte =>
          (SatoshisPerByte.calc(inputAmount, tx), defaultRange)
        case _: SatoshisPerVirtualByte =>
          (SatoshisPerVirtualByte.calc(inputAmount, tx), defaultRange)
        case _: SatoshisPerKiloByte =>
          (SatoshisPerKiloByte.calc(inputAmount, tx), defaultRange)
        case _: SatoshisPerKW =>
          // multiply range by 4 because an extra byte on a sig counts as 4 weight units
          (SatoshisPerKW.calc(inputAmount, tx), defaultRange * 4)
      }

      // +- range in case of rounding or unexpected signature sizes
      assert(
        actualFeeRate.toLong === expectedFeeRate.toLong +- range,
        s"Expected fee rate: $expectedFeeRate, inputs: $numInputs input: $inputAmount, tx bytes: ${tx.byteSize} " +
          s"vsize: ${tx.vsize} weight ${tx.weight}"
      )

      assert(tx.outputs.size == 1)
      assert(tx.outputs.head.scriptPubKey == testAddress.scriptPubKey)
    }
  }

  it must "fail to sendfromoutpoints if the outpoint doesn't exist" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      recoverToSucceededIf[IllegalArgumentException] {
        wallet.sendFundsHandling.sendFromOutPoints(Vector.empty,
                                                   testAddress,
                                                   SatoshisPerVirtualByte.one)
      }
  }

  it should "correctly sweep the wallet" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    for {
      utxos <- wallet.utxoHandling.getUtxos()
      tx <- wallet.sendFundsHandling.sweepWallet(testAddress, None)
      balance <- wallet.getBalance()
    } yield {
      assert(balance == Satoshis.zero)

      val expectedFeeRate =
        wallet.feeRateApi.asInstanceOf[RandomFeeProvider].lastFeeRate.get
      assert(
        utxos
          .map(_.outPoint)
          .forall(outPoint => tx.inputs.exists(_.previousOutput == outPoint)),
        "Every outpoint was not included included"
      )
      assert(tx.inputs.size == utxos.size, "An extra input was added")

      val inputAmount = utxos.map(_.output.value).sum
      val numInputs = utxos.size

      val defaultRange = expectedFeeRate.scaleFactor * numInputs

      val (actualFeeRate, range) = expectedFeeRate match {
        case _: SatoshisPerByte =>
          (SatoshisPerByte.calc(inputAmount, tx), defaultRange)
        case _: SatoshisPerVirtualByte =>
          (SatoshisPerVirtualByte.calc(inputAmount, tx), defaultRange)
        case _: SatoshisPerKiloByte =>
          (SatoshisPerKiloByte.calc(inputAmount, tx), defaultRange)
        case _: SatoshisPerKW =>
          // multiply range by 4 because an extra byte on a sig counts as 4 weight units
          (SatoshisPerKW.calc(inputAmount, tx), defaultRange * 4)
      }

      // +- range in case of rounding or unexpected signature sizes
      assert(
        actualFeeRate.toLong === expectedFeeRate.toLong +- range,
        s"Expected fee rate: $expectedFeeRate, inputs: $numInputs input: $inputAmount, tx bytes: ${tx.byteSize} " +
          s"vsize: ${tx.vsize} weight ${tx.weight}"
      )

      assert(tx.outputs.size == 1)
      assert(tx.outputs.head.scriptPubKey == testAddress.scriptPubKey)
    }
  }

  it should "correctly bump the fee rate of a transaction" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val feeRate = FeeUnitGen.satsPerByte.sampleSome

    for {
      tx <- wallet.sendFundsHandling.sendToAddress(testAddress,
                                                   amountToSend,
                                                   feeRate)

      firstBal <- wallet.getBalance()

      newFeeRate = SatoshisPerByte(feeRate.currencyUnit + Satoshis(50))
      bumpedTx <- wallet.sendFundsHandling.bumpFeeRBF(tx.txIdBE, newFeeRate)
      outgoingTxDAO = OutgoingTransactionDAO()(executionContext,
                                               fundedWallet.walletConfig)
      txDb1Opt <- outgoingTxDAO.findByTxId(tx.txIdBE)
      txDb2Opt <- outgoingTxDAO.findByTxId(bumpedTx.txIdBE)

      secondBal <- wallet.getBalance()
    } yield {
      assert(txDb1Opt.isDefined)
      assert(txDb2Opt.isDefined)
      val txDb1 = txDb1Opt.get
      val txDb2 = txDb2Opt.get

      assert(txDb1.actualFee < txDb2.actualFee)
      assert(firstBal - secondBal == txDb2.actualFee - txDb1.actualFee)
    }
  }

  it should "fail to RBF a confirmed transaction" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val feeRate = FeeUnitGen.satsPerByte.sampleSome
    val newFeeRate = SatoshisPerByte(feeRate.currencyUnit + Satoshis.one)

    for {
      tx <- wallet.sendFundsHandling.sendToAddress(testAddress,
                                                   amountToSend,
                                                   feeRate)
      blockHashWithConfsOpt <- WalletUtil.getBlockHashWithConfs(
        chainQueryApi,
        Some(DoubleSha256DigestBE.empty))
      _ <- wallet.transactionProcessing.processTransaction(
        tx,
        blockHashWithConfsOpt)

      res <- recoverToSucceededIf[IllegalArgumentException] {
        wallet.sendFundsHandling.bumpFeeRBF(tx.txIdBE, newFeeRate)
      }
    } yield res
  }

  it should "fail to RBF a non-signaling transaction" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    for {
      addr <- wallet.getNewAddress()
      utxo <- wallet.utxoHandling.getUtxos().map(_.head)

      // Create tx not signaling RBF
      input = TransactionInput(
        utxo.outPoint,
        EmptyScriptSignature,
        TransactionConstants.disableRBFSequence
      )
      output =
        TransactionOutput(utxo.output.value - Satoshis(500), addr.scriptPubKey)
      tx =
        BaseTransaction(Int32.two, Vector(input), Vector(output), UInt32.zero)
      psbt = PSBT.fromUnsignedTx(tx)

      // Have wallet sign and process transaction
      signedPSBT <- wallet.sendFundsHandling.signPSBT(psbt)
      signedTx = signedPSBT.finalizePSBT.get.extractTransactionAndValidate.get
      _ <- wallet.transactionProcessing.processTransaction(signedTx, None)

      res <- recoverToSucceededIf[IllegalArgumentException] {
        wallet.sendFundsHandling.bumpFeeRBF(
          signedTx.txIdBE,
          SatoshisPerVirtualByte.fromLong(100))
      }
    } yield res
  }

  it should "correctly CPFP a transaction" in { fundedWallet =>
    val wallet = fundedWallet.wallet
    for {
      parent <- wallet.sendFundsHandling.sendToAddress(testAddress,
                                                       amountToSend,
                                                       None)
      bumpRate <- wallet.feeRateApi.getFeeRate()
      child <- wallet.sendFundsHandling.bumpFeeCPFP(parent.txIdBE, bumpRate)
      spendingInfoDAO = SpendingInfoDAO()(executionContext,
                                          fundedWallet.walletConfig)
      received <- spendingInfoDAO.findTx(child).map(_.nonEmpty)
    } yield {
      // Verify we are only sending to ourself
      assert(child.outputs.size == 1)
      assert(received)

      // Verify we are only spending 1 output from the parent tx
      assert(child.inputs.size == 1)
      assert(child.inputs.head.previousOutput.txId == parent.txId)
      assert(child.inputs.head.previousOutput.vout.toInt < parent.outputs.size)

      // Verify fee rate
      val utxo = parent.outputs(child.inputs.head.previousOutput.vout.toInt)
      val inputAmount = utxo.value
      val childFeeRate = bumpRate match {
        case _: SatoshisPerByte =>
          SatoshisPerByte.calc(inputAmount, child)
        case _: SatoshisPerKiloByte =>
          SatoshisPerKiloByte.calc(inputAmount, child)
        case _: SatoshisPerVirtualByte =>
          SatoshisPerVirtualByte.calc(inputAmount, child)
        case _: SatoshisPerKW =>
          SatoshisPerKW.calc(inputAmount, child)
      }

      // Do +/- scale factor because of rounding errors
      assert(childFeeRate.toLong === bumpRate.toLong +- bumpRate.scaleFactor)
    }
  }

  it should "fail to CPFP a confirmed transaction" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    val feeRate = FeeUnitGen.satsPerByte.sampleSome

    for {
      tx <- wallet.sendFundsHandling.sendToAddress(testAddress,
                                                   amountToSend,
                                                   feeRate)
      blockHashWithConfsOpt <- WalletUtil.getBlockHashWithConfs(
        chainQueryApi,
        Some(DoubleSha256DigestBE.empty))
      _ <- wallet.transactionProcessing.processTransaction(
        tx,
        blockHashWithConfsOpt)

      res <- recoverToSucceededIf[IllegalArgumentException] {
        wallet.sendFundsHandling.bumpFeeCPFP(tx.txIdBE, feeRate)
      }
    } yield res
  }

  it should "fail to CPFP a transaction we don't own" in { fundedWallet =>
    val wallet = fundedWallet.wallet

    recoverToSucceededIf[RuntimeException](
      wallet.sendFundsHandling.bumpFeeCPFP(EmptyTransaction.txIdBE,
                                           SatoshisPerByte.one)
    )
  }

  it should "fail to send from outpoints when already spent" in {
    fundedWallet =>
      val wallet = fundedWallet.wallet
      for {
        allUtxos <- wallet.utxoHandling.getUtxos()
        // Make one already spent
        spent = allUtxos.head
          .copyWithSpendingTxId(
            DoubleSha256DigestBE.empty
          ) // dummy spending txid
          .copyWithState(TxoState.PendingConfirmationsSpent)
        spendingInfoDAO = SpendingInfoDAO()(executionContext,
                                            fundedWallet.walletConfig)
        _ <- spendingInfoDAO.update(spent)
        test <- recoverToSucceededIf[IllegalArgumentException](
          wallet.sendFundsHandling.sendFromOutPoints(
            allUtxos.map(_.outPoint),
            testAddress,
            amountToSend,
            None
          )
        )
      } yield test
  }

  def testSendWithAlgo(
      wallet: WalletApi,
      algo: CoinSelectionAlgo
  ): Future[Assertion] = {
    for {
      account <- wallet.accountHandling.getDefaultAccount()
      feeRate <- wallet.getFeeRate()
      allUtxos <- wallet.utxoHandling
        .getUtxos(account.hdAccount)
        .map(_.map(CoinSelectorUtxo.fromSpendingInfoDb))

      output = TransactionOutput(amountToSend, testAddress.scriptPubKey)
      expectedUtxos =
        CoinSelector.selectByAlgo(algo, allUtxos, Vector(output), feeRate)
      tx <- wallet.sendFundsHandling.sendWithAlgo(testAddress,
                                                  amountToSend,
                                                  feeRate,
                                                  algo)
    } yield {
      val diff =
        expectedUtxos.map(_.outPoint).diff(tx.inputs.map(_.previousOutput))
      assert(diff.isEmpty, s"Incorrect inputs, $diff")

      val expectedOutput =
        TransactionOutput(amountToSend, testAddress.scriptPubKey)
      assert(
        tx.outputs.contains(expectedOutput),
        "Did not contain expected output"
      )
    }
  }

  it should "correctly send with accumulate largest" in { fundedWallet =>
    testSendWithAlgo(fundedWallet.wallet, CoinSelectionAlgo.AccumulateLargest)
  }

  it should "correctly send with accumulate smallest" in { fundedWallet =>
    testSendWithAlgo(
      fundedWallet.wallet,
      CoinSelectionAlgo.AccumulateSmallestViable
    )
  }

  it should "correctly send with standard accumulate" in { fundedWallet =>
    testSendWithAlgo(fundedWallet.wallet, CoinSelectionAlgo.StandardAccumulate)
  }

  it must "it must not double spend utxos used to fund other txs in the wallet" in {
    fundedWallet =>
      // i expected an error saying insufficient balance

      val addr1F = fundedWallet.wallet.getNewAddress()
      val balanceF = fundedWallet.wallet.getBalance()

      val failedTx: Future[Unit] = for {
        balance <- balanceF
        addr1 <- addr1F
        amt = balance - Satoshis(
          5000
        ) // for fee, fee rates are random so we might need a lot

        // build these transactions in parallel intentionally
        outputs = Vector(TransactionOutput(amt, addr1.scriptPubKey))
        _ <- fundedWallet.wallet.fundTxHandling.fundRawTransaction(
          destinations = outputs,
          feeRate = SatoshisPerVirtualByte.one,
          fromTagOpt = None,
          markAsReserved = true)
        _ <- fundedWallet.wallet.fundTxHandling.fundRawTransaction(
          destinations = outputs,
          feeRate = SatoshisPerVirtualByte.one,
          fromTagOpt = None,
          markAsReserved = true)
      } yield ()

      val exnF: Future[RuntimeException] =
        recoverToExceptionIf[RuntimeException](failedTx)

      exnF.map(err =>
        assert(
          err.getMessage.contains(
            "Not enough value in given outputs to make transaction spending 599995000 sats plus fees"
          )
        ))

  }
}
