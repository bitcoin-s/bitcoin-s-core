package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{InputInfo, InputSigningInfo}

import scala.util.{Failure, Random, Success, Try}

/** This trait is responsible for converting RawTxBuilderResults into
  * finalized (unsigned) transactions. This process usually includes
  * such things as computation on inputs and outputs to generate
  * things like change outputs, or to reorder inputs or outputs.
  *
  * Once a transaction is done being finalized, its txid/wtxid should
  * not change as the RawTxSigner's only responsibility is adding signature
  * data not included in the txid/wtxid.
  *
  * RawTxFinalizer may (but is not required to) generate witness data
  * other than signatures (i.e. public keys for P2WPKH and redeem scripts
  * for P2WSH). RawTxFinalizer may not otherwise populate any other kind
  * of script signature or witness data.
  *
  * RawTxFinalizers are compose-able through the andThen method which will
  * turn the first RawTxFinalizer's finalized transaction into a RawTxBuilderResult
  * by taking that transactions inputs (in order), outputs (in order), locktime and
  * version and this RawTxBuilderResult is then given to the second RawTxFinalizer.
  */
trait RawTxFinalizer {

  /** Constructs a finalized (unsigned) transaction */
  def buildTx(txBuilderResult: RawTxBuilderResult): Transaction

  /** The result of buildTx is converted into a RawTxBuilderResult
    * by taking that transactions inputs (in order), outputs (in order),
    * locktime and version and this RawTxBuilderResult is then passed to
    * the other RawTxFinalizer's buildTx
    */
  def andThen(other: RawTxFinalizer): RawTxFinalizer = {
    // this.buildTx above gets shadowed below, so this allows us to call it
    def thisBuildTx(txBuilderResult: RawTxBuilderResult): Transaction =
      this.buildTx(txBuilderResult)

    new RawTxFinalizer {
      override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
        val thisResult =
          RawTxBuilderResult.fromTransaction(thisBuildTx(txBuilderResult))
        other.buildTx(thisResult)
      }
    }
  }
}

abstract class FinalizerFactory[T <: RawTxFinalizer] {

  def txBuilderFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      defaultSequence: UInt32 = Policy.sequence): RawTxBuilderWithFinalizer[
    T] = {
    val inputs = InputUtil.calcSequenceForInputs(utxos, defaultSequence)
    val lockTime = TxUtil.calcLockTime(utxos).get
    val builder = RawTxBuilder().setLockTime(lockTime) ++= outputs ++= inputs
    val finalizer =
      txFinalizerFrom(utxos.toVector.map(_.inputInfo), feeRate, changeSPK)

    builder.setFinalizer(finalizer)
  }

  def txFinalizerFrom(
      inputs: Vector[InputInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): T

  def txFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): Transaction = {
    val builder = txBuilderFrom(outputs, utxos, feeRate, changeSPK)

    builder.buildTx()
  }
}

/** A trivial finalizer that does no processing */
case object RawFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    txBuilderResult.toBaseTransaction
  }
}

object RawFinalizerFactory extends FinalizerFactory[RawFinalizer.type] {

  override def txFinalizerFrom(
      inputs: Vector[InputInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): RawFinalizer.type = {
    RawFinalizer
  }

  def txBuilderWithLockTimeFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      lockTime: UInt32,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      defaultSequence: UInt32 = Policy.sequence): RawTxBuilderWithFinalizer[
    RawFinalizer.type] = {
    val inputs = InputUtil.calcSequenceForInputs(utxos, defaultSequence)
    val builder = RawTxBuilder().setLockTime(lockTime) ++= outputs ++= inputs
    val finalizer =
      txFinalizerFrom(utxos.toVector.map(_.inputInfo), feeRate, changeSPK)

    builder.setFinalizer(finalizer)
  }

  def txFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      lockTime: UInt32,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): Transaction = {
    val builder =
      txBuilderWithLockTimeFrom(outputs, utxos, lockTime, feeRate, changeSPK)

    builder.buildTx()
  }
}

/** A simple finalizer that only removes outputs beneath the dust
  * threshold and does nothing else
  */
case object FilterDustFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val filteredOutputs =
      txBuilderResult.outputs.filter(_.value >= Policy.dustThreshold)

    txBuilderResult.toBaseTransaction.copy(outputs = filteredOutputs)
  }
}

case object BIP69Finalizer extends RawTxFinalizer {
  import org.bitcoins.core.transactionInputOrder
  import org.bitcoins.core.transactionOutputOrder

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val sortedInputs = txBuilderResult.inputs.sorted
    val sortedOutputs = txBuilderResult.outputs.sorted

    txBuilderResult.toBaseTransaction.copy(inputs = sortedInputs,
                                           outputs = sortedOutputs)
  }
}

/** A finalizer who's Future fails if its sanity checks are not passed,
  * otherwise it does nothing.
  */
case class SanityCheckFinalizer(
    inputInfos: Vector[InputInfo],
    expectedOutputSPKs: Vector[ScriptPubKey],
    expectedFeeRate: FeeUnit,
    changeSPKs: Vector[ScriptPubKey] = Vector.empty)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val tx = txBuilderResult.toBaseTransaction

    val passInOutChecksT =
      SanityCheckFinalizer.sanityDestinationChecks(inputInfos.map(_.outPoint),
                                                   expectedOutputSPKs,
                                                   changeSPKs,
                                                   tx)

    val passChecksT = passInOutChecksT.flatMap { _ =>
      TxUtil.sanityChecks(isSigned = false, inputInfos, expectedFeeRate, tx)
    }

    passChecksT match {
      case Success(_)   => tx
      case Failure(err) => throw err
    }
  }
}

object SanityCheckFinalizer {

  /** Checks that a finalized transaction contains the expected
    * inputs and scriptpubkeys.
    *
    * Note that it is not responsible for checking output values
    * or that change isn't dropped (as it can be dust). That is
    * covered by TxUtil.sanityChecks above
    */
  def sanityDestinationChecks(
      expectedOutPoints: Vector[TransactionOutPoint],
      expectedOutputSPKs: Vector[ScriptPubKey],
      changeSPKs: Vector[ScriptPubKey],
      finalizedTx: Transaction): Try[Unit] = {
    //make sure we send coins to the appropriate destinations
    val finalizedSPKs = finalizedTx.outputs.map(_.scriptPubKey)
    val isMissingDestination =
      !expectedOutputSPKs.forall(finalizedSPKs.contains)
    val hasExtraOutputs = finalizedSPKs
      .diff(expectedOutputSPKs)
      .diff(changeSPKs)
      .nonEmpty
    val spendingTxOutPoints = finalizedTx.inputs.map(_.previousOutput)
    val hasExtraOutPoints =
      !spendingTxOutPoints.forall(expectedOutPoints.contains) ||
        expectedOutPoints.length != spendingTxOutPoints.length
    if (isMissingDestination) {
      TxBuilderError.MissingDestinationOutput
    } else if (hasExtraOutputs) {
      TxBuilderError.ExtraOutputsAdded
    } else if (hasExtraOutPoints) {
      TxBuilderError.ExtraOutPoints
    } else {
      Success(())
    }
  }
}

/** A finalizer which performs fee estimation and adds a
  * change output resulting in the expected fee.
  */
case class ChangeFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val outputsWithDummyChange =
      txBuilderResult.outputs :+ TransactionOutput(Satoshis.zero, changeSPK)

    val totalCrediting = inputInfos
      .map(_.output.value)
      .foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)
    val totalSpending =
      txBuilderResult.outputs
        .map(_.value)
        .foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)

    val txDummyChange =
      txBuilderResult.toBaseTransaction.copy(outputs = outputsWithDummyChange)
    val dummyTx = TxUtil.addDummySigs(txDummyChange, inputInfos)

    val fee = feeRate.calc(dummyTx)
    val change = totalCrediting - totalSpending - fee

    val newChangeOutput = TransactionOutput(change, changeSPK)

    val newOutputs = if (change <= Policy.dustThreshold) {
      txBuilderResult.outputs
    } else {
      txBuilderResult.outputs :+ newChangeOutput
    }

    txBuilderResult.toBaseTransaction.copy(outputs = newOutputs)
  }
}

/** A finalizer which adds only the witness data included in the
  * wtxid (pubkey in P2WPKH and redeem script in p2sh).
  *
  * Note that when used in composition with other finalizers,
  * if AddWitnessDataFinalizer is not last then its effects
  * will be reversed since witness data is not currently kept
  * between finalizers during composition.
  */
case class AddWitnessDataFinalizer(inputInfos: Vector[InputInfo])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {

    val result = txBuilderResult.toBaseTransaction

    val sortedInputInfos = result.inputs
      .flatMap(input => inputInfos.find(_.outPoint == input.previousOutput))
      .toVector

    require(sortedInputInfos.size == inputInfos.size, "Missing input infos")

    val witnesses = sortedInputInfos.map(InputInfo.getScriptWitness)
    TransactionWitness.fromWitOpt(witnesses) match {
      case _: EmptyWitness =>
        txBuilderResult.toBaseTransaction
      case wit: TransactionWitness =>
        WitnessTransaction
          .toWitnessTx(txBuilderResult.toBaseTransaction)
          .copy(witness = wit)
    }
  }
}

/** A finalizer which adds a change output, performs sanity checks,
  * and adds non-signature witness data. This is the standard
  * non-interactive finalizer within the Bitcoin-S wallet.
  */
case class StandardNonInteractiveFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val addChange = ChangeFinalizer(inputInfos, feeRate, changeSPK)

    val sanityCheck = SanityCheckFinalizer(
      inputInfos = inputInfos,
      expectedOutputSPKs = txBuilderResult.outputs.map(_.scriptPubKey),
      expectedFeeRate = feeRate,
      changeSPKs = Vector(changeSPK))

    val addWitnessData = AddWitnessDataFinalizer(inputInfos)

    addChange
      .andThen(sanityCheck)
      .andThen(addWitnessData)
      .buildTx(txBuilderResult)
  }
}

object StandardNonInteractiveFinalizer
    extends FinalizerFactory[StandardNonInteractiveFinalizer] {

  override def txFinalizerFrom(
      inputs: Vector[InputInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): StandardNonInteractiveFinalizer = {
    StandardNonInteractiveFinalizer(inputs, feeRate, changeSPK)
  }
}

/** A finalizer which adds a change output, performs sanity checks,
  * shuffles inputs and outputs, and adds non-signature witness data.
  * This is the standard non-interactive finalizer within the Bitcoin-S wallet.
  */
case class ShufflingNonInteractiveFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val addChange = ChangeFinalizer(inputInfos, feeRate, changeSPK)

    val shuffled = addChange.andThen(ShuffleFinalizer)

    val tempTx = shuffled.buildTx(txBuilderResult)
    val tempTxBuilderResult = RawTxBuilderResult.fromTransaction(tempTx)

    val shuffledInputInfos = tempTxBuilderResult.inputs
      .map(input => inputInfos.find(_.outPoint == input.previousOutput).get)

    val sanityCheck =
      SanityCheckFinalizer(inputInfos = shuffledInputInfos,
                           expectedOutputSPKs =
                             tempTxBuilderResult.outputs.map(_.scriptPubKey),
                           expectedFeeRate = feeRate,
                           changeSPKs = Vector(changeSPK))

    sanityCheck
      .andThen(AddWitnessDataFinalizer(shuffledInputInfos))
      .buildTx(tempTxBuilderResult)
  }
}

object ShufflingNonInteractiveFinalizer
    extends FinalizerFactory[ShufflingNonInteractiveFinalizer] {

  override def txFinalizerFrom(
      inputs: Vector[InputInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): ShufflingNonInteractiveFinalizer = {
    ShufflingNonInteractiveFinalizer(inputs, feeRate, changeSPK)
  }
}

/** Adds a an amount to the output with the given ScriptPubKey
  * and subtracts that amount in equal proportions from the specified
  * change ScriptPubKeys.
  *
  * This can be useful when you want to account for a future spending
  * fee in this transaction to get nice output values on the spending tx.
  */
case class AddFutureFeeFinalizer(
    spk: ScriptPubKey,
    futureFee: CurrencyUnit,
    changeSPKs: Vector[ScriptPubKey])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val changeOutputs = txBuilderResult.outputs.filter(output =>
      changeSPKs.contains(output.scriptPubKey))

    val outputT = txBuilderResult.outputs.zipWithIndex
      .find(_._1.scriptPubKey == spk) match {
      case Some((output, index)) =>
        val newOutput = output.copy(value = output.value + futureFee)
        Success((newOutput, index))
      case None =>
        Failure(new RuntimeException(
          s"Did not find expected SPK $spk in ${txBuilderResult.outputs.map(_.scriptPubKey)}"))
    }

    val outputsT = outputT.map { case (newOutput, index) =>
      val subFromChange =
        Satoshis(futureFee.satoshis.toLong / changeOutputs.length)
      val outputsMinusChange = txBuilderResult.outputs.map { output =>
        if (changeSPKs.contains(output.scriptPubKey)) {
          output.copy(value = output.value - subFromChange)
        } else {
          output
        }
      }

      outputsMinusChange.updated(index, newOutput)
    }

    val txT = outputsT.map { outputs =>
      txBuilderResult.toBaseTransaction.copy(outputs = outputs)
    }

    txT match {
      case Success(tx)  => tx
      case Failure(err) => throw err
    }
  }
}

/** Subtracts the given fee from the output with the given ScriptPubKey.
  * This can be useful if you are constructing a transaction without
  * considering fees and have some after-the-fact external formula for
  * computing fees and they need the removed.
  */
case class SubtractFromOutputFinalizer(spk: ScriptPubKey, subAmt: CurrencyUnit)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    txBuilderResult.outputs.zipWithIndex.find(_._1.scriptPubKey == spk) match {
      case Some((output, index)) =>
        val newOutput = output.copy(value = output.value - subAmt)
        val newOutputs = txBuilderResult.outputs.updated(index, newOutput)

        txBuilderResult.toBaseTransaction.copy(outputs = newOutputs)
      case None =>
        throw new RuntimeException(
          s"Did not find expected SPK $spk in ${txBuilderResult.outputs.map(_.scriptPubKey)}")
    }
  }
}

/** Assumes the input transaction has had no fee considerations
  * and subtracts the estimated fee in equal portions from the
  * outputs with the specified ScriptPubKeys
  */
case class SubtractFeeFromOutputsFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    spks: Vector[ScriptPubKey])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val dummyTx =
      TxUtil.addDummySigs(txBuilderResult.toBaseTransaction, inputInfos)

    val outputsAfterFee =
      SubtractFeeFromOutputsFinalizer.subtractFees(
        dummyTx,
        feeRate,
        spks
      )

    txBuilderResult.toBaseTransaction.copy(outputs = outputsAfterFee)
  }
}

object SubtractFeeFromOutputsFinalizer {

  def subtractFees(
      tx: Transaction,
      feeRate: FeeUnit,
      spks: Vector[ScriptPubKey]): Vector[TransactionOutput] = {
    val fee = feeRate.calc(tx)

    val (outputs, unchangedOutputs) =
      tx.outputs.zipWithIndex.toVector.partition { case (output, _) =>
        spks.contains(output.scriptPubKey)
      }

    val feePerOutput = Satoshis(Int64(fee.satoshis.toLong / outputs.length))
    val feeRemainder = Satoshis(Int64(fee.satoshis.toLong % outputs.length))

    val newOutputsWithoutRemainder = outputs.map { case (output, index) =>
      (TransactionOutput(output.value - feePerOutput, output.scriptPubKey),
       index)
    }
    val (lastOutput, lastOutputIndex) = newOutputsWithoutRemainder.last
    val newLastOutput = TransactionOutput(lastOutput.value - feeRemainder,
                                          lastOutput.scriptPubKey)
    val newOutputs = newOutputsWithoutRemainder
      .dropRight(1)
      .:+((newLastOutput, lastOutputIndex))

    (newOutputs ++ unchangedOutputs).sortBy(_._2).map(_._1)
  }
}

case class DualFundingInput(
    scriptSignature: ScriptSignature,
    maxWitnessLen: Int)

/** Finalizes a dual-funded transaction given the DualFundingInputs
  * from both parties, their change spks and the funding scriptpubkey
  * for the dual funded protocol.
  *
  * This includes adding the future fee of spending transactions
  * to the funding output as well as subtracting relevant fees
  * from the change outputs. This finalizer filters dust outputs.
  */
case class DualFundingTxFinalizer(
    offerInputs: Vector[DualFundingInput],
    offerPayoutSPK: ScriptPubKey,
    offerChangeSPK: ScriptPubKey,
    acceptInputs: Vector[DualFundingInput],
    acceptPayoutSPK: ScriptPubKey,
    acceptChangeSPK: ScriptPubKey,
    feeRate: SatoshisPerVirtualByte,
    fundingSPK: ScriptPubKey)
    extends RawTxFinalizer {

  /** @see https://github.com/discreetlogcontracts/dlcspecs/blob/8ee4bbe816c9881c832b1ce320b9f14c72e3506f/Transactions.md#fees */
  private def computeFees(
      inputs: Vector[DualFundingInput],
      payoutSPK: ScriptPubKey,
      changeSPK: ScriptPubKey): (CurrencyUnit, CurrencyUnit) = {
    // https://github.com/discreetlogcontracts/dlcspecs/blob/8ee4bbe816c9881c832b1ce320b9f14c72e3506f/Transactions.md#expected-weight-of-the-contract-execution-or-refund-transaction
    val futureFeeWeight = 249 + 4 * payoutSPK.asmBytes.length
    val futureFeeVBytes = Math.ceil(futureFeeWeight / 4.0).toLong
    val futureFee = feeRate * futureFeeVBytes

    // https://github.com/discreetlogcontracts/dlcspecs/blob/8ee4bbe816c9881c832b1ce320b9f14c72e3506f/Transactions.md#expected-weight-of-the-funding-transaction
    val inputWeight =
      inputs.foldLeft(0L) {
        case (weight, DualFundingInput(scriptSignature, maxWitnessLen)) =>
          weight + 164 + 4 * scriptSignature.asmBytes.length + maxWitnessLen.toInt
      }
    val outputWeight = 36 + 4 * changeSPK.asmBytes.length
    val weight = 107 + outputWeight + inputWeight
    val vbytes = Math.ceil(weight / 4.0).toLong
    val fundingFee = feeRate * vbytes

    (futureFee, fundingFee)
  }

  lazy val (offerFutureFee, offerFundingFee) =
    computeFees(offerInputs, offerPayoutSPK, offerChangeSPK)

  lazy val offerFees: CurrencyUnit = offerFutureFee + offerFundingFee

  lazy val (acceptFutureFee, acceptFundingFee) =
    computeFees(acceptInputs, acceptPayoutSPK, acceptChangeSPK)

  lazy val acceptFees: CurrencyUnit = acceptFutureFee + acceptFundingFee

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val addOfferFutureFee =
      AddFutureFeeFinalizer(fundingSPK, offerFutureFee, Vector(offerChangeSPK))
    val addAcceptFutureFee = AddFutureFeeFinalizer(fundingSPK,
                                                   acceptFutureFee,
                                                   Vector(acceptChangeSPK))
    val subtractOfferFundingFee =
      SubtractFromOutputFinalizer(offerChangeSPK, offerFundingFee)
    val subtractAcceptFundingFee =
      SubtractFromOutputFinalizer(acceptChangeSPK, acceptFundingFee)

    addOfferFutureFee
      .andThen(addAcceptFutureFee)
      .andThen(subtractOfferFundingFee)
      .andThen(subtractAcceptFundingFee)
      .andThen(FilterDustFinalizer)
      .buildTx(txBuilderResult)
  }
}

/** Shuffles in the inputs and outputs of the Transaction into a random order */
case object ShuffleFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    ShuffleInputsFinalizer
      .andThen(ShuffleOutputsFinalizer)
      .buildTx(txBuilderResult)
  }
}

/** Shuffles in the inputs of the Transaction into a random order */
case object ShuffleInputsFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val shuffledInputs = Random.shuffle(txBuilderResult.inputs)

    txBuilderResult.toBaseTransaction.copy(inputs = shuffledInputs)
  }
}

/** Shuffles in the outputs of the Transaction into a random order */
case object ShuffleOutputsFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult): Transaction = {
    val shuffledOutputs = Random.shuffle(txBuilderResult.outputs)

    txBuilderResult.toBaseTransaction.copy(outputs = shuffledOutputs)
  }
}
