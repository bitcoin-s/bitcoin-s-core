package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.wallet.utxo.*

import scala.util.{Failure, Success, Try}

/** Created by chris on 4/6/16. Represents a transaction whose input is being
  * checked against the spending conditions of a
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
  */
sealed abstract class TxSigComponent {

  /** The transaction being checked for the validity of signatures */
  def transaction: Transaction

  /** The index of the input whose script signature is being checked */
  def inputIndex: UInt32

  def input: TransactionInput = transaction.inputs(inputIndex.toInt)

  /** The script signature being checked */
  def scriptSignature: ScriptSignature = input.scriptSignature

  /** This is the output we are spending. We need this for script and digital
    * signatures checks
    */
  def output: TransactionOutput

  /** The scriptPubKey for which the input is being checked against */
  def scriptPubKey: ScriptPubKey = output.scriptPubKey

  /** The amount of [[org.bitcoins.core.currency.CurrencyUnit CurrencyUnit]] we
    * are spending in this TxSigComponent
    */
  def amount: CurrencyUnit = output.value

  /** The flags that are needed to verify if the signature is correct */
  def flags: Seq[ScriptFlag]

  /** Represents the serialization algorithm used to verify/create signatures
    * for Bitcoin
    */
  def sigVersion: SignatureVersion
}

object TxSigComponent {

  def apply(
      inputInfo: InputInfo,
      unsignedTx: Transaction,
      outputMap: PreviousOutputMap,
      flags: Seq[ScriptFlag] = Policy.standardFlags): TxSigComponent = {
    inputInfo match {
      case kp: TaprootKeyPathInputInfo =>
        fromWitnessInput(kp, unsignedTx, flags)
      case segwit: SegwitV0NativeInputInfo =>
        fromWitnessInput(segwit, unsignedTx, flags)
      case unassigned: UnassignedSegwitNativeInputInfo =>
        fromWitnessInput(unassigned, unsignedTx, outputMap, flags)
      case p2sh: P2SHInputInfo =>
        fromP2SHInput(p2sh, unsignedTx, flags)
      case raw: RawInputInfo =>
        fromRawInput(raw, unsignedTx, flags)
    }
  }

  private def setTransactionWitness(
      inputInfo: InputInfo,
      unsignedTx: Transaction): WitnessTransaction = {
    val idx = TxUtil.inputIndex(inputInfo, unsignedTx)
    val unsignedWtx = WitnessTransaction.toWitnessTx(unsignedTx)

    unsignedWtx.witness.witnesses(idx) match {
      // Only set the witness if we don't already have one
      case EmptyScriptWitness =>
        InputInfo.getScriptWitness(inputInfo) match {
          case None =>
            unsignedWtx
          case Some(scriptWitness) =>
            unsignedWtx.updateWitness(idx, scriptWitness)
        }
      case _: ScriptWitnessV0 => unsignedWtx
      case _: TaprootWitness  => unsignedWtx
    }
  }

  def fromWitnessInput(
      inputInfo: SegwitV0NativeInputInfo,
      unsignedTx: Transaction,
      flags: Seq[ScriptFlag]): TxSigComponent = {
    val idx = TxUtil.inputIndex(inputInfo, unsignedTx)
    val wtx = setTransactionWitness(inputInfo, unsignedTx)

    WitnessTxSigComponentRaw(transaction = wtx,
                             inputIndex = UInt32(idx),
                             output = inputInfo.output,
                             flags = flags)
  }

  def fromWitnessInput(
      inputInfo: UnassignedSegwitNativeInputInfo,
      unsignedTx: Transaction,
      outputMap: PreviousOutputMap,
      flags: Seq[ScriptFlag]): TxSigComponent = {
    val idx = TxUtil.inputIndex(inputInfo, unsignedTx)
    val wtx = setTransactionWitness(inputInfo, unsignedTx)

    WitnessTxSigComponent(wtx, UInt32(idx), inputInfo.output, outputMap, flags)
  }

  def fromWitnessInput(
      info: TaprootKeyPathInputInfo,
      unsignedTx: Transaction,
      flags: Seq[ScriptFlag]): TaprootTxSigComponent = {
    val inputIndex: UInt32 = UInt32(info.inputIndex)
    val wtx = setTransactionWitness(info, unsignedTx)
    TaprootTxSigComponent(transaction = wtx,
                          inputIndex = inputIndex,
                          outputMap = info.previousOutputMap,
                          flags = flags)
  }

  def fromWitnessInput(
      inputInfo: P2SHNestedSegwitV0InputInfo,
      unsignedTx: Transaction,
      flags: Seq[ScriptFlag] = Policy.standardFlags): TxSigComponent = {
    val idx = TxUtil.inputIndex(inputInfo, unsignedTx)
    val emptyInput = unsignedTx.inputs(idx)
    val newInput = TransactionInput(
      emptyInput.previousOutput,
      P2SHScriptSignature(EmptyScriptSignature, inputInfo.redeemScript),
      emptyInput.sequence)
    val updatedTx = unsignedTx.updateInput(idx, newInput)

    val wtx = WitnessTransaction.toWitnessTx(updatedTx)
    val updatedWtx =
      wtx.updateWitness(idx, InputInfo.getScriptWitness(inputInfo).get)

    WitnessTxSigComponentP2SH(updatedWtx, UInt32(idx), inputInfo.output, flags)
  }

  def fromP2SHInput(
      inputInfo: P2SHInputInfo,
      unsignedTx: Transaction,
      flags: Seq[ScriptFlag]): TxSigComponent = {
    inputInfo match {
      case nonSegwit: P2SHNonSegwitInputInfo =>
        fromP2SHInput(nonSegwit, unsignedTx, flags)
      case segwit: P2SHNestedSegwitV0InputInfo =>
        fromWitnessInput(segwit, unsignedTx, flags)
    }
  }

  def fromP2SHInput(
      inputInfo: P2SHNonSegwitInputInfo,
      unsignedTx: Transaction,
      flags: Seq[ScriptFlag]): TxSigComponent = {
    val idx = TxUtil.inputIndex(inputInfo, unsignedTx)

    val updatedTx = unsignedTx.inputs(idx).scriptSignature match {
      case EmptyScriptSignature =>
        val emptyInput = unsignedTx.inputs(idx)
        val newInput = TransactionInput(
          emptyInput.previousOutput,
          P2SHScriptSignature(EmptyScriptSignature, inputInfo.redeemScript),
          emptyInput.sequence)
        unsignedTx.updateInput(idx, newInput)
      case _: P2SHScriptSignature =>
        unsignedTx
      case invalid @ (_: CLTVScriptSignature | _: CSVScriptSignature |
          _: ConditionalScriptSignature | _: MultiSignatureScriptSignature |
          _: NonStandardScriptSignature | _: P2PKHScriptSignature |
          _: P2PKScriptSignature | TrivialTrueScriptSignature) =>
        throw new IllegalArgumentException(
          s"Unexpected script sig with P2SHNonSegwitInputInfo, got $invalid")
    }

    P2SHTxSigComponent(updatedTx, UInt32(idx), inputInfo.output, flags)
  }

  def fromRawInput(
      inputInfo: RawInputInfo,
      unsignedTx: Transaction,
      flags: Seq[ScriptFlag] = Policy.standardFlags): TxSigComponent = {
    val idx = TxUtil.inputIndex(inputInfo, unsignedTx)
    BaseTxSigComponent(unsignedTx, UInt32(idx), inputInfo.output, flags)
  }

  def apply(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      outputMap: PreviousOutputMap,
      flags: Seq[ScriptFlag]): TxSigComponent = {
    val scriptSig = transaction.inputs(inputIndex.toInt).scriptSignature
    output.scriptPubKey match {
      case _: WitnessScriptPubKey =>
        transaction match {
          case _: NonWitnessTransaction =>
            // before soft fork activation, you can spend a segwit output with a base transaction
            // as segwit outputs are ANYONECANSPEND before soft fork activation
            BaseTxSigComponent(transaction, inputIndex, output, flags)
          case wtx: WitnessTransaction =>
            WitnessTxSigComponent(wtx, inputIndex, output, outputMap, flags)
        }
      case _: P2SHScriptPubKey =>
        val p2shScriptSig = scriptSig.asInstanceOf[P2SHScriptSignature]
        if (WitnessScriptPubKey.isValidAsm(p2shScriptSig.redeemScript.asm)) {
          transaction match {
            case _: NonWitnessTransaction =>
              throw new IllegalArgumentException(
                s"Cannot spend from segwit output ($output) with a base transaction ($transaction)")
            case wtx: WitnessTransaction =>
              WitnessTxSigComponentP2SH(wtx, inputIndex, output, flags)
          }
        } else {
          P2SHTxSigComponent(transaction, inputIndex, output, flags)
        }
      case _: RawScriptPubKey =>
        BaseTxSigComponent(transaction, inputIndex, output, flags)
    }
  }

  def getScriptWitness(
      txSigComponent: TxSigComponent): Option[ScriptWitness] = {
    txSigComponent.transaction match {
      case _: NonWitnessTransaction => None
      case wtx: WitnessTransaction =>
        val witness = wtx.witness.witnesses(txSigComponent.inputIndex.toInt)
        witness match {
          case EmptyScriptWitness             => None
          case witness: ScriptWitnessV0       => Some(witness)
          case taprootWitness: TaprootWitness => Some(taprootWitness)
        }
    }
  }
}

/** The [[org.bitcoins.core.crypto.TxSigComponent TxSigComponent]] used to
  * evaluate the original Satoshi transaction digest algorithm. Basically
  * this is every spk that is not a
  * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
  * EXCEPT in the case of a P2SH(witness script)
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
  */
sealed abstract class BaseTxSigComponent extends TxSigComponent {
  override def sigVersion: SignatureVersion = SigVersionBase
}

sealed abstract class P2SHTxSigComponent extends BaseTxSigComponent {

  require(
    input.scriptSignature
      .isInstanceOf[P2SHScriptSignature],
    "Must have P2SHScriptSignature for P2SH, got: " + input.scriptSignature
  )
  require(
    output.scriptPubKey
      .isInstanceOf[P2SHScriptPubKey],
    "Must have P2SHScriptPubKey for P2SH, got: " + output.scriptPubKey
  )

  override def scriptPubKey: P2SHScriptPubKey =
    output.scriptPubKey.asInstanceOf[P2SHScriptPubKey]

  override def scriptSignature: P2SHScriptSignature =
    input.scriptSignature.asInstanceOf[P2SHScriptSignature]
}

/** The [[org.bitcoins.core.crypto.TxSigComponent TxSigComponent]] used to
  * represent all the components necessarily for
  * [[https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki BIP143]].
  * Examples of these
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]'s are
  * [[org.bitcoins.core.protocol.script.P2WPKHWitnessV0 P2WPKHWitnessSPKV0]],
  * [[org.bitcoins.core.protocol.script.P2WSHWitnessSPKV0 P2WSHWitnessSPKV0]],
  * and P2SH(witness script)
  */
sealed trait WitnessTxSigComponent extends TxSigComponent {

  override def transaction: WitnessTransaction

  def witness: ScriptWitness = transaction.witness(inputIndex.toInt)

  def witnessVersion: WitnessVersion
}

/** This represents checking the
  * [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]
  * against a
  * [[org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0 P2WPKHWitnessSPKV0]]
  * or a
  * [[org.bitcoins.core.protocol.script.P2WSHWitnessSPKV0 P2WSHWitnessSPKV0]]
  */
sealed abstract class WitnessTxSigComponentRaw extends WitnessTxSigComponent {

  override def scriptPubKey: WitnessScriptPubKey =
    output.scriptPubKey.asInstanceOf[WitnessScriptPubKey]

  override def witnessVersion: WitnessVersion = {
    scriptPubKey.witnessVersion
  }

  override def sigVersion: SignatureVersion = {
    scriptPubKey match {
      case t: TaprootScriptPubKey =>
        sys.error(
          s"Use TaprootTxSigComponent for taproot spks rather than WitnessTxSigComponentRaw, got=$t")
      case _: WitnessScriptPubKeyV0 | _: UnassignedWitnessScriptPubKey =>
        SigVersionWitnessV0
    }
  }
}

/** This represents checking the
  * [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]
  * against a P2SH(P2WSH) or P2SH(P2WPKH) scriptPubKey
  */
sealed abstract class WitnessTxSigComponentP2SH
    extends P2SHTxSigComponent
    with WitnessTxSigComponent {

  override def scriptPubKey: P2SHScriptPubKey =
    output.scriptPubKey.asInstanceOf[P2SHScriptPubKey]

  override def scriptSignature: P2SHScriptSignature = {
    val s = transaction.inputs(inputIndex.toInt).scriptSignature
    require(s.isInstanceOf[P2SHScriptSignature],
            "Must have P2SHScriptSignature for P2SH(WitSPK()), got: " + s)
    val p2sh = s.asInstanceOf[P2SHScriptSignature]
    p2sh

  }

  def witnessScriptPubKey: Try[WitnessScriptPubKey] =
    scriptSignature.redeemScript match {
      case w: WitnessScriptPubKey => Success(w)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: P2SHScriptPubKey | _: CSVScriptPubKey | _: CLTVScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | EmptyScriptPubKey) =>
        Failure(new IllegalArgumentException(
          "Must have a witness scriptPubKey as redeemScript for P2SHScriptPubKey in WitnessTxSigComponentP2SH, got: " + x))

    }

  override def witnessVersion: WitnessVersion =
    witnessScriptPubKey match {
      case Success(w)   => w.witnessVersion
      case Failure(err) => throw err
    }

  override def amount: CurrencyUnit = output.value

  override def sigVersion: SignatureVersion = SigVersionWitnessV0
}

/** This represents a 'rebuilt'
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] that was
  * constructed from
  * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
  * After the [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] is
  * rebuilt, we need to use that rebuilt scriptpubkey to evaluate the
  * [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]] See
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program BIP141]]
  * for more info on rebuilding P2WSH and P2WPKH scriptpubkeys
  */
sealed abstract class WitnessTxSigComponentRebuilt extends TxSigComponent {
  override def transaction: WitnessTransaction

  override def scriptPubKey: ScriptPubKey = output.scriptPubKey

  /** The
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    * we used to rebuild the scriptPubKey above
    */
  def witnessScriptPubKey: WitnessScriptPubKey

  override def sigVersion: SignatureVersion = witnessScriptPubKey match {
    case _: WitnessScriptPubKeyV0 => SigVersionWitnessV0
    case _: TaprootScriptPubKey   =>
      // i believe we cannot have keypath spend here because we don't
      // need to rebuild a spk with keypath spent
      // https://github.com/bitcoin/bitcoin/blob/9e4fbebcc8e497016563e46de4c64fa094edab2d/src/script/interpreter.cpp#L399
      SigVersionTapscript
    case w: UnassignedWitnessScriptPubKey =>
      sys.error(
        s"Cannot determine sigVersion for an unassigned witness, got=$w")
  }

  def witnessVersion: WitnessVersion = witnessScriptPubKey.witnessVersion

}

/** Tx sig component that contains the differences between BIP143 (segwit v0)
  * transaction signature serialization and BIP341.
  *
  * The unique thing with BIP341 is the message commits to the scriptPubKeys of
  * all outputs spent by the transaction, also
  *
  * If the SIGHASH_ANYONECANPAY flag is not set, the message commits to the
  * amounts of all transaction inputs.[18]
  *
  * This means we need to bring ALL the outputs we are spending, even though
  * this data structure is for checking the signature of a _single_ output.
  *
  * @see
  *   https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#common-signature-message
  */
case class TaprootTxSigComponent(
    transaction: WitnessTransaction,
    inputIndex: UInt32,
    outputMap: PreviousOutputMap,
    flags: Seq[ScriptFlag])
    extends WitnessTxSigComponent {
  require(
    scriptPubKey.isInstanceOf[TaprootScriptPubKey],
    s"Can only spend taproot spks with TaprootTxSigComponent, got=$scriptPubKey")

  override lazy val output: TransactionOutput = {
    val outpoint = transaction.inputs(inputIndex.toInt).previousOutput
    outputMap(outpoint)
  }

  val outputs: Vector[TransactionOutput] = outputMap.values.toVector

  override def scriptPubKey: TaprootScriptPubKey = {
    output.scriptPubKey.asInstanceOf[TaprootScriptPubKey]
  }

  override def witness: TaprootWitness = {
    transaction.witness(inputIndex.toInt) match {
      case t: TaprootWitness  => t
      case EmptyScriptWitness => TaprootKeyPath.dummy
      case s: ScriptWitnessV0 =>
        sys.error(
          s"Cannot have ScriptWitnessV0 in a TaprootTxSigComponent, got=$s")
    }
  }

  override val witnessVersion: WitnessVersion1.type = WitnessVersion1

  override def sigVersion: SigVersionTaproot = {
    witness match {
      case _: TaprootKeyPath => SigVersionTaprootKeySpend
      case _: TaprootScriptPath | _: TaprootUnknownPath => SigVersionTapscript
    }
  }
}

object BaseTxSigComponent {

  private case class BaseTxSigComponentImpl(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends BaseTxSigComponent

  def apply(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): BaseTxSigComponent = {
    BaseTxSigComponentImpl(transaction, inputIndex, output, flags)
  }

}

object P2SHTxSigComponent {

  private case class P2SHTxSigComponentImpl(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends P2SHTxSigComponent

  def apply(
      transaction: Transaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): P2SHTxSigComponent = {
    lazy val nonWitnessSigComponent =
      P2SHTxSigComponentImpl(transaction, inputIndex, output, flags)
    transaction match {
      case _: NonWitnessTransaction => nonWitnessSigComponent
      case wtx: WitnessTransaction =>
        if (wtx.witness(inputIndex.toInt) == EmptyScriptWitness) {
          nonWitnessSigComponent
        } else {
          WitnessTxSigComponentP2SH(wtx, inputIndex, output, flags)
        }
    }
  }
}

object WitnessTxSigComponent {

  def apply(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      outputMap: PreviousOutputMap,
      flags: Seq[ScriptFlag]): WitnessTxSigComponent =
    output.scriptPubKey match {
      case _: WitnessScriptPubKeyV0 | _: UnassignedWitnessScriptPubKey =>
        WitnessTxSigComponentRaw(transaction, inputIndex, output, flags)
      case _: TaprootScriptPubKey =>
        TaprootTxSigComponent(transaction, inputIndex, outputMap, flags)
      case _: P2SHScriptPubKey =>
        WitnessTxSigComponentP2SH(transaction, inputIndex, output, flags)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: LockTimeScriptPubKey | _: ConditionalScriptPubKey |
          _: WitnessCommitment | _: NonStandardScriptPubKey |
          EmptyScriptPubKey) =>
        throw new IllegalArgumentException(
          s"Cannot create a WitnessTxSigComponent out of $x")
    }

}

object WitnessTxSigComponentRaw {

  private case class WitnessTxSigComponentRawImpl(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends WitnessTxSigComponentRaw

  def apply(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): WitnessTxSigComponentRaw = {
    output.scriptPubKey match {
      case _: WitnessScriptPubKey =>
        WitnessTxSigComponentRawImpl(transaction, inputIndex, output, flags)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: P2SHScriptPubKey | _: LockTimeScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | EmptyScriptPubKey) =>
        throw new IllegalArgumentException(
          s"Cannot create a WitnessTxSigComponentRaw with a spk of $x")
    }

  }
}

object WitnessTxSigComponentP2SH {

  private case class WitnessTxSigComponentP2SHImpl(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag])
      extends WitnessTxSigComponentP2SH

  def apply(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      flags: Seq[ScriptFlag]): WitnessTxSigComponentP2SH = {
    output.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        WitnessTxSigComponentP2SHImpl(transaction, inputIndex, output, flags)
      case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: LockTimeScriptPubKey | _: ConditionalScriptPubKey |
          _: NonStandardScriptPubKey | _: WitnessCommitment |
          _: WitnessScriptPubKey | EmptyScriptPubKey) =>
        throw new IllegalArgumentException(
          s"Cannot create a WitnessTxSigComponentP2SH with a spk of $x")
    }

  }
}

object WitnessTxSigComponentRebuilt {

  private case class WitnessTxSigComponentRebuiltImpl(
      transaction: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      witnessScriptPubKey: WitnessScriptPubKey,
      flags: Seq[ScriptFlag])
      extends WitnessTxSigComponentRebuilt

  def apply(
      wtx: WitnessTransaction,
      inputIndex: UInt32,
      output: TransactionOutput,
      witScriptPubKey: WitnessScriptPubKey,
      flags: Seq[ScriptFlag]): WitnessTxSigComponentRebuilt = {
    WitnessTxSigComponentRebuiltImpl(wtx,
                                     inputIndex,
                                     output,
                                     witScriptPubKey,
                                     flags)
  }
}
