package org.bitcoins.core.psbt

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.{HashType, _}
import scodec.bits._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class PSBT(
    globalMap: GlobalPSBTMap,
    inputMaps: Vector[InputPSBTMap],
    outputMaps: Vector[OutputPSBTMap])
    extends NetworkElement {
  require(
    inputMaps.size == transaction.inputs.size,
    s"There must be an input map for every input in the global transaction, inputs: ${transaction.inputs}")
  require(
    outputMaps.size == transaction.outputs.size,
    s"There must be an output map for every output in the global transaction, outputs: ${transaction.outputs}"
  )

  require(
    inputMaps.zip(transaction.inputs).forall { case (inputMap, txIn) =>
      val prevTxOpt = inputMap.nonWitnessOrUnknownUTXOOpt
      prevTxOpt.isEmpty || prevTxOpt.get.transactionSpent.txId == txIn.previousOutput.txId
    },
    "Some of the inputMaps' nonWitnessOrUnknownUTXO txId does not match the unsigned transaction's txId" +
      s", got $inputMaps, ${transaction.inputs}"
  )

  import org.bitcoins.core.psbt.InputPSBTRecord._
  import org.bitcoins.core.psbt.PSBTInputKeyId._

  // Need to define this so when we compare the PSBTs
  // the map lexicographical ordering is enforced
  override def equals(other: Any): Boolean =
    other match {
      case p: PSBT => this.bytes == p.bytes
      case _       => other.equals(this)
    }

  private val inputBytes: ByteVector =
    inputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  private val outputBytes: ByteVector =
    outputMaps.foldLeft(ByteVector.empty)(_ ++ _.bytes)

  override val bytes: ByteVector = PSBT.magicBytes ++
    globalMap.bytes ++
    inputBytes ++
    outputBytes

  val base64: String = bytes.toBase64

  def transaction: Transaction = globalMap.unsignedTransaction.transaction

  def isFinalized: Boolean = inputMaps.forall(_.isFinalized)

  def version: UInt32 = globalMap.version.version

  def validateBIP143Vulnerability: PSBT = {
    require(
      isFinalized || inputMaps.size == 1 || inputMaps.forall(
        !_.isBIP143Vulnerable),
      "One or more of the input maps are susceptible to the BIP 143 vulnerability"
    )

    this
  }

  /** The next [[PSBTRole]] that should be used for this PSBT */
  lazy val nextRole: PSBTRole = {
    val roles = inputMaps.zip(transaction.inputs).map { case (inputMap, txIn) =>
      inputMap.nextRole(txIn)
    }

    roles.minBy(_.order)
  }

  lazy val feeOpt: Option[CurrencyUnit] = {
    val hasPrevUtxos =
      inputMaps.zipWithIndex.forall(i => i._1.prevOutOpt(i._2).isDefined)
    if (hasPrevUtxos) {
      val inputAmount = inputMaps.zipWithIndex.foldLeft(CurrencyUnits.zero) {
        case (accum, (input, index)) =>
          // .get is safe because of hasPrevUtxos
          val prevOut = input.prevOutOpt(index).get
          accum + prevOut.value
      }
      val outputAmount =
        transaction.outputs.foldLeft(CurrencyUnits.zero)(_ + _.value)
      Some(inputAmount - outputAmount)
    } else None
  }

  lazy val estimateWeight: Option[Long] = {
    if (nextRole.order >= PSBTRole.SignerPSBTRole.order) {
      val dummySigner = Sign.dummySign(ECPublicKey.freshPublicKey)

      val inputWeight =
        inputMaps.zip(transaction.inputs).foldLeft(0L) {
          case (weight, (inputMap, txIn)) =>
            val signingInfo = inputMap
              .toUTXOSatisfyingInfoUsingSigners(txIn, Vector(dummySigner))
            val scriptSigLen = signingInfo.maxScriptSigLen
            val maxWitnessLen = signingInfo.maxWitnessLen

            weight + 164 + maxWitnessLen + scriptSigLen
        }
      val outputWeight = transaction.outputs.foldLeft(0L)(_ + _.byteSize)
      val weight = 107 + outputWeight + inputWeight

      Some(weight)
    } else None
  }

  lazy val estimateVSize: Option[Long] = {
    estimateWeight.map { weight =>
      Math.ceil(weight / 4.0).toLong
    }
  }

  lazy val estimateSatsPerVByte: Option[SatoshisPerVirtualByte] = {
    (feeOpt, estimateVSize) match {
      case (Some(fee), Some(vsize)) =>
        val rate = SatoshisPerVirtualByte.fromLong(fee.satoshis.toLong / vsize)
        Some(rate)
      case (None, None) | (Some(_), None) | (None, Some(_)) =>
        None
    }
  }

  /** Combiner defined by
    * https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#combiner
    * Takes another PSBT and adds all records that are not contained in this
    * PSBT A record's distinctness is determined by its key
    * @param other
    *   PSBT to be combined with
    * @return
    *   A PSBT with the combined data of the two PSBTs
    */
  def combinePSBT(other: PSBT): PSBT = {
    require(this.transaction.txId == other.transaction.txId,
            "Can only combine PSBTs with the same global transaction.")

    val global = this.globalMap.combine(other.globalMap)
    val inputs = this.inputMaps
      .zip(other.inputMaps)
      .map { case (input, otherInput) => input.combine(otherInput) }
    val outputs = this.outputMaps
      .zip(other.outputMaps)
      .map { case (output, otherOutput) => output.combine(otherOutput) }

    PSBT(global, inputs, outputs)
  }

  def combinePSBTAndValidate(other: PSBT): PSBT = {
    combinePSBT(other).validateBIP143Vulnerability
  }

  def finalizeInput(index: Int): Try[PSBT] = {
    require(index >= 0 && index < inputMaps.size,
            s"Index must be within 0 and the number of inputs, got: $index")
    val inputMap = inputMaps(index)
    if (inputMap.isFinalized) {
      Success(this)
    } else {
      inputMap.finalize(transaction.inputs(index)).map { finalizedInputMap =>
        val newInputMaps =
          inputMaps.updated(index, finalizedInputMap)

        PSBT(globalMap, newInputMaps, outputMaps)
      }
    }
  }

  /** Finalizes this PSBT if possible, returns a Failure otherwise
    * @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#input-finalizer]]
    */
  def finalizePSBT: Try[PSBT] = {
    if (isFinalized) {
      Failure(
        new IllegalStateException(
          s"Cannot finalize an already finalized PSBT: $this"))
    } else {
      val finalizedInputTs = inputMaps.zip(transaction.inputs).map {
        case (inputMap, input) => inputMap.finalize(input)
      }

      val finalizedInputMapsT = finalizedInputTs
        .foldLeft[Try[Vector[InputPSBTMap]]](Success(Vector.empty)) {
          case (inputsSoFarT, inputT) =>
            inputsSoFarT.flatMap { inputsSoFar =>
              inputT.map { input =>
                inputsSoFar :+ input
              }
            }
        }

      finalizedInputMapsT.map(finalizedInputMaps =>
        this.copy(inputMaps = finalizedInputMaps))
    }
  }

  /** Signs the PSBT's input at the given input with the signer, then adds it to
    * the PSBT in a PartialSignature record
    * @param inputIndex
    *   Index of input to sign
    * @param signer
    *   Function or private key used to sign the PSBT
    * @param conditionalPath
    *   Represents the spending branch being taken in a ScriptPubKey's execution
    * @return
    */
  def sign(
      inputIndex: Int,
      signer: Sign,
      conditionalPath: ConditionalPath = ConditionalPath.NoCondition): PSBT = {
    require(
      inputMaps.size == 1 || !inputMaps(inputIndex).isBIP143Vulnerable,
      "This input map is susceptible to the BIP 143 vulnerability, add the non-witness utxo to be safe"
    )

    BitcoinSigner.sign(psbt = this,
                       inputIndex = inputIndex,
                       signer = signer,
                       conditionalPath = conditionalPath)
  }

  /** Takes the InputPSBTMap at the given index and returns a
    * NewSpendingInfoFull that can be used to sign the input
    * @param index
    *   index of the InputPSBTMap
    * @param signers
    *   Signers that will be used to sign the input
    * @param conditionalPath
    *   Path that should be used for the script
    * @return
    *   A corresponding NewSpendingInfoFull
    */
  def getSpendingInfoUsingSigners(
      index: Int,
      signers: Vector[Sign],
      conditionalPath: ConditionalPath = ConditionalPath.NoCondition)
      : ScriptSignatureParams[InputInfo] = {
    require(index >= 0 && index < inputMaps.size,
            s"Index must be within 0 and the number of inputs, got: $index")
    inputMaps(index)
      .toUTXOSatisfyingInfoUsingSigners(transaction.inputs(index),
                                        signers,
                                        conditionalPath)
  }

  /** Adds tx to the indexed InputPSBTMap to either the NonWitnessOrUnknownUTXO
    * or WitnessUTXO field depending on the tx and available information in the
    * PSBT
    * @param tx
    *   Transaction to add to PSBT
    * @param index
    *   index of the InputPSBTMap to add tx to
    * @return
    *   PSBT with added tx
    */
  def addUTXOToInput(tx: Transaction, index: Int): PSBT = {
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")

    val inputMap = inputMaps(index)
    require(!inputMap.isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val txIn = transaction.inputs(index)
    val elements =
      if (txIn.previousOutput.vout.toInt < tx.outputs.size) {
        val out = tx.outputs(txIn.previousOutput.vout.toInt)

        val outIsWitnessScript =
          WitnessScriptPubKey.isValidAsm(out.scriptPubKey.asm)
        val hasWitScript = inputMap.witnessScriptOpt.isDefined
        val hasWitRedeemScript =
          inputMap.redeemScriptOpt.isDefined && WitnessScriptPubKey
            .isValidAsm(inputMap.redeemScriptOpt.get.redeemScript.asm)
        val notBIP143Vulnerable = {
          !out.scriptPubKey.isInstanceOf[WitnessScriptPubKeyV0] && !(
            hasWitRedeemScript &&
              inputMap.redeemScriptOpt.get.redeemScript
                .isInstanceOf[WitnessScriptPubKeyV0]
          )
        }
        val notTaproot: Boolean =
          !out.scriptPubKey.isInstanceOf[TaprootScriptPubKey]

        if (
          (outIsWitnessScript || hasWitScript || hasWitRedeemScript) && notBIP143Vulnerable && notTaproot
        ) {
          inputMap.filterRecords(WitnessUTXOKeyId) :+ WitnessUTXO(out)
        } else {
          inputMap.filterRecords(
            NonWitnessUTXOKeyId) :+ NonWitnessOrUnknownUTXO(tx)
        }
      } else {
        throw new IllegalArgumentException(
          s"Transaction does not correspond to map at given index($index), got: $tx")
      }

    val newInputMaps =
      inputMaps.updated(index, InputPSBTMap(elements))
    PSBT(globalMap, newInputMaps, outputMaps)

  }

  /** Adds the TransactionOutput to the indexed InputPSBTMap to the WitnessUTXO
    * field
    * @param output
    *   TransactionOutput to add to PSBT
    * @param index
    *   index of the InputPSBTMap to add tx to
    * @return
    *   PSBT with added tx
    */
  def addWitnessUTXOToInput(output: TransactionOutput, index: Int): PSBT = {
    require(WitnessScriptPubKey.isValidAsm(output.scriptPubKey.asm),
            s"Given output was not a Witness UTXO: $output")
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")

    val inputMap = inputMaps(index)
    require(!inputMap.isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val elements =
      inputMap.filterRecords(WitnessUTXOKeyId) :+ WitnessUTXO(output)
    val newInputMaps = inputMaps.updated(index, InputPSBTMap(elements))

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  /** Adds script to the indexed InputPSBTMap to either the RedeemScript or
    * WitnessScript field depending on the script and available information in
    * the PSBT
    * @param script
    *   ScriptPubKey to add to PSBT
    * @param index
    *   index of the InputPSBTMap to add script to
    * @return
    *   PSBT with added script
    */
  def addRedeemOrWitnessScriptToInput(
      script: ScriptPubKey,
      index: Int): PSBT = {
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val inputMap = inputMaps(index)

    val isWitScript = WitnessScriptPubKey.isValidAsm(script.asm)
    val redeemScriptOpt = inputMap.redeemScriptOpt
    val hasRedeemScript = redeemScriptOpt.isDefined && WitnessScriptPubKey
      .isValidAsm(redeemScriptOpt.get.redeemScript.asm)

    val elements = if (!isWitScript && hasRedeemScript) {
      inputMap.filterRecords(WitnessScriptKeyId) :+ InputPSBTRecord
        .WitnessScript(script.asInstanceOf[RawScriptPubKey])
    } else {
      inputMap.filterRecords(RedeemScriptKeyId) :+ InputPSBTRecord.RedeemScript(
        script)
    }
    val newMap = InputPSBTMap(elements).compressMap(transaction.inputs(index))
    val newInputMaps = inputMaps.updated(index, newMap)

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  private def redeemScriptToOutputRecord(
      outputScript: ScriptPubKey,
      redeemScript: ScriptPubKey): OutputPSBTRecord = {
    outputScript match {
      case p2sh: P2SHScriptPubKey =>
        val scriptHash = P2SHScriptPubKey(redeemScript).scriptHash
        if (scriptHash != p2sh.scriptHash) {
          throw new IllegalArgumentException(
            s"The given script's hash does not match the expected script has, got: $scriptHash, expected ${p2sh.scriptHash}")
        } else {
          OutputPSBTRecord.RedeemScript(redeemScript)
        }
      case p2wsh: P2WSHWitnessSPKV0 =>
        val scriptHash = {
          redeemScript match {
            case raw: RawScriptPubKey => P2WSHWitnessSPKV0(raw).scriptHash
            case nonraw @ (_: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
                _: TaprootScriptPubKey | _: UnassignedWitnessScriptPubKey |
                _: P2WSHWitnessSPKV0) =>
              throw new IllegalArgumentException(
                s"Cannot make p2wsh from non raw spk=$nonraw")
          }
        }
        if (scriptHash != p2wsh.scriptHash) {
          throw new IllegalArgumentException(
            s"The given script's hash does not match the expected script has, got: $scriptHash, expected ${p2wsh.scriptHash}")
        } else {
          OutputPSBTRecord.WitnessScript(redeemScript)
        }
      case _: NonStandardScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey | _: MultiSignatureScriptPubKey |
          _: ConditionalScriptPubKey | _: LockTimeScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: WitnessScriptPubKey |
          _: P2PKScriptPubKey | _: P2PKHScriptPubKey =>
        throw new IllegalArgumentException(
          s"Output script does not need a redeem script, got: $outputScript")
    }
  }

  def addScriptWitnessToInput(
      scriptWitness: ScriptWitness,
      index: Int): PSBT = {
    require(index >= 0,
            s"index must be greater than or equal to 0, got: $index")
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")
    require(
      inputMaps(index).witnessScriptOpt.isEmpty,
      s"Input map already contains a ScriptWitness: ${inputMaps(index).witnessScriptOpt.get}")

    val previousElements = inputMaps(index).elements

    val newMap = scriptWitness match {
      case _: P2WPKHWitnessV0 =>
        // We do not need to add the ScriptWitness because it will be known
        // by having a 20 byte hash in the script
        InputPSBTMap(previousElements)
      case p2wsh: P2WSHWitnessV0 =>
        val newElement = InputPSBTRecord.WitnessScript(p2wsh.redeemScript)
        InputPSBTMap(previousElements :+ newElement)
      case taprootWitness: TaprootWitness =>
        throw new UnsupportedOperationException(
          s"Taproot not supported, got=$taprootWitness")
      case EmptyScriptWitness =>
        throw new IllegalArgumentException(
          s"Invalid scriptWitness given, got: $scriptWitness")
    }
    val newInputMaps = inputMaps.updated(index, newMap)
    PSBT(globalMap, newInputMaps, outputMaps)
  }

  def addFinalizedScriptWitnessToInput(
      scriptSignature: ScriptSignature,
      scriptWitness: ScriptWitness,
      index: Int): PSBT = {
    require(index >= 0,
            s"index must be greater than or equal to 0, got: $index")
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val prevInput = inputMaps(index)

    require(
      prevInput.elements.forall {
        case _: NonWitnessOrUnknownUTXO | _: WitnessUTXO | _: Unknown => true
        case redeemScript: RedeemScript                               =>
          // RedeemScript is okay if it matches the script signature given
          redeemScript.redeemScript match {
            case _: NonWitnessScriptPubKey => false
            case wspk: WitnessScriptPubKey =>
              P2SHScriptSignature(wspk) == scriptSignature
          }
        case _: InputPSBTRecord => false
      },
      s"Input already contains fields: ${prevInput.elements}"
    )

    val finalizedScripts = Vector(FinalizedScriptSig(scriptSignature),
                                  FinalizedScriptWitness(scriptWitness))

    // Replace RedeemScripts with FinalizedScriptSignatures and add FinalizedScriptWitnesses
    val records = prevInput.elements.filterNot(
      _.isInstanceOf[RedeemScript]) ++ finalizedScripts
    val newMap = InputPSBTMap(records)
    val newInputMaps = inputMaps.updated(index, newMap)
    PSBT(globalMap, newInputMaps, outputMaps)
  }

  /** Adds script to the indexed OutputPSBTMap to either the RedeemScript or
    * WitnessScript field depending on the script and available information in
    * the PSBT
    * @param script
    *   ScriptPubKey to add to PSBT
    * @param index
    *   index of the OutputPSBTMap to add script to
    * @return
    *   PSBT with added script
    */
  def addRedeemOrWitnessScriptToOutput(
      script: ScriptPubKey,
      index: Int): PSBT = {
    require(
      index < outputMaps.size,
      s"index must be less than the number of output maps present in the psbt, $index >= ${outputMaps.size}")
    require(!isFinalized, "Cannot update a PSBT that is finalized")

    val outputMap = outputMaps(index)
    val redeemScriptOpt = outputMap.redeemScriptOpt.map(_.redeemScript)
    val isWitScript = WitnessScriptPubKey.isValidAsm(script.asm)
    val hasWitScript = redeemScriptOpt.isDefined && WitnessScriptPubKey
      .isValidAsm(redeemScriptOpt.get.asm)

    val newElement =
      if (!isWitScript && hasWitScript)
        redeemScriptToOutputRecord(redeemScriptOpt.get, script)
      else
        redeemScriptToOutputRecord(transaction.outputs(index).scriptPubKey,
                                   script)

    val newMap = OutputPSBTMap(outputMap.elements :+ newElement)
    val newOutputMaps = outputMaps.updated(index, newMap)

    PSBT(globalMap, inputMaps, newOutputMaps)
  }

  def addScriptWitnessToOutput(
      scriptWitness: ScriptWitness,
      index: Int): PSBT = {
    require(index >= 0,
            s"index must be greater than or equal to 0, got: $index")
    require(
      index < outputMaps.size,
      s"index must be less than the number of output maps present in the psbt, $index >= ${inputMaps.size}")
    require(!isFinalized, "Cannot update a PSBT that is finalized")
    require(
      outputMaps(index).witnessScriptOpt.isEmpty,
      s"Output map already contains a ScriptWitness: ${outputMaps(index).witnessScriptOpt.get}")

    val outputMap = outputMaps(index)

    val newMap = scriptWitness match {
      case _: P2WPKHWitnessV0 =>
        // We do not need to add a WitnessScript for P2WPKH because it will
        // be assumed because of the ScriptPubKey having the 20-byte hash
        outputMap
      case p2wsh: P2WSHWitnessV0 =>
        OutputPSBTMap(
          outputMap.filterRecords(PSBTOutputKeyId.WitnessScriptKeyId) :+
            OutputPSBTRecord.WitnessScript(p2wsh.redeemScript))
      case taprootWitness: TaprootWitness =>
        throw new UnsupportedOperationException(
          s"Taproot not supported, got=$taprootWitness")
      case EmptyScriptWitness =>
        throw new IllegalArgumentException(
          s"Invalid scriptWitness given, got: $scriptWitness")
    }

    val newOutputMaps = outputMaps.updated(index, newMap)
    PSBT(globalMap, inputMaps, newOutputMaps)
  }

  private def addKeyPathToMap[
      RecordType <: PSBTRecord,
      MapType <: PSBTMap[RecordType]](
      extKey: ExtKey,
      path: BIP32Path,
      pubKey: ECPublicKey,
      index: Int,
      keyIdByte: Byte,
      maps: Vector[MapType],
      makeRecord: (ECPublicKey, ByteVector, BIP32Path) => RecordType,
      makeMap: Vector[RecordType] => MapType): Vector[MapType] = {
    require(
      index < maps.size,
      s"index must be less than the number of output maps present in the psbt, $index >= ${outputMaps.size}")
    require(!isFinalized, "Cannot update a PSBT that is finalized")

    val previousElements = maps(index).elements
    lazy val expectedBytes = pubKey.bytes.+:(keyIdByte)

    val elements =
      if (!previousElements.exists(_.key == expectedBytes)) {
        val fp =
          if (extKey.fingerprint == ExtKey.masterFingerprint) {
            extKey.deriveChildPubKey(path.head).get.fingerprint
          } else {
            extKey.fingerprint
          }

        previousElements :+ makeRecord(pubKey, fp, path)
      } else {
        previousElements
      }

    maps.updated(index, makeMap(elements))
  }

  /** Adds the BIP32Path to the indexed InputPSBTMap to the BIP32DerivationPath
    * field
    * @param extKey
    *   ExtKey to derive key from
    * @param path
    *   path of key to add to PSBT
    * @param index
    *   index of the InputPSBTMap to add the BIP32Path to
    * @return
    *   PSBT with added BIP32Path
    */
  def addKeyPathToInput(
      extKey: ExtKey,
      path: BIP32Path,
      pubKey: ECPublicKey,
      index: Int): PSBT = {
    val newInputMaps = addKeyPathToMap[InputPSBTRecord, InputPSBTMap](
      extKey = extKey,
      path = path,
      pubKey = pubKey,
      index = index,
      keyIdByte = PSBTInputKeyId.BIP32DerivationPathKeyId.byte,
      maps = inputMaps,
      makeRecord = InputPSBTRecord.BIP32DerivationPath.apply,
      makeMap = InputPSBTMap.apply
    )

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  /** Adds the BIP32Path to the indexed OutputPSBTMap to the BIP32DerivationPath
    * field
    * @param extKey
    *   ExtKey to derive key from
    * @param path
    *   path of key to add to PSBT
    * @param index
    *   index of the OutputPSBTMap to add the BIP32Path to
    * @return
    *   PSBT with added BIP32Path
    */
  def addKeyPathToOutput(
      extKey: ExtKey,
      path: BIP32Path,
      pubKey: ECPublicKey,
      index: Int): PSBT = {
    val newOutputMaps = addKeyPathToMap[OutputPSBTRecord, OutputPSBTMap](
      extKey = extKey,
      path = path,
      pubKey = pubKey,
      index = index,
      keyIdByte = PSBTOutputKeyId.BIP32DerivationPathKeyId.byte,
      maps = outputMaps,
      makeRecord = OutputPSBTRecord.BIP32DerivationPath.apply,
      makeMap = OutputPSBTMap.apply
    )

    PSBT(globalMap, inputMaps, newOutputMaps)
  }

  /** @param hashType
    *   HashType to add to the input
    * @param index
    *   index of the InputPSBTMap to add the HashType to
    * @return
    *   PSBT with added HashType
    */
  def addSigHashTypeToInput(hashType: HashType, index: Int): PSBT = {
    require(
      index < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $index >= ${inputMaps.size}")
    require(!inputMaps(index).isFinalized,
            s"Cannot update an InputPSBTMap that is finalized, index: $index")

    val newElements =
      inputMaps(index).filterRecords(SigHashTypeKeyId) :+ SigHashType(hashType)

    val newInputMaps =
      inputMaps.updated(index, InputPSBTMap(newElements))
    PSBT(globalMap, newInputMaps, outputMaps)
  }

  def addSignature(
      pubKey: ECPublicKey,
      sig: ECDigitalSignature,
      inputIndex: Int): PSBT =
    addSignature(PartialSignature(pubKey.toPublicKeyBytes(), sig), inputIndex)

  def addSignature(
      partialSignature: PartialSignature[ECDigitalSignature],
      inputIndex: Int): PSBT =
    addSignatures(Vector(partialSignature), inputIndex)

  /** Adds all the PartialSignatures to the input map at the given index */
  def addSignatures(
      partialSignatures: Vector[PartialSignature[ECDigitalSignature]],
      inputIndex: Int): PSBT = {
    require(
      inputIndex < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $inputIndex >= ${inputMaps.size}")
    require(
      !inputMaps(inputIndex).isFinalized,
      s"Cannot update an InputPSBTMap that is finalized, index: $inputIndex")
    val intersect =
      inputMaps(inputIndex).partialSignatures.intersect(partialSignatures)
    val allSigs = inputMaps(inputIndex).partialSignatures ++ partialSignatures

    require(
      allSigs.groupBy(_.pubKey).values.forall(_.length == 1),
      s"Cannot add differing signatures for associated public keys, got: $intersect"
    )
    val newElements = inputMaps(inputIndex).elements ++ partialSignatures

    val newInputMaps =
      inputMaps.updated(inputIndex, InputPSBTMap(newElements))

    PSBT(globalMap, newInputMaps, outputMaps)
  }

  def addKeyPathSignature(
      partialSignature: PartialSignature[SchnorrDigitalSignature],
      inputIndex: Int): PSBT = {
    require(
      inputIndex < inputMaps.size,
      s"index must be less than the number of input maps present in the psbt, $inputIndex >= ${inputMaps.size}")
    require(
      !inputMaps(inputIndex).isFinalized,
      s"Cannot update an InputPSBTMap that is finalized, index: $inputIndex")
    val inputMap = inputMaps(inputIndex)
    val newInputMap: InputPSBTMap = inputMaps(
      inputIndex).keySpendSignatureOpt match {
      case Some(sig) =>
        if (sig.signature == partialSignature.signature)
          inputMap
        else
          sys.error(
            s"Cannot have 2 different keypath signatures for same input, got=${sig.signature} and ${partialSignature.value}")
      case None =>
        val trSpend = TRKeySpendSignature(partialSignature.signature)
        InputPSBTMap(inputMap.elements.appended(trSpend))
    }

    val newInputMaps = inputMaps.updated(inputIndex, newInputMap)
    PSBT(globalMap, newInputMaps, outputMaps)
  }

  def getPrevOutputMap(): PreviousOutputMap = {
    val map = transaction.inputs
      .zip(inputMaps)
      .map { case (input, inputMap) =>
        val prevTxOpt = inputMap.nonWitnessOrUnknownUTXOOpt
        val output = prevTxOpt match {
          case Some(prevTx) =>
            prevTx.transactionSpent.outputs(input.previousOutput.vout.toInt)
          case None =>
            inputMap.witnessUTXOOpt match {
              case Some(witnessUTXO) =>
                witnessUTXO.witnessUTXO
              case None =>
                throw new IllegalStateException(
                  "Cannot get previous output for input without previous transaction or witness UTXO")
            }
        }

        input.previousOutput -> output
      }
      .toMap

    PreviousOutputMap(map)
  }

  def verifyFinalizedInput(index: Int): Boolean = {
    val inputMap = inputMaps(index)
    require(inputMap.isFinalized, "Input must be finalized to verify")

    val utxoOpt = inputMap.nonWitnessOrUnknownUTXOOpt
    val wUtxoOpt = inputMap.witnessUTXOOpt match {
      case Some(wutxo) => Some(wutxo)
      case None =>
        utxoOpt match {
          case Some(utxo) =>
            val output = utxo.transactionSpent.outputs(
              transaction.inputs(index).previousOutput.vout.toInt)
            output.scriptPubKey match {
              case _: RawScriptPubKey => None
              case _: P2SHScriptPubKey =>
                inputMap.finalizedScriptSigOpt match {
                  case Some(
                        FinalizedScriptSig(scriptSig: P2SHScriptSignature)) =>
                    scriptSig.redeemScript match {
                      case _: NonWitnessScriptPubKey => None
                      case _: WitnessScriptPubKey => Some(WitnessUTXO(output))
                    }
                  case None | Some(_) => None
                }
              case _: WitnessScriptPubKeyV0 => Some(WitnessUTXO(output))
              case _: TaprootScriptPubKey   => None
              case u: UnassignedWitnessScriptPubKey =>
                sys.error(s"Cannot verifyFinalizedInput for $u")
            }
          case None => None
        }
    }

    val newInput = {
      val input = transaction.inputs(index)
      val scriptSigOpt = inputMap.finalizedScriptSigOpt
      val scriptSig =
        scriptSigOpt.map(_.scriptSig).getOrElse(EmptyScriptSignature)
      TransactionInput(input.previousOutput, scriptSig, input.sequence)
    }

    val tx = transaction.updateInput(index, newInput)

    wUtxoOpt match {
      case Some(wUtxo) =>
        inputMap.finalizedScriptWitnessOpt match {
          case Some(scriptWit) =>
            val wtx = {
              val wtx = WitnessTransaction.toWitnessTx(tx)
              wtx.updateWitness(index, scriptWit.scriptWitness)
            }
            val output = wUtxo.witnessUTXO

            val outputMap = output.scriptPubKey match {
              case _: NonWitnessScriptPubKey | _: WitnessScriptPubKeyV0 =>
                PreviousOutputMap.empty
              case _: TaprootScriptPubKey | _: UnassignedWitnessScriptPubKey =>
                getPrevOutputMap()
            }

            ScriptInterpreter.verifyInputScript(wtx, index, outputMap, output)
          case None =>
            false
        }
      case None =>
        utxoOpt match {
          case Some(utxo) =>
            val input = tx.inputs(index)
            val output =
              utxo.transactionSpent.outputs(input.previousOutput.vout.toInt)

            val outputMap = output.scriptPubKey match {
              case _: NonWitnessScriptPubKey | _: WitnessScriptPubKeyV0 =>
                PreviousOutputMap.empty
              case _: TaprootScriptPubKey | _: UnassignedWitnessScriptPubKey =>
                getPrevOutputMap()
            }

            ScriptInterpreter.verifyInputScript(tx, index, outputMap, output)
          case None =>
            false
        }
    }
  }

  /** Extracts the serialized from the serialized, fully signed transaction from
    * this PSBT and validates the script signatures using the ScriptInterpreter.
    * Only inputs for which UTXO records are present get validated.
    *
    * Note: This PSBT must be finalized.
    */
  def extractTransactionAndValidate: Try[Transaction] = {
    val outputMap = getPrevOutputMap()
    inputMaps.zipWithIndex.foldLeft(Try(extractTransaction)) {
      case (txT, (inputMap, index)) =>
        txT.flatMap { tx =>
          BitcoinScriptUtil.verifyPSBTInputScript(tx = tx,
                                                  inputMap = inputMap,
                                                  index = index,
                                                  outputMap = outputMap)
        }
    }
  }

  /** @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#transaction-extractor]]
    */
  def extractTransaction: Transaction = {
    if (isFinalized) {
      val newInputs =
        transaction.inputs.zip(inputMaps).map { case (input, inputMap) =>
          val scriptSigOpt = inputMap.finalizedScriptSigOpt
          val scriptSig =
            scriptSigOpt.map(_.scriptSig).getOrElse(EmptyScriptSignature)
          TransactionInput(input.previousOutput, scriptSig, input.sequence)
        }

      if (
        inputMaps
          .flatMap(_.elements)
          .exists(_.isInstanceOf[FinalizedScriptWitness])
      ) {
        val witness = inputMaps.zipWithIndex.foldLeft[TransactionWitness](
          EmptyWitness.fromInputs(transaction.inputs)) {
          case (witness, (inputMap, index)) =>
            inputMap.finalizedScriptWitnessOpt match {
              case None => witness
              case Some(
                    InputPSBTRecord.FinalizedScriptWitness(scriptWitness)) =>
                witness.updated(index, scriptWitness)
            }
        }
        WitnessTransaction(transaction.version,
                           newInputs,
                           transaction.outputs,
                           transaction.lockTime,
                           witness)
      } else {
        transaction match {
          case btx: NonWitnessTransaction =>
            BaseTransaction(btx.version, newInputs, btx.outputs, btx.lockTime)
          case wtx: WitnessTransaction =>
            WitnessTransaction(wtx.version,
                               newInputs,
                               wtx.outputs,
                               wtx.lockTime,
                               wtx.witness)
        }
      }
    } else {
      throw new IllegalStateException(
        "PSBT must be finalized in order to extract")
    }
  }
}

object PSBT extends Factory[PSBT] with StringFactory[PSBT] {

  // The known version of PSBTs this library can process defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#version-numbers
  final val knownVersions: Vector[UInt32] = Vector(UInt32.zero)

  // The magic bytes and separator defined by https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#specification
  final val magicBytes = hex"70736274ff"

  final val empty = fromUnsignedTx(EmptyTransaction)

  override def fromString(str: String): PSBT = {
    ByteVector.fromHex(str) match {
      case Some(hex) =>
        PSBT(hex)
      case None =>
        ByteVector.fromBase64(str) match {
          case Some(base64) =>
            PSBT(base64)
          case None =>
            throw new IllegalArgumentException(
              s"String given must be in base64 or hexadecimal, got: $str")
        }
    }
  }

  override def fromBytes(bytes: ByteVector): PSBT = {
    require(bytes.startsWith(magicBytes),
            s"A PSBT must start with the magic bytes $magicBytes, got: $bytes")

    val globalBytes = bytes.drop(magicBytes.size)

    val global: GlobalPSBTMap = GlobalPSBTMap.fromBytes(globalBytes)

    val tx = global.unsignedTransaction.transaction

    @tailrec
    def mapLoop[MapType <: PSBTMap[PSBTRecord]](
        bytes: ByteVector,
        numMaps: Int,
        accum: Vector[MapType],
        factory: Factory[MapType]): Vector[MapType] = {
      if (numMaps <= 0 || bytes.isEmpty) {
        accum
      } else {
        val newMap = factory.fromBytes(bytes)
        mapLoop(bytes.drop(newMap.bytes.size),
                numMaps - 1,
                accum :+ newMap,
                factory)
      }
    }

    val inputBytes = globalBytes.drop(global.bytes.size)

    val inputMaps = mapLoop[InputPSBTMap](inputBytes,
                                          tx.inputs.size,
                                          Vector.empty,
                                          InputPSBTMap)

    val outputBytes =
      inputBytes.drop(inputMaps.foldLeft(0)(_ + _.bytes.size.toInt))

    val outputMaps = mapLoop[OutputPSBTMap](outputBytes,
                                            tx.outputs.size,
                                            Vector.empty,
                                            OutputPSBTMap)

    val remainingBytes =
      outputBytes.drop(outputMaps.foldLeft(0)(_ + _.bytes.size.toInt))

    require(remainingBytes.isEmpty,
            s"The PSBT should be empty now, got: $remainingBytes")

    PSBT(global, inputMaps, outputMaps)
  }

  def fromBase64(base64: String): PSBT = {
    ByteVector.fromBase64(base64) match {
      case None =>
        throw new IllegalArgumentException(
          s"String given was not in base64 format, got: $base64")
      case Some(bytes) => fromBytes(bytes)
    }
  }

  /** Creates an empty PSBT with only the global transaction record filled
    * @param unsignedTx
    *   global transaction for the PSBT
    * @return
    *   Created PSBT
    */
  def fromUnsignedTx(unsignedTx: Transaction): PSBT = {
    require(unsignedTx.inputs.forall(_.scriptSignature == EmptyScriptSignature),
            s"The transaction must not have any signatures, got: $unsignedTx")

    val btx = unsignedTx match {
      case wtx: WitnessTransaction =>
        BaseTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime)
      case base: NonWitnessTransaction => base
    }
    val globalMap = GlobalPSBTMap(
      Vector(GlobalPSBTRecord.UnsignedTransaction(btx)))
    val inputMaps = unsignedTx.inputs.map(_ => InputPSBTMap.empty)
    val outputMaps = unsignedTx.outputs.map(_ => OutputPSBTMap.empty)

    PSBT(globalMap, inputMaps, outputMaps)
  }

  def fromUnsignedTxWithP2SHScript(tx: Transaction): PSBT = {
    val inputs = tx.inputs.toVector
    val utxInputs = inputs.map { input =>
      TransactionInput(input.previousOutput,
                       EmptyScriptSignature,
                       input.sequence)
    }
    val utx = BaseTransaction(tx.version, utxInputs, tx.outputs, tx.lockTime)
    val psbt = fromUnsignedTx(utx)

    val p2shScriptSigs = inputs
      .map(_.scriptSignature)
      .zipWithIndex
      .collect { case (p2sh: P2SHScriptSignature, index) =>
        (p2sh, index)
      }
      .filter(_._1.scriptSignatureNoRedeemScript == EmptyScriptSignature)

    p2shScriptSigs.foldLeft(psbt) { case (psbtSoFar, (p2sh, index)) =>
      psbtSoFar.addRedeemOrWitnessScriptToInput(p2sh.redeemScript, index)
    }
  }

  /** Constructs a full (ready to be finalized) but unfinalized PSBT from an
    * unsigned transaction and a SpendingInfoAndNonWitnessTxs
    */
  def fromUnsignedTxAndInputs(
      unsignedTx: Transaction,
      spendingInfoAndNonWitnessTxs: Vector[ScriptSignatureParams[InputInfo]])
      : PSBT = {
    fromUnsignedTxAndInputs(unsignedTx,
                            spendingInfoAndNonWitnessTxs,
                            finalized = false)
  }

  /** Constructs a finalized PSBT from an unsigned transaction and a
    * SpendingInfoAndNonWitnessTxs
    */
  def finalizedFromUnsignedTxAndInputs(
      unsignedTx: Transaction,
      spendingInfos: Vector[ScriptSignatureParams[InputInfo]]): PSBT = {
    fromUnsignedTxAndInputs(unsignedTx, spendingInfos, finalized = true)
  }

  private def fromUnsignedTxAndInputs(
      unsignedTx: Transaction,
      spendingInfos: Vector[ScriptSignatureParams[InputInfo]],
      finalized: Boolean): PSBT = {
    require(spendingInfos.length == unsignedTx.inputs.length,
            "Must have a SpendingInfo for every input")
    require(
      spendingInfos.zip(unsignedTx.inputs).forall { case (info, input) =>
        info.outPoint == input.previousOutput
      },
      "SpendingInfos must correspond to transaction inputs"
    )
    val emptySigTx = TxUtil.emptyAllScriptSigs(unsignedTx)
    val btx = emptySigTx match {
      case wtx: WitnessTransaction =>
        BaseTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime)
      case base: NonWitnessTransaction => base
    }

    val globalMap = GlobalPSBTMap(
      Vector(GlobalPSBTRecord.UnsignedTransaction(btx)))
    val inputMaps = spendingInfos.map { info =>
      if (finalized) {
        InputPSBTMap.finalizedFromSpendingInfo(info, unsignedTx)
      } else {
        InputPSBTMap.fromUTXOInfo(info, unsignedTx)
      }
    }
    val outputMaps = unsignedTx.outputs.map(_ => OutputPSBTMap.empty).toVector

    PSBT(globalMap, inputMaps, outputMaps)
  }
}
