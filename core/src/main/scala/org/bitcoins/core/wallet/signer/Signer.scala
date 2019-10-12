package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.TxBuilderError
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer {

  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param signers the [[Signer]] needed to sign the utxo
    * @param output the utxo we are spending
    * @param unsignedTx the unsigned transaction which is spending the utxo
    * @param inputIndex the input index inside of the unsigned transaction which spends the utxo
    * @param hashType the signature hashing algorithm we should use to sign the utxo
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @return
    */
  def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent]

  def doSign(
      sigComponent: TxSigComponent,
      sign: ByteVector => Future[ECDigitalSignature],
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[ECDigitalSignature] = {
    if (isDummySignature) {
      Future.successful(DummyECDigitalSignature)
    } else {
      TransactionSignatureCreator.createSig(sigComponent, sign, hashType)
    }
  }
}

sealed abstract class OverridesForNestedSigning {
  def txSigComponentOpt: Option[TxSigComponent]
  def scriptPubKeyOpt: Option[ScriptPubKey]
}

object NestedSigning {
  case object NoOverrides extends OverridesForNestedSigning {
    override val txSigComponentOpt: Option[TxSigComponent] = None
    override val scriptPubKeyOpt: Option[ScriptPubKey] = None
  }

  case class P2WSHOverrides(
      externalSigComponent: TxSigComponent,
      nestedSPK: ScriptPubKey)
      extends OverridesForNestedSigning {
    override val txSigComponentOpt: Option[TxSigComponent] = Some(
      externalSigComponent)
    override val scriptPubKeyOpt: Option[ScriptPubKey] = Some(nestedSPK)
  }
}

/** Represents all signers for the bitcoin protocol, we could add another network later like litecoin */
sealed abstract class BitcoinSigner extends Signer

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner {

  override def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(signers,
         output,
         unsignedTx,
         inputIndex,
         hashType,
         isDummySignature,
         overrides = NestedSigning.NoOverrides)
  }

  def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = overrides.scriptPubKeyOpt match {
      case None               => output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign: ByteVector => Future[ECDigitalSignature] =
        signers.head.signFunction
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      val flags = Policy.standardFlags

      val signed: Future[TxSigComponent] = spk match {
        case _: P2PKScriptPubKey =>
          val sigComponent = overrides.txSigComponentOpt match {
            case None =>
              BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
            case Some(sigComponent) => sigComponent
          }
          val signature = doSign(sigComponent, sign, hashType, isDummySignature)
          signature.map { sig =>
            val p2pkScriptSig = P2PKScriptSignature(sig)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               p2pkScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
            val signedTx = unsignedTx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                signedInputs,
                                btx.outputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   signedInputs,
                                   wtx.outputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
            BaseTxSigComponent(signedTx, inputIndex, output, flags)
          }
        case lock: LockTimeScriptPubKey =>
          lock.nestedScriptPubKey match {
            case _: P2PKScriptPubKey =>
              val sigComponent = overrides.txSigComponentOpt match {
                case None =>
                  BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
                case Some(sigComponent) => sigComponent
              }
              val signature =
                doSign(sigComponent, sign, hashType, isDummySignature)
              signature.map { sig =>
                val p2pkScriptSig = P2PKScriptSignature(sig)
                val signedInput = TransactionInput(unsignedInput.previousOutput,
                                                   p2pkScriptSig,
                                                   unsignedInput.sequence)
                val signedInputs =
                  unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
                val signedTx = unsignedTx match {
                  case btx: BaseTransaction =>
                    BaseTransaction(btx.version,
                                    signedInputs,
                                    btx.outputs,
                                    btx.lockTime)
                  case wtx: WitnessTransaction =>
                    WitnessTransaction(wtx.version,
                                       signedInputs,
                                       wtx.outputs,
                                       wtx.lockTime,
                                       wtx.witness)
                }
                BaseTxSigComponent(signedTx, inputIndex, output, flags)
              }

            case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey |
                _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
                _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
                _: CLTVScriptPubKey | _: CSVScriptPubKey |
                _: WitnessCommitment | EmptyScriptPubKey |
                _: UnassignedWitnessScriptPubKey =>
              Future.fromTry(TxBuilderError.WrongSigner)
          }
        case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey |
            _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner extends BitcoinSigner {

  override def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(signers,
         output,
         unsignedTx,
         inputIndex,
         hashType,
         isDummySignature,
         overrides = NestedSigning.NoOverrides)
  }

  def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = overrides.scriptPubKeyOpt match {
      case None               => output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign = signers.head.signFunction
      val pubKey = signers.head.publicKey
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      val flags = Policy.standardFlags

      val signed: Future[TxSigComponent] = spk match {
        case p2pkh: P2PKHScriptPubKey =>
          if (p2pkh != P2PKHScriptPubKey(pubKey)) {
            Future.fromTry(TxBuilderError.WrongPublicKey)
          } else {
            val sigComponent = overrides.txSigComponentOpt match {
              case None =>
                BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
              case Some(sigComponent) => sigComponent
            }
            val signature =
              doSign(sigComponent, sign, hashType, isDummySignature)
            signature.map { sig =>
              val p2pkhScriptSig = P2PKHScriptSignature(sig, pubKey)
              val signedInput = TransactionInput(unsignedInput.previousOutput,
                                                 p2pkhScriptSig,
                                                 unsignedInput.sequence)
              val signedInputs =
                unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
              val signedTx = unsignedTx match {
                case btx: BaseTransaction =>
                  BaseTransaction(btx.version,
                                  signedInputs,
                                  btx.outputs,
                                  btx.lockTime)
                case wtx: WitnessTransaction =>
                  WitnessTransaction(wtx.version,
                                     signedInputs,
                                     wtx.outputs,
                                     wtx.lockTime,
                                     wtx.witness)
              }
              BaseTxSigComponent(signedTx, inputIndex, output, flags)
            }
          }
        case lock: LockTimeScriptPubKey =>
          lock.nestedScriptPubKey match {
            case p2pkh: P2PKHScriptPubKey =>
              if (p2pkh != P2PKHScriptPubKey(pubKey)) {
                Future.fromTry(TxBuilderError.WrongPublicKey)
              } else {
                val sigComponent = overrides.txSigComponentOpt match {
                  case None =>
                    BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
                  case Some(sigComponent) => sigComponent
                }
                val signature =
                  doSign(sigComponent, sign, hashType, isDummySignature)
                signature.map { sig =>
                  val p2pkhScriptSig = P2PKHScriptSignature(sig, pubKey)
                  val signedInput =
                    TransactionInput(unsignedInput.previousOutput,
                                     p2pkhScriptSig,
                                     unsignedInput.sequence)
                  val signedInputs =
                    unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
                  val signedTx = unsignedTx match {
                    case btx: BaseTransaction =>
                      BaseTransaction(btx.version,
                                      signedInputs,
                                      btx.outputs,
                                      btx.lockTime)
                    case wtx: WitnessTransaction =>
                      WitnessTransaction(wtx.version,
                                         signedInputs,
                                         wtx.outputs,
                                         wtx.lockTime,
                                         wtx.witness)
                  }
                  BaseTxSigComponent(signedTx, inputIndex, output, flags)
                }
              }
            case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey |
                _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
                _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
                _: CLTVScriptPubKey | _: CSVScriptPubKey |
                _: WitnessCommitment | EmptyScriptPubKey |
                _: UnassignedWitnessScriptPubKey =>
              Future.fromTry(TxBuilderError.WrongSigner)
          }
        case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey |
            _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class MultiSigSigner extends BitcoinSigner {

  override def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(signers,
         output,
         unsignedTx,
         inputIndex,
         hashType,
         isDummySignature,
         overrides = NestedSigning.NoOverrides)
  }

  def sign(
      signersWithPubKeys: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean,
      overrides: OverridesForNestedSigning)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = overrides.scriptPubKeyOpt match {
      case None               => output.scriptPubKey
      case Some(nestedScript) => nestedScript
    }
    val signers = signersWithPubKeys.map(_.signFunction)
    val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
    val flags = Policy.standardFlags

    val signed: Future[TxSigComponent] = spk match {
      case multiSigSPK: MultiSignatureScriptPubKey =>
        val requiredSigs = multiSigSPK.requiredSigs
        if (signers.size < requiredSigs) {
          Future.fromTry(TxBuilderError.WrongSigner)
        } else {
          val sigComponent = overrides.txSigComponentOpt match {
            case None =>
              BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
            case Some(sigComponent) => sigComponent
          }
          val signaturesNested = 0
            .until(requiredSigs)
            .map(i =>
              doSign(sigComponent, signers(i), hashType, isDummySignature))
          val signatures = Future.sequence(signaturesNested)
          signatures.map { sigs =>
            val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               multiSigScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
            val signedTx = unsignedTx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                signedInputs,
                                btx.outputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   signedInputs,
                                   wtx.outputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
            BaseTxSigComponent(signedTx,
                               inputIndex,
                               output,
                               Policy.standardFlags)
          }
        }
      case lock: LockTimeScriptPubKey =>
        val nested = lock.nestedScriptPubKey
        val multiSigSPK = nested match {
          case m: MultiSignatureScriptPubKey => Future.successful(m)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
              _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
              _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
              _: CLTVScriptPubKey | _: CSVScriptPubKey |
              _: UnassignedWitnessScriptPubKey | _: NonStandardScriptPubKey |
              _: WitnessCommitment | EmptyScriptPubKey =>
            Future.fromTry(TxBuilderError.WrongSigner)
        }
        multiSigSPK.flatMap { mSPK =>
          val requiredSigs = mSPK.requiredSigs
          val sigComponent = overrides.txSigComponentOpt match {
            case None =>
              BaseTxSigComponent(unsignedTx, inputIndex, output, flags)
            case Some(sigComponent) => sigComponent
          }
          val signatures: Future[Seq[ECDigitalSignature]] =
            if (signers.size < requiredSigs) {
              Future.fromTry(TxBuilderError.WrongSigner)
            } else {
              val sigs = 0.until(requiredSigs).map { i =>
                doSign(sigComponent, signers(i), hashType, isDummySignature)
              }
              Future.sequence(sigs)
            }
          val signedTxSigComp = signatures.map { sigs =>
            val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               multiSigScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
            val signedTx = unsignedTx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                signedInputs,
                                btx.outputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   signedInputs,
                                   wtx.outputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
            BaseTxSigComponent(signedTx, inputIndex, output, flags)
          }
          signedTxSigComp
        }
      case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey |
          _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
          _: NonStandardScriptPubKey | _: WitnessCommitment |
          _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey =>
        Future.fromTry(TxBuilderError.WrongSigner)
    }
    signed
  }
}

object MultiSigSigner extends MultiSigSigner

sealed abstract class P2WPKHSigner extends BitcoinSigner {
  override def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] =
    unsignedTx match {
      case wtx: WitnessTransaction =>
        if (signers.size != 1) {
          Future.fromTry(TxBuilderError.TooManySigners)
        } else {

          val sign = signers.head.signFunction

          val pubKey = signers.head.publicKey

          val unsignedScriptWit = P2WPKHWitnessV0(pubKey)

          val unsignedTxWitness = TransactionWitness(
            wtx.witness.witnesses.updated(inputIndex.toInt, unsignedScriptWit))

          val unsignedWtx = WitnessTransaction(wtx.version,
                                               wtx.inputs,
                                               wtx.outputs,
                                               wtx.lockTime,
                                               unsignedTxWitness)

          val witSPK = output.scriptPubKey match {
            case p2wpkh: P2WPKHWitnessSPKV0 =>
              if (p2wpkh != P2WPKHWitnessSPKV0(pubKey)) {
                Future.fromTry(TxBuilderError.WrongPublicKey)
              } else Future.successful(p2wpkh)
            case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
                _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
                _: CLTVScriptPubKey | _: CSVScriptPubKey |
                _: WitnessCommitment | EmptyScriptPubKey |
                _: UnassignedWitnessScriptPubKey =>
              Future.fromTry(TxBuilderError.NonWitnessSPK)
          }

          witSPK.flatMap { w =>
            val witOutput = TransactionOutput(output.value, w)

            val wtxComp = WitnessTxSigComponentRaw(unsignedWtx,
                                                   inputIndex,
                                                   witOutput,
                                                   Policy.standardFlags)

            val signature = doSign(wtxComp, sign, hashType, isDummySignature)

            signature.map { sig =>
              val scriptWitness = P2WPKHWitnessV0(pubKey, sig)
              val signedTxWitness =
                wtx.witness.updated(inputIndex.toInt, scriptWitness)
              val signedTx = WitnessTransaction(unsignedWtx.version,
                                                unsignedWtx.inputs,
                                                unsignedWtx.outputs,
                                                unsignedWtx.lockTime,
                                                signedTxWitness)
              WitnessTxSigComponentRaw(signedTx,
                                       inputIndex,
                                       witOutput,
                                       Policy.standardFlags)
            }

          }

        }
      case btx: BaseTransaction =>
        val wtx = WitnessTransaction(btx.version,
                                     btx.inputs,
                                     btx.outputs,
                                     btx.lockTime,
                                     EmptyWitness)
        sign(signers, output, wtx, inputIndex, hashType, isDummySignature)
    }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSigner extends BitcoinSigner {
  override def sign(
      signers: Seq[Sign],
      output: TransactionOutput,
      unsignedTx: Transaction,
      inputIndex: UInt32,
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val spk = output.scriptPubKey
    val flags = Policy.standardFlags

    spk match {
      case _: P2WSHWitnessSPKV0 =>
        val wtx = unsignedTx match {
          case btx: BaseTransaction =>
            WitnessTransaction(btx.version,
                               btx.inputs,
                               btx.outputs,
                               btx.lockTime,
                               EmptyWitness)
          case wtx: WitnessTransaction => wtx
        }
        val redeemScript = wtx.witness.witnesses(inputIndex.toInt) match {
          case x: P2WSHWitnessV0 => Future.successful(x.redeemScript)
          case _: P2WPKHWitnessV0 =>
            Future.fromTry(TxBuilderError.NoRedeemScript)
          case EmptyScriptWitness => Future.fromTry(TxBuilderError.NoWitness)
        }
        val sigComponent =
          WitnessTxSigComponentRaw(wtx, inputIndex, output, flags)
        val signedSigComponentF = redeemScript.flatMap {
          case p2pk: P2PKScriptPubKey =>
            P2PKSigner.sign(signers,
                            output,
                            wtx,
                            inputIndex,
                            hashType,
                            isDummySignature,
                            overrides =
                              NestedSigning.P2WSHOverrides(sigComponent, p2pk))
          case p2pkh: P2PKHScriptPubKey =>
            P2PKHSigner.sign(
              signers,
              output,
              wtx,
              inputIndex,
              hashType,
              isDummySignature,
              overrides = NestedSigning.P2WSHOverrides(sigComponent, p2pkh))
          case multiSig: MultiSignatureScriptPubKey =>
            MultiSigSigner.sign(
              signers,
              output,
              wtx,
              inputIndex,
              hashType,
              isDummySignature,
              overrides = NestedSigning.P2WSHOverrides(sigComponent, multiSig))
          case lock: LockTimeScriptPubKey =>
            lock.nestedScriptPubKey match {
              case _: P2PKScriptPubKey =>
                P2PKSigner.sign(
                  signers,
                  output,
                  wtx,
                  inputIndex,
                  hashType,
                  isDummySignature,
                  overrides = NestedSigning.P2WSHOverrides(sigComponent, lock))
              case _: P2PKHScriptPubKey =>
                P2PKHSigner.sign(
                  signers,
                  output,
                  wtx,
                  inputIndex,
                  hashType,
                  isDummySignature,
                  overrides = NestedSigning.P2WSHOverrides(sigComponent, lock))
              case _: MultiSignatureScriptPubKey =>
                MultiSigSigner.sign(
                  signers,
                  output,
                  wtx,
                  inputIndex,
                  hashType,
                  isDummySignature,
                  overrides = NestedSigning.P2WSHOverrides(sigComponent, lock))
              case _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
                  _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
                  _: CLTVScriptPubKey | _: CSVScriptPubKey |
                  _: WitnessCommitment | EmptyScriptPubKey |
                  _: UnassignedWitnessScriptPubKey =>
                Future.fromTry(TxBuilderError.WrongSigner)
            }
          case _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
              _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
              _: WitnessCommitment | EmptyScriptPubKey |
              _: UnassignedWitnessScriptPubKey =>
            Future.fromTry(TxBuilderError.WrongSigner)
        }

        val scriptWitF = signedSigComponentF.flatMap { signedSigComponent =>
          redeemScript.map { rs =>
            P2WSHWitnessV0(rs, signedSigComponent.scriptSignature)
          }
        }

        scriptWitF.map { scriptWit =>
          val signedWitness =
            wtx.witness.updated(inputIndex.toInt, scriptWit)
          val signedWTx = WitnessTransaction(wtx.version,
                                             wtx.inputs,
                                             wtx.outputs,
                                             wtx.lockTime,
                                             signedWitness)
          WitnessTxSigComponentRaw(signedWTx, inputIndex, output, flags)
        }

      case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
          _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey |
          _: WitnessCommitment | _: LockTimeScriptPubKey | EmptyScriptPubKey |
          _: UnassignedWitnessScriptPubKey =>
        Future.fromTry(TxBuilderError.WrongSigner)
    }
  }
}
object P2WSHSigner extends P2WSHSigner
