package org.bitcoins.testkitcore.gen

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  P2SHNestedSegwitV0InputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto.{HashType, Sign}
import org.scalacheck.Gen

sealed abstract class CreditingTxGen {

  /** Minimum amount of outputs to generate */
  private val min = 1

  /** Maximum amount of outputs to generate */
  private val max = 3

  /** Note this generator does NOT generate outputs with negative values */
  private def nonEmptyOutputs: Gen[Seq[TransactionOutput]] =
    Gen.choose(1, 5).flatMap { n =>
      Gen.listOfN(n, TransactionGenerators.realisticOutput)
    }

  /** Generator for non-script hash based output */
  def nonSHOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    Gen.oneOf(
      p2pkOutput,
      p2pkhOutput,
      /*p2pkWithTimeoutOutput,*/
      multiSigOutput, /*cltvOutput,*/ csvOutput,
      multiSignatureWithTimeoutOutput,
      conditionalOutput,
      p2wpkhOutput
    )
  }

  def nonSHOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, nonSHOutput))

  def basicOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    Gen.oneOf(p2pkOutput, p2pkhOutput, multiSigOutput)
  }

  def nonP2WSHOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    //note, cannot put a p2wpkh here
    Gen.oneOf(p2pkOutput,
              p2pkhOutput,
              /*p2pkWithTimeoutOutput,*/
              multiSigOutput, /*cltvOutput,*/ csvOutput,
              multiSignatureWithTimeoutOutput,
              conditionalOutput)
  }

  def nonP2WSHOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, nonP2WSHOutput))

  def rawOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    Gen.oneOf(p2pkOutput,
              p2pkhOutput,
              p2pkWithTimeoutOutput,
              multiSigOutput,
              cltvOutput,
              csvOutput,
              multiSignatureWithTimeoutOutput,
              conditionalOutput)
  }

  def rawOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, rawOutput))

  def witOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    Gen.oneOf(p2wpkhOutput, p2wshOutput)
  }

  def witOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, witOutput))

  /** Only for use in constructing P2SH outputs */
  private def nonP2SHOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    Gen
      .oneOf(
        p2pkOutput,
        p2pkhOutput,
        p2pkWithTimeoutOutput,
        multiSigOutput, /*cltvOutput,*/ csvOutput,
        multiSignatureWithTimeoutOutput,
        conditionalOutput,
        p2wpkhOutput,
        p2wshOutput
      )
      .suchThat(output =>
        !ScriptGenerators.redeemScriptTooBig(output.output.scriptPubKey))
      .suchThat {
        case ScriptSignatureParams(
              P2SHNestedSegwitV0InputInfo(_, _, witness, _, _),
              _,
              _,
              _) =>
          witness.stack.exists(_.length > ScriptInterpreter.MAX_PUSH_SIZE)
        case _ => true
      }
  }

  private val nonCltvOutputGens = Vector(
    p2pkOutput,
    p2pkhOutput,
    multiSigOutput,
    p2shOutput,
    p2pkWithTimeoutOutput,
    csvOutput,
    multiSignatureWithTimeoutOutput,
    conditionalOutput,
    p2wpkhOutput,
    p2wshOutput
  )

  private val cltvOutputGens = Vector(cltvOutput)

  def output: Gen[ScriptSignatureParams[InputInfo]] =
    Gen.oneOf(nonCltvOutputGens).flatMap(identity)

  /** Either a list of non-CLTV outputs or a single CLTV output, with proportional probability */
  def outputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] = {
    val cltvGen = Gen
      .oneOf(cltvOutput, p2pkWithTimeoutOutput)
      .map { output =>
        Vector(output)
      }
    val nonCltvGen = Gen.choose(min, 5).flatMap(n => Gen.listOfN(n, output))

    val cltvSize = cltvOutputGens.length
    val nonCltvSize = nonCltvOutputGens.length

    // For some reason this breaks in scala 2.13 without the explicit type param
    Gen.frequency[Seq[ScriptSignatureParams[InputInfo]]](
      (cltvSize, cltvGen),
      (nonCltvSize, nonCltvGen))
  }

  /** Generates a crediting tx with a p2pk spk at the returned index */
  def p2pkOutput: Gen[ScriptSignatureParams[InputInfo]] =
    ScriptGenerators.p2pkScriptPubKey.flatMap { p2pk =>
      build(p2pk._1, Seq(p2pk._2), None, None)
    }

  /** Generates multiple crediting txs with p2pk spks at the returned index */
  def p2pkOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkOutput))
  }

  def p2pkWithTimeoutOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    ScriptGenerators.p2pkWithTimeoutScriptPubKey.flatMap { p2pkWithTimeout =>
      build(p2pkWithTimeout._1, Seq(p2pkWithTimeout._2.head), None, None)
    }
  }

  def p2pkWithTimeoutOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkWithTimeoutOutput))
  }

  /** Generates a transaction that has a p2pkh output at the returned index. This
    * output can be spent by the returned ECPrivateKey
    */
  def p2pkhOutput: Gen[ScriptSignatureParams[InputInfo]] =
    ScriptGenerators.p2pkhScriptPubKey.flatMap { p2pkh =>
      build(p2pkh._1, Seq(p2pkh._2), None, None)
    }

  /** Generates a sequence of p2pkh outputs at the returned index */
  def p2pkhOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkhOutput))
  }

  def multiSigOutput: Gen[ScriptSignatureParams[InputInfo]] =
    ScriptGenerators.multiSigScriptPubKey.flatMap { multisig =>
      build(multisig._1, multisig._2, None, None)
    }

  def multiSigOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, multiSigOutput))
  }

  def multiSignatureWithTimeoutOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    ScriptGenerators.multiSignatureWithTimeoutScriptPubKey.flatMap {
      case (conditional, keys) =>
        build(conditional, keys, None, None)
    }
  }

  def multiSignatureWithTimeoutOutputs: Gen[
    Seq[ScriptSignatureParams[InputInfo]]] = {
    Gen
      .choose(min, max)
      .flatMap(n => Gen.listOfN(n, multiSignatureWithTimeoutOutput))
  }

  def conditionalOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    ScriptGenerators
      .nonLocktimeConditionalScriptPubKey(ScriptGenerators.defaultMaxDepth)
      .flatMap { case (conditional, keys) =>
        build(conditional, keys, None, None)
      }
  }

  def conditionalOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, conditionalOutput))
  }

  def p2shOutput: Gen[ScriptSignatureParams[InputInfo]] =
    nonP2SHOutput.flatMap { o =>
      CryptoGenerators.hashType.map { hashType =>
        val oldOutput = o.output
        val redeemScript = o.output.scriptPubKey
        val p2sh = P2SHScriptPubKey(redeemScript)
        val updatedOutput = TransactionOutput(oldOutput.value, p2sh)
        val scriptWitnessOpt = InputInfo.getScriptWitness(o.inputInfo)
        val tc = TransactionConstants
        val oldOutputs = o.prevTransaction.outputs
        val updated = oldOutputs.updated(o.outPoint.vout.toInt, updatedOutput)
        val creditingTx =
          BaseTransaction(tc.validLockVersion, Nil, updated, tc.lockTime)

        ScriptSignatureParams(
          InputInfo(
            TransactionOutPoint(creditingTx.txId, o.outPoint.vout),
            updatedOutput,
            Some(redeemScript),
            scriptWitnessOpt,
            computeAllTrueConditionalPath(redeemScript, None, scriptWitnessOpt),
            o.signers.map(_.publicKey)
          ),
          creditingTx,
          o.signers,
          hashType
        )
      }
    }

  def p2shOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2shOutput))
  }

  def cltvOutput: Gen[ScriptSignatureParams[InputInfo]] =
    TransactionGenerators.spendableCLTVValues.flatMap { case (scriptNum, _) =>
      basicOutput.flatMap { o =>
        CryptoGenerators.hashType.map { hashType =>
          val oldOutput = o.output
          val csvSPK = CLTVScriptPubKey(scriptNum, oldOutput.scriptPubKey)
          val updatedOutput = TransactionOutput(oldOutput.value, csvSPK)
          val tc = TransactionConstants
          val oldOutputs = o.prevTransaction.outputs
          val updated =
            oldOutputs.updated(o.outPoint.vout.toInt, updatedOutput)
          val creditingTx =
            BaseTransaction(tc.validLockVersion, Nil, updated, tc.lockTime)

          ScriptSignatureParams(
            InputInfo(
              TransactionOutPoint(creditingTx.txId, o.outPoint.vout),
              updatedOutput,
              InputInfo.getRedeemScript(o.inputInfo),
              InputInfo.getScriptWitness(o.inputInfo),
              ConditionalPath.NoCondition,
              o.signers.map(_.publicKey)
            ),
            creditingTx,
            o.signers,
            hashType
          )
        }
      }
    }

  def cltvOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, cltvOutput))

  def csvOutput: Gen[ScriptSignatureParams[InputInfo]] =
    TransactionGenerators.spendableCSVValues.flatMap { case (scriptNum, _) =>
      basicOutput.flatMap { o =>
        CryptoGenerators.hashType.map { hashType =>
          val oldOutput = o.output
          val csvSPK = CSVScriptPubKey(scriptNum, oldOutput.scriptPubKey)
          val updatedOutput = TransactionOutput(oldOutput.value, csvSPK)
          val tc = TransactionConstants
          val oldOutputs = o.prevTransaction.outputs
          val updated =
            oldOutputs.updated(o.outPoint.vout.toInt, updatedOutput)
          val creditingTx =
            BaseTransaction(tc.validLockVersion, Nil, updated, tc.lockTime)

          ScriptSignatureParams(
            InputInfo(
              TransactionOutPoint(creditingTx.txId, o.outPoint.vout),
              updatedOutput,
              InputInfo.getRedeemScript(o.inputInfo),
              InputInfo.getScriptWitness(o.inputInfo),
              ConditionalPath.NoCondition,
              o.signers.map(_.publicKey)
            ),
            creditingTx,
            o.signers,
            hashType
          )
        }
      }
    }

  def csvOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, csvOutput))

  def p2wpkhOutput: Gen[ScriptSignatureParams[InputInfo]] =
    ScriptGenerators.p2wpkhSPKV0.flatMap { witSPK =>
      val scriptWit = P2WPKHWitnessV0(witSPK._2.head.publicKey)
      build(witSPK._1, witSPK._2, None, Some(scriptWit))
    }

  def p2wpkhOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2wpkhOutput))

  def p2wshOutput: Gen[ScriptSignatureParams[InputInfo]] =
    nonP2WSHOutput
      .suchThat(output =>
        !ScriptGenerators.redeemScriptTooBig(output.output.scriptPubKey))
      .flatMap { case ScriptSignatureParams(info, _, signers, _) =>
        val spk = info.scriptPubKey
        spk match {
          case rspk: RawScriptPubKey =>
            val scriptWit = P2WSHWitnessV0(rspk)
            val witSPK = P2WSHWitnessSPKV0(rspk)
            build(witSPK, signers, None, Some(scriptWit))
          case _ =>
            throw new IllegalArgumentException(
              "nonP2WSHOutput created a non RawScriptPubKey")
        }
      }

  def p2wshOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2wshOutput))

  /** A nested output is a p2sh/p2wsh wrapped output */
  def nestedOutput: Gen[ScriptSignatureParams[InputInfo]] = {
    Gen.oneOf(p2wshOutput, p2shOutput)
  }

  def nestedOutputs: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, nestedOutput))

  def random: Gen[ScriptSignatureParams[InputInfo]] =
    nonEmptyOutputs.flatMap { outputs =>
      Gen.choose(0, outputs.size - 1).flatMap { case outputIndex: Int =>
        ScriptGenerators.scriptPubKey.flatMap { case (spk, keys) =>
          WitnessGenerators.scriptWitness.flatMap { case wit: ScriptWitness =>
            CryptoGenerators.hashType.map { case hashType: HashType =>
              val tc = TransactionConstants
              val signers: Vector[Sign] = keys.toVector
              val creditingTx =
                BaseTransaction(tc.validLockVersion, Nil, outputs, tc.lockTime)
              ScriptSignatureParams(
                InputInfo(
                  TransactionOutPoint(creditingTx.txId,
                                      UInt32.apply(outputIndex)),
                  TransactionOutput(
                    creditingTx.outputs(outputIndex).value,
                    creditingTx.outputs(outputIndex).scriptPubKey),
                  Some(spk),
                  Some(wit),
                  ConditionalPath.NoCondition,
                  signers.map(_.publicKey)
                ),
                creditingTx,
                signers,
                hashType
              )
            }
          }
        }
      }
    }

  def randoms: Gen[Seq[ScriptSignatureParams[InputInfo]]] =
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, random))

  private def computeAllTrueConditionalPath(
      spk: ScriptPubKey,
      redeemScript: Option[ScriptPubKey],
      scriptWitness: Option[ScriptWitness]): ConditionalPath = {
    spk match {
      case conditional: ConditionalScriptPubKey =>
        ConditionalPath.ConditionTrue(
          computeAllTrueConditionalPath(conditional.trueSPK, None, None))
      case _: P2PKWithTimeoutScriptPubKey => ConditionalPath.nonNestedTrue
      case lockTimeScriptPubKey: LockTimeScriptPubKey =>
        computeAllTrueConditionalPath(lockTimeScriptPubKey.nestedScriptPubKey,
                                      None,
                                      None)
      case _: RawScriptPubKey | _: P2WPKHWitnessSPKV0 =>
        ConditionalPath.NoCondition
      case _: P2SHScriptPubKey =>
        redeemScript match {
          case None =>
            throw new IllegalArgumentException(
              "Expected redeem script for P2SH")
          case Some(script) =>
            computeAllTrueConditionalPath(script, None, scriptWitness)
        }
      case _: P2WSHWitnessSPKV0 =>
        scriptWitness match {
          case Some(witness: P2WSHWitnessV0) =>
            computeAllTrueConditionalPath(witness.redeemScript, None, None)
          case _ =>
            throw new IllegalArgumentException(
              "Expected P2WSHWitness for P2WSH")
        }
      case _: TaprootScriptPubKey =>
        throw new IllegalArgumentException(
          s"Unexpected (unsupported) taproot SPK: $spk")
      case _: UnassignedWitnessScriptPubKey =>
        throw new IllegalArgumentException(
          s"Unexpected unassigned witness SPK: $spk")
    }
  }

  private def build(
      spk: ScriptPubKey,
      signers: Seq[Sign],
      redeemScript: Option[ScriptPubKey],
      scriptWitness: Option[ScriptWitness]): Gen[
    ScriptSignatureParams[InputInfo]] =
    nonEmptyOutputs.flatMap { outputs =>
      CryptoGenerators.hashType.flatMap { hashType =>
        Gen.choose(0, outputs.size - 1).map { idx =>
          val old = outputs(idx)
          val updated = outputs.updated(idx, TransactionOutput(old.value, spk))
          val tc = TransactionConstants
          val btx = BaseTransaction(tc.version, Nil, updated, tc.lockTime)
          ScriptSignatureParams(
            InputInfo(
              TransactionOutPoint(btx.txId, UInt32.apply(idx)),
              TransactionOutput(old.value, spk),
              redeemScript,
              scriptWitness,
              computeAllTrueConditionalPath(spk, redeemScript, scriptWitness),
              signers.toVector.map(_.publicKey)
            ),
            btx,
            signers.toVector,
            hashType
          )
        }
      }
    }

  def inputsAndOutputs(
      outputsToUse: Gen[Seq[ScriptSignatureParams[InputInfo]]] = outputs,
      destinationGenerator: CurrencyUnit => Gen[Seq[TransactionOutput]] =
        TransactionGenerators.smallOutputs): Gen[
    (Seq[ScriptSignatureParams[InputInfo]], Seq[TransactionOutput])] = {
    inputsAndP2SHOutputs(
      outputsToUse,
      destinationGenerator.andThen(_.map(_.map(x => (x, ScriptPubKey.empty)))))
      .map(x => (x._1, x._2.map(_._1)))
  }

  def inputsAndP2SHOutputs(
      outputsToUse: Gen[Seq[ScriptSignatureParams[InputInfo]]] = outputs,
      destinationGenerator: CurrencyUnit => Gen[
        Seq[(TransactionOutput, ScriptPubKey)]] =
        TransactionGenerators.smallP2SHOutputs): Gen[(
      Seq[ScriptSignatureParams[InputInfo]],
      Seq[(TransactionOutput, ScriptPubKey)])] = {
    outputsToUse
      .flatMap { creditingTxsInfo =>
        val creditingOutputs = creditingTxsInfo.map(c => c.output)
        val creditingOutputsAmt = creditingOutputs.map(_.value)
        val totalAmount = creditingOutputsAmt.fold(CurrencyUnits.zero)(_ + _)

        destinationGenerator(totalAmount).map { destinations =>
          (creditingTxsInfo, destinations)
        }
      }
      .suchThat(_._1.nonEmpty)
  }
}

object CreditingTxGen extends CreditingTxGen {}
