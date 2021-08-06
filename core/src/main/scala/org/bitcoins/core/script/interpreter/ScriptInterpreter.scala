package org.bitcoins.core.script.interpreter

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script._
import org.bitcoins.core.script.arithmetic._
import org.bitcoins.core.script.bitwise._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.flag._
import org.bitcoins.core.script.locktime.{
  LockTimeInterpreter,
  OP_CHECKLOCKTIMEVERIFY,
  OP_CHECKSEQUENCEVERIFY
}
import org.bitcoins.core.script.reserved._
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.splice._
import org.bitcoins.core.script.stack._
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.util._
import org.bitcoins.crypto.SchnorrPublicKey

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/** Created by chris on 1/6/16.
  */
sealed abstract class ScriptInterpreter {

  /** Currently bitcoin core limits the maximum number of non-push operations per script
    * to 201
    */
  private lazy val MAX_SCRIPT_OPS = 201

  /** We cannot push an element larger than 520 bytes onto the stack */
  val MAX_PUSH_SIZE: Int = 520

  /** Runs an entire script though our script programming language and
    * returns a [[org.bitcoins.core.script.result.ScriptResult ScriptResult]]
    * indicating if the script was valid, or if not what error it encountered
    */
  def run(program: PreExecutionScriptProgram): ScriptResult = {
    val scriptPubKey = program.txSignatureComponent.scriptPubKey
    val flags = program.flags

    val p2shEnabled = ScriptFlagUtil.p2shEnabled(flags)
    val segwitEnabled = ScriptFlagUtil.segWitEnabled(flags)

    val executedProgram: ExecutedScriptProgram =
      programFlagsViolated(program) match {
        case Some(err) => program.failExecution(err)
        case None =>
          val scriptSigExecutedProgram = executeProgram(program)

          val sigComponent = scriptSigExecutedProgram.txSignatureComponent
          val scriptPubKeyProgram = PreExecutionScriptProgram(
            txSignatureComponent = sigComponent,
            stack = scriptSigExecutedProgram.stack,
            script = sigComponent.scriptPubKey.asm.toList,
            originalScript = sigComponent.scriptPubKey.asm.toList,
            altStack = Nil,
            flags = scriptSigExecutedProgram.flags
          )

          val scriptPubKeyExecutedProgram: ExecutedScriptProgram =
            executeProgram(scriptPubKeyProgram)
          if (scriptSigExecutedProgram.error.isDefined) {
            scriptSigExecutedProgram
          } else if (
            scriptPubKeyExecutedProgram.error.isDefined || scriptPubKeyExecutedProgram.stackTopIsFalse
          ) {
            scriptPubKeyExecutedProgram
          } else {
            scriptPubKey match {
              case witness: WitnessScriptPubKey =>
                if (segwitEnabled)
                  executeSegWitScript(scriptPubKeyExecutedProgram, witness).get
                else scriptPubKeyExecutedProgram
              case _: P2SHScriptPubKey =>
                if (p2shEnabled) executeP2shScript(scriptSigExecutedProgram)
                else scriptPubKeyExecutedProgram
              case _: P2PKHScriptPubKey | _: P2PKScriptPubKey |
                  _: P2PKWithTimeoutScriptPubKey |
                  _: MultiSignatureScriptPubKey | _: CSVScriptPubKey |
                  _: CLTVScriptPubKey | _: ConditionalScriptPubKey |
                  _: NonStandardScriptPubKey | _: WitnessCommitment |
                  EmptyScriptPubKey =>
                scriptPubKeyExecutedProgram
            }
          }
      }

    val result = evaluateExecutedScriptProgram(program, executedProgram)
    result
  }

  private def programFlagsViolated(
      program: PreExecutionScriptProgram): Option[ScriptError] = {
    val scriptSig = program.txSignatureComponent.scriptSignature
    val flags = program.flags
    val p2shEnabled = ScriptFlagUtil.p2shEnabled(flags)

    if (
      ScriptFlagUtil.requirePushOnly(flags)
      && !BitcoinScriptUtil.isPushOnly(program.script)
    ) {
      Some(ScriptErrorSigPushOnly)
    } else {
      scriptSig match {
        case _: P2SHScriptSignature
            if p2shEnabled && !BitcoinScriptUtil.isPushOnly(scriptSig.asm) =>
          Some(ScriptErrorSigPushOnly)
        case _: ScriptSignature => None
      }
    }
  }

  private def evaluateExecutedScriptProgram(
      program: PreExecutionScriptProgram,
      executedProgram: ExecutedScriptProgram): ScriptResult = {
    executedProgram.error match {
      case Some(err) => err
      case None =>
        if (hasUnexpectedWitness(program)) {
          //note: the 'program' value we pass above is intentional, we need to check the original program
          //as the 'executedProgram' may have had the scriptPubKey value changed to the rebuilt ScriptPubKey of the witness program
          ScriptErrorWitnessUnexpected
        } else if (executedProgram.stackTopIsTrue) {
          if (program.flags.contains(ScriptVerifyCleanStack)) {
            //require that the stack after execution has exactly one element on it
            if (executedProgram.stack.size == 1) {
              ScriptOk
            } else {
              ScriptErrorCleanStack
            }
          } else {
            ScriptOk
          }
        } else {
          ScriptErrorEvalFalse
        }
    }
  }

  /** Runs the given [[org.bitcoins.core.script.PreExecutionScriptProgram PreExecutionScriptProgram]] and
    * return if that script was valid or not
    */
  def runVerify(p: PreExecutionScriptProgram): Boolean = {
    ScriptInterpreter.run(p) == ScriptOk
  }

  /** Every given [[org.bitcoins.core.script.PreExecutionScriptProgram PreExecutionScriptProgram]] and returns
    * it's [[org.bitcoins.core.script.result.ScriptResult ScriptResult]]
    */
  def runAll(programs: Seq[PreExecutionScriptProgram]): Seq[ScriptResult] = {
    programs.map(p => ScriptInterpreter.run(p))
  }

  /** Runs all the given [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] and return
    * if it is valid or not
    */
  def runAllVerify(programs: Seq[PreExecutionScriptProgram]): Boolean = {
    !programs.exists(p => ScriptInterpreter.run(p) != ScriptOk)
  }

  def verifyInputScript(
      transaction: Transaction,
      inputIndex: Long,
      outputMap: PreviousOutputMap,
      prevOut: TransactionOutput): Boolean = {
    val sigComponent = TxSigComponent(
      transaction,
      UInt32(inputIndex),
      prevOut,
      outputMap,
      Policy.standardFlags
    )
    ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent))
  }

  def verifyTransaction(
      transaction: Transaction,
      outputMap: PreviousOutputMap): Boolean = {
    require(
      transaction.inputs.size == outputMap.size,
      s"There must be a prevOut for every input in the transaction, got ${outputMap.size}")

    outputMap.zipWithIndex.forall { case ((_, prevOut), index) =>
      verifyInputScript(transaction, index, outputMap, prevOut)
    }
  }

  /** Helper function to actually run a p2sh script */
  private def runP2SH(
      p: ExecutedScriptProgram,
      s: ScriptPubKey): ExecutedScriptProgram = {

    val p2shRedeemScriptProgram = PreExecutionScriptProgram(
      txSignatureComponent = p.txSignatureComponent,
      stack = p.stack.tail,
      script = s.asm.toList,
      originalScript = p.originalScript,
      altStack = Nil,
      flags = p.flags
    )

    /*ScriptProgram(p.txSignatureComponent, stack.tail,s.asm)*/
    if (
      ScriptFlagUtil.requirePushOnly(
        p2shRedeemScriptProgram.flags) && !BitcoinScriptUtil
        .isPushOnly(s.asm)
    ) {
      p2shRedeemScriptProgram.failExecution(ScriptErrorSigPushOnly)
    } else executeProgram(p2shRedeemScriptProgram)
  }

  /** P2SH scripts are unique in their evaluation, first the scriptSignature must be added to the stack, next the
    * p2sh scriptPubKey must be run to make sure the serialized redeem script hashes to the value found in the p2sh
    * scriptPubKey, then finally the serialized redeemScript is decoded and run with the arguments in the p2sh script signature
    * a p2sh script returns true if both of those intermediate steps evaluate to true
    *
    * @param scriptPubKeyExecutedProgram the program with the script signature pushed onto the stack
    * @return the executed program
    */
  private def executeP2shScript(
      scriptPubKeyExecutedProgram: ExecutedScriptProgram): ExecutedScriptProgram = {
    val flags = scriptPubKeyExecutedProgram.flags

    val segwitEnabled = ScriptFlagUtil.segWitEnabled(flags)

    val scriptSig =
      scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
    val scriptSigAsm: Seq[ScriptToken] = scriptSig.asm
    //need to check if the scriptSig is push only as required by bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1419
    if (!BitcoinScriptUtil.isPushOnly(scriptSigAsm)) {
      scriptPubKeyExecutedProgram.failExecution(ScriptErrorSigPushOnly)
    } else if (scriptPubKeyExecutedProgram.error.isDefined) {
      scriptPubKeyExecutedProgram
    } else {
      if (scriptPubKeyExecutedProgram.stackTopIsTrue) {

        //we need to run the deserialized redeemScript & the scriptSignature without the serialized redeemScript
        val stack = scriptPubKeyExecutedProgram.stack

        val redeemScriptBytes = stack.head.bytes

        val c = CompactSizeUInt.calculateCompactSizeUInt(redeemScriptBytes)

        val redeemScript = ScriptPubKey(c.bytes ++ redeemScriptBytes)

        redeemScript match {

          case p2wpkh: P2WPKHWitnessSPKV0 =>
            val wtxSigP2SH = scriptPubKeyExecutedProgram.txSignatureComponent
              .asInstanceOf[WitnessTxSigComponentP2SH]

            //for the p2sh(p2wpkh) case
            //https://github.com/bitcoin/bitcoin/blob/78dae8caccd82cfbfd76557f1fb7d7557c7b5edb/src/script/interpreter.cpp#L1437

            val pushOp = BitcoinScriptUtil.calculatePushOp(redeemScriptBytes)

            val expectedScriptBytes =
              BytesUtil.toByteVector(pushOp) ++ redeemScriptBytes

            val isExpectedScriptBytes =
              scriptSig.asmBytes == expectedScriptBytes
            if (
              segwitEnabled &&
              wtxSigP2SH.witness.stack.size == 2 &&
              isExpectedScriptBytes
            ) {
              executeSegWitScript(scriptPubKeyExecutedProgram, p2wpkh).get
            } else if (segwitEnabled) {
              scriptPubKeyExecutedProgram.failExecution(
                ScriptErrorWitnessMalleatedP2SH)
            } else {
              //segwit not enabled, treat as old spk
              runP2SH(scriptPubKeyExecutedProgram, p2wpkh)
            }

          case p2wsh: P2WSHWitnessSPKV0 =>
            val pushOp = BitcoinScriptUtil.calculatePushOp(redeemScriptBytes)

            val expectedScriptBytes =
              BytesUtil.toByteVector(pushOp) ++ redeemScriptBytes

            val isExpectedScriptBytes =
              scriptSig.asmBytes == expectedScriptBytes

            if (segwitEnabled && isExpectedScriptBytes) {
              // The scriptSig must be _exactly_ a single push of the redeemScript. Otherwise we
              // reintroduce malleability.
              //TODO: remove .get here
              executeSegWitScript(scriptPubKeyExecutedProgram, p2wsh).get
            } else if (
              segwitEnabled && (scriptSig.asmBytes != expectedScriptBytes)
            ) {
              scriptPubKeyExecutedProgram.failExecution(
                ScriptErrorWitnessMalleatedP2SH)
            } else {
              //treat the segwit scriptpubkey as any other redeem script
              runP2SH(scriptPubKeyExecutedProgram, p2wsh)
            }
          case _: TaprootScriptPubKey =>
            val hasUpgradeableFlag =
              flags.exists(_ == ScriptVerifyDiscourageUpgradableWitnessProgram)
            if (hasUpgradeableFlag) {
              scriptPubKeyExecutedProgram.failExecution(
                ScriptErrorDiscourageUpgradeableWitnessProgram)
            } else {
              //trivially passes
              scriptPubKeyExecutedProgram
            }
          case s @ (_: P2SHScriptPubKey | _: P2PKHScriptPubKey |
              _: P2PKWithTimeoutScriptPubKey | _: P2PKScriptPubKey |
              _: MultiSignatureScriptPubKey | _: CLTVScriptPubKey |
              _: CSVScriptPubKey | _: ConditionalScriptPubKey |
              _: NonStandardScriptPubKey | _: WitnessCommitment |
              _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
            runP2SH(scriptPubKeyExecutedProgram, s)
        }
      } else {
        scriptPubKeyExecutedProgram
      }
    }
  }

  /** Runs a segwit script through our interpreter, mimics this functionality in bitcoin core:
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1441-L1452]]
    * @param scriptPubKeyExecutedProgram the program with the
    *                                    [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] executed
    */
  private def executeSegWitScript(
      scriptPubKeyExecutedProgram: ExecutedScriptProgram,
      witnessScriptPubKey: WitnessScriptPubKey): Try[ExecutedScriptProgram] = {
    scriptPubKeyExecutedProgram.txSignatureComponent match {
      case w: WitnessTxSigComponent =>
        val scriptSig =
          scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
        val witness = w.witness
        //scriptsig must be empty if we have raw p2wsh
        //if script pubkey is a P2SHScriptPubKey then we have P2SH(P2WSH)
        (scriptSig, w.scriptPubKey) match {
          case (EmptyScriptSignature, _) | (_, _: P2SHScriptPubKey) =>
            verifyWitnessProgram(
              scriptWitness = witness,
              witnessSPK = witnessScriptPubKey,
              wTxSigComponent = w,
              scriptPubKeyExecutedProgram = scriptPubKeyExecutedProgram
            )
          case (_, _) =>
            Success(
              scriptPubKeyExecutedProgram.failExecution(
                ScriptErrorWitnessMalleated)
            )
        }
      case b: BaseTxSigComponent =>
        val scriptSig =
          scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
        (scriptSig, b.scriptPubKey) match {
          case (EmptyScriptSignature, _) | (_, _: P2SHScriptPubKey) =>
            witnessScriptPubKey.witnessVersion match {
              case WitnessVersion0 =>
                val f = scriptPubKeyExecutedProgram
                  .failExecution(ScriptErrorWitnessProgramWitnessEmpty)
                Success(f)
              case WitnessVersion1 =>
                if (ScriptFlagUtil.taprootEnabled(b.flags)) {
                  val f = scriptPubKeyExecutedProgram
                    .failExecution(ScriptErrorWitnessProgramWitnessEmpty)
                  Success(f)
                } else {
                  evaluateUnassignedWitness(b)
                }
              case UnassignedWitness(_) =>
                evaluateUnassignedWitness(b)
            }
          case (_, _) =>
            Success(
              scriptPubKeyExecutedProgram.failExecution(
                ScriptErrorWitnessMalleated))
        }
      case _: WitnessTxSigComponentRebuilt =>
        Failure(new IllegalArgumentException(
          "Cannot have a rebuild witness tx sig component here, the witness tx sigcomponent is rebuilt in verifyWitnessProgram"))
    }
  }

  /** Helper function to run the post segwit execution checks */
  private def postSegWitProgramChecks(
      evaluated: ExecutedScriptProgram): ExecutedScriptProgram = {
    if (evaluated.error.isDefined) evaluated
    else if (evaluated.stack.size != 1) {
      // Scripts inside witness implicitly require cleanstack behaviour
      //https://github.com/bitcoin/bitcoin/blob/561a7d30478b82f5d46dcf0f16e864a9608004f4/src/script/interpreter.cpp#L1464
      evaluated.failExecution(ScriptErrorCleanStack)
    } else if (evaluated.stackTopIsFalse)
      evaluated.failExecution(ScriptErrorEvalFalse)
    else evaluated
  }

  /** Rebuilds a [[WitnessVersion1]] witness program for execution in the script interpreter */
  private def rebuildV1(
      witness: TaprootWitness,
      witnessSPK: WitnessScriptPubKey): Either[
    ScriptError,
    (Seq[ScriptToken], ScriptPubKey)] = {
    require(witnessSPK.isInstanceOf[TaprootScriptPubKey],
            s"WitnessScriptPubKey must be a taproot spk, got=${witnessSPK}")
    val taprootSPK = witnessSPK.asInstanceOf[TaprootScriptPubKey]
    val program = witnessSPK.witnessProgram
    val programBytes = BytesUtil.toByteVector(program)
    programBytes.size match {
      case 32 =>
        //valid p2tr
        if (witness.stack.isEmpty) {
          Left(ScriptErrorWitnessProgramWitnessEmpty)
        } else {
          val rebuiltE = WitnessVersion1.rebuild(witness, taprootSPK)

          rebuiltE match {
            case Right(rebuilt) =>
              val constants = witness.stack.map(ScriptConstant(_))
              witness match {
                case _: TaprootKeyPath | _: TaprootScriptPath |
                    _: TaprootUnknownPath =>
                  Right((constants, rebuilt))
              }
            case Left(err) => Left(err)
          }
        }
      case _ =>
        //witness version 1 progarms need to be 32 bytes in size
        Left(ScriptErrorWitnessProgramWrongLength)
    }
  }

  /** Verifies a segregated witness program by running it through the interpreter
    * [[https://github.com/bitcoin/bitcoin/blob/f8528134fc188abc5c7175a19680206964a8fade/src/script/interpreter.cpp#L1302]]
    */
  private def verifyWitnessProgram(
      scriptWitness: ScriptWitness,
      witnessSPK: WitnessScriptPubKey,
      wTxSigComponent: WitnessTxSigComponent,
      scriptPubKeyExecutedProgram: ExecutedScriptProgram): Try[
    ExecutedScriptProgram] = {

    /** Helper function to run the post segwit execution checks */
    def postSegWitProgramChecks(
        evaluated: ExecutedScriptProgram): ExecutedScriptProgram = {
      if (evaluated.error.isDefined) evaluated
      else if (evaluated.stack.size != 1) {
        // Scripts inside witness implicitly require cleanstack behaviour
        //https://github.com/bitcoin/bitcoin/blob/561a7d30478b82f5d46dcf0f16e864a9608004f4/src/script/interpreter.cpp#L1464
        evaluated.failExecution(ScriptErrorCleanStack)
      } else if (evaluated.stackTopIsFalse)
        evaluated.failExecution(ScriptErrorEvalFalse)
      else evaluated
    }

    def rebuildV0(
        witness: ScriptWitness,
        witnessSPKV0: WitnessScriptPubKey): Either[
      ScriptError,
      (Seq[ScriptToken], ScriptPubKey)] = {
      val program = witnessSPKV0.witnessProgram
      val programBytes = BytesUtil.toByteVector(program)
      programBytes.size match {
        case 20 =>
          //p2wpkh
          if (witness.stack.size != 2) {
            Left(ScriptErrorWitnessProgramMisMatch)
          } else {
            for {
              rebuilt <- WitnessVersion0.rebuild(witness, witnessSPKV0)
              r <- Right((witness.stack.map(ScriptConstant(_)), rebuilt))
            } yield r
          }
        case 32 =>
          //p2wsh
          if (scriptWitness.stack.isEmpty)
            Left(ScriptErrorWitnessProgramWitnessEmpty)
          else {
            WitnessVersion0.rebuild(witness, witnessSPKV0) match {
              case Right(rebuilt) =>
                Right((witness.stack.tail.map(ScriptConstant(_)), rebuilt))
              case Left(err) => Left(err)
            }
          }
        case _ =>
          //witness version 0 programs need to be 20 bytes or 32 bytes in size
          Left(ScriptErrorWitnessProgramWrongLength)
      }
    }

    witnessSPK match {
      case _: WitnessScriptPubKeyV0 =>
        val either: Either[ScriptError, (Seq[ScriptToken], ScriptPubKey)] =
          rebuildV0(scriptWitness, witnessSPK)
        either match {
          case Right((stack, scriptPubKey)) =>
            if (stack.exists(_.bytes.length > MAX_PUSH_SIZE)) {
              val fail =
                scriptPubKeyExecutedProgram.failExecution(ScriptErrorPushSize)
              Success(fail)
            } else {
              val newWTxSigComponent =
                rebuildWTxSigComponent(wTxSigComponent, scriptPubKey)
              val newProgram = newWTxSigComponent.map { comp =>
                PreExecutionScriptProgram(txSignatureComponent = comp,
                                          stack = stack.toList,
                                          script = scriptPubKey.asm.toList,
                                          originalScript =
                                            scriptPubKey.asm.toList,
                                          altStack = Nil,
                                          flags = comp.flags)
              }
              val evaluated = newProgram.map(executeProgram)
              evaluated.map(e => postSegWitProgramChecks(e))
            }

          case Left(err) =>
            val program = ExecutedScriptProgram(
              txSignatureComponent = wTxSigComponent,
              stack = Nil,
              script = Nil,
              originalScript = Nil,
              altStack = Nil,
              flags = wTxSigComponent.flags,
              lastCodeSeparator = None,
              codeSeparatorTapscriptIdx = None,
              error = Some(err)
            )
            Success(program)
        }
      case trSPK: TaprootScriptPubKey =>
        require(
          scriptWitness.isInstanceOf[TaprootWitness],
          s"witness must be taproot witness for witness version 1 script execution, got=$scriptWitness")
        val taprootWitness = scriptWitness.asInstanceOf[TaprootWitness]
        val either: Either[ScriptError, (Seq[ScriptToken], ScriptPubKey)] =
          rebuildV1(taprootWitness, witnessSPK)
        either match {
          case Right((stack, scriptPubKey)) =>
            executeTapscript(
              taprootWitness = taprootWitness,
              taprootSPK = trSPK,
              rebuiltSPK = scriptPubKey,
              stack = stack.toVector,
              wTxSigComponent = wTxSigComponent,
              scriptPubKeyExecutedProgram = scriptPubKeyExecutedProgram
            )
          case Left(err) =>
            val program = ExecutedScriptProgram(
              txSignatureComponent = wTxSigComponent,
              stack = Nil,
              script = Nil,
              originalScript = Nil,
              altStack = Nil,
              flags = wTxSigComponent.flags,
              lastCodeSeparator = None,
              codeSeparatorTapscriptIdx = None,
              error = Some(err)
            )
            Success(program)
        }
      case u: UnassignedWitnessScriptPubKey =>
        if (u.asm.contains(OP_0)) {
          //cannot have an v0 unassigned witness as according to BIP141
          //a witness v0 script must be 20 bytes or 32 bytes
          val program = ExecutedScriptProgram(
            txSignatureComponent = wTxSigComponent,
            stack = Nil,
            script = Nil,
            originalScript = Nil,
            altStack = Nil,
            flags = wTxSigComponent.flags,
            lastCodeSeparator = None,
            codeSeparatorTapscriptIdx = None,
            error = Some(ScriptErrorWitnessProgramWrongLength)
          )
          Success(program)
        } else {
          evaluateUnassignedWitness(wTxSigComponent)
        }

    }
  }

  private val opSuccessBytes: Vector[Byte] = {
    Vector(80.toByte, 98.toByte) ++
      126.to(129).map(_.toByte).toVector ++
      131.to(134).map(_.toByte).toVector ++
      Vector(137.toByte, 138.toByte, 141.toByte, 142.toByte) ++
      149.to(153).map(_.toByte) ++
      187.to(254).map(_.toByte)
  }

  /** Checks if there is an opcode defined as OP_SUCCESSx in BIP342
    * @see https://github.com/bitcoin/bips/blob/master/bip-0342.mediawiki#specification
    */
  private def containsOpSuccess(asm: Vector[ScriptToken]): Boolean = {
    asm.exists {
      case op: ScriptOperation =>
        opSuccessBytes.contains(op.toByte)
      case _: ScriptToken => false
    }
  }

  private def executeTapscript(
      taprootWitness: TaprootWitness,
      taprootSPK: TaprootScriptPubKey,
      rebuiltSPK: ScriptPubKey,
      stack: Vector[ScriptToken],
      wTxSigComponent: WitnessTxSigComponent,
      scriptPubKeyExecutedProgram: ExecutedScriptProgram): Try[
    ExecutedScriptProgram] = {
    val sigVersion = scriptPubKeyExecutedProgram.txSignatureComponent.sigVersion
    if (ScriptFlagUtil.taprootEnabled(scriptPubKeyExecutedProgram.flags)) {
      val containsOPSuccess = containsOpSuccess(rebuiltSPK.asm.toVector)

      if (sigVersion == SigVersionTapscript && containsOPSuccess) {
        handleOpSuccess(scriptPubKeyExecutedProgram)
      } else {
        taprootWitness match {
          case keypath: TaprootKeyPath =>
            val program = checkSchnorrSignature(
              keypath,
              taprootSPK.pubKey.schnorrPublicKey,
              program = scriptPubKeyExecutedProgram)
            Success(program)
          case _: TaprootUnknownPath =>
            //is this right? I believe to maintain softfork compatibility we
            //just succeed?
            Success(scriptPubKeyExecutedProgram)
          case taprootScriptPath: TaprootScriptPath =>
            require(
              wTxSigComponent.isInstanceOf[TaprootTxSigComponent],
              s"Must have taproot tx sig component to execute tapscript, got=${wTxSigComponent.getClass.getSimpleName}"
            )

            val taprootTxSigComponent =
              wTxSigComponent.asInstanceOf[TaprootTxSigComponent]
            val controlBlock = taprootScriptPath.controlBlock
            val script = taprootScriptPath.script
            //execdata.m_tapleaf_hash = ComputeTapleafHash(control[0] & TAPROOT_LEAF_MASK, exec_script);
            val tapLeafHash = TaprootScriptPath.computeTapleafHash(
              controlBlock.leafVersion,
              script)
            val isValidTaprootCommitment =
              TaprootScriptPath.verifyTaprootCommitment(
                controlBlock = controlBlock,
                program = taprootSPK,
                tapLeafHash = tapLeafHash)
            if (!isValidTaprootCommitment) {
              val p = scriptPubKeyExecutedProgram.failExecution(
                ScriptErrorWitnessProgramMisMatch)
              Success(p)
            } else {
              val isDiscouragedTaprootVersion =
                scriptPubKeyExecutedProgram.flags.exists(
                  _ == ScriptVerifyDiscourageUpgradableTaprootVersion)
              if (controlBlock.isTapLeafMask) {
                //drop the control block & script in the witness
                val stackNoControlBlockOrScript = {
                  if (scriptPubKeyExecutedProgram.getAnnexHashOpt.isDefined) {
                    //if we have an annex we need to drop
                    //annex,control block, script
                    stack.tail.tail.tail
                  } else {
                    //else just drop control block, script
                    stack.tail.tail
                  }
                }
                if (
                  stackNoControlBlockOrScript.exists(
                    _.bytes.length > MAX_PUSH_SIZE)
                ) {
                  val fail =
                    scriptPubKeyExecutedProgram.failExecution(
                      ScriptErrorPushSize)
                  Success(fail)
                } else {
                  val newProgram = PreExecutionScriptProgram(
                    txSignatureComponent = taprootTxSigComponent,
                    stack = stackNoControlBlockOrScript.toList,
                    script = rebuiltSPK.asm.toList,
                    originalScript = rebuiltSPK.asm.toList,
                    altStack = Nil,
                    flags = taprootTxSigComponent.flags
                  )
                  val evaluated = executeProgram(newProgram)
                  val segwitChecks = postSegWitProgramChecks(evaluated)
                  Success(segwitChecks)
                }
              } else if (isDiscouragedTaprootVersion) {
                val p = scriptPubKeyExecutedProgram.failExecution(
                  ScriptErrorDiscourageUpgradableTaprootVersion)
                Success(p)
              } else {
                //is this right? I believe to maintain softfork compatibility we
                //just succeed?
                Success(scriptPubKeyExecutedProgram)
              }
            }
        }
      }
    } else {
      //if taproot flag not set trivially pass
      Success(scriptPubKeyExecutedProgram)
    }
  }

  private def handleOpSuccess(
      scriptPubKeyExecutedProgram: ExecutedScriptProgram): Try[
    ExecutedScriptProgram] = {
    val discourageOpSuccess =
      ScriptFlagUtil.discourageOpSuccess(scriptPubKeyExecutedProgram.flags)
    val p = if (discourageOpSuccess) {
      scriptPubKeyExecutedProgram.failExecution(ScriptErrorDiscourageOpSuccess)
    } else {
      scriptPubKeyExecutedProgram
    }
    Success(p)
  }

  /** Similar to the check schnorr signature method in bitcoin core
    * @see https://github.com/bitcoin/bitcoin/blob/b71d37da2c8c8d2a9cef020731767a6929db54b4/src/script/interpreter.cpp#L1673
    */
  private def checkSchnorrSignature(
      witness: TaprootKeyPath,
      pubKey: SchnorrPublicKey,
      program: ExecutedScriptProgram): ExecutedScriptProgram = {
    val scriptResult = TransactionSignatureChecker.checkSchnorrSignature(
      program.txSignatureComponent,
      pubKey,
      witness,
      program.taprootSerializationOptions)
    scriptResult match {
      case ScriptOk =>
        // Set stack to OP_TRUE so we don't fail
        // from empty stack
        program.copy(stack = List(OP_TRUE))
      case err: ScriptError => program.failExecution(err)
    }
  }

  /** Executes a PreExecutionScriptProgram */
  private def executeProgram(
      program: PreExecutionScriptProgram): ExecutedScriptProgram = {
    val scriptByteVector = BytesUtil.toByteVector(program.script)
    val sigVersion = program.txSignatureComponent.sigVersion
    val isTaprootSigVersion =
      sigVersion == SigVersionTapscript || sigVersion == SigVersionTaprootKeySpend

    if (scriptByteVector.length > 10000 && !isTaprootSigVersion) {
      program.failExecution(ScriptErrorScriptSize)
    } else {
      loop(program.toExecutionInProgress, 0)
    }
  }

  /** Finalizes an ExecutesScriptProgram by counting Script Ops
    * and giving the ScriptProgram an error if there were too many.
    */
  @tailrec
  private def completeProgramExecution(
      program: ExecutedScriptProgram): ExecutedScriptProgram = {
    val countedOps = program.originalScript
      .count(BitcoinScriptUtil.countsTowardsScriptOpLimit)
    val sigVersion = program.txSignatureComponent.sigVersion
    val isBaseOrSegwitV0 =
      sigVersion == SigVersionBase || sigVersion == SigVersionWitnessV0
    if (
      isBaseOrSegwitV0 && countedOps > MAX_SCRIPT_OPS && program.error.isEmpty
    ) {
      completeProgramExecution(program.failExecution(ScriptErrorOpCount))
    } else {
      program
    }
  }

  private val disabledOpCodes = {
    val arithmetic =
      Seq(OP_MUL, OP_2MUL, OP_DIV, OP_2DIV, OP_MOD, OP_LSHIFT, OP_RSHIFT)

    val bitwise = Seq(OP_INVERT, OP_AND, OP_OR, OP_XOR)

    val splice = Seq(OP_CAT, OP_SUBSTR, OP_LEFT, OP_RIGHT)

    arithmetic ++ bitwise ++ splice
  }

  private val badOpCodes = Vector(OP_VERIF, OP_VERNOTIF)

  /** The execution loop for a script
    *
    * @param program the program whose script needs to be evaluated
    * @return program the final state of the program after being evaluated by the interpreter
    */
  @tailrec
  private def loop(
      program: ExecutionInProgressScriptProgram,
      opCount: Int): ExecutedScriptProgram = {
    val scriptByteVector = BytesUtil.toByteVector(program.script)
    val sigVersion = program.txSignatureComponent.sigVersion
    val isTaprootSigVersion =
      sigVersion == SigVersionTapscript || sigVersion == SigVersionTaprootKeySpend
    if (opCount > MAX_SCRIPT_OPS && !isTaprootSigVersion) {
      completeProgramExecution(program.failExecution(ScriptErrorOpCount))
    } else if (scriptByteVector.length > 10000 && !isTaprootSigVersion) {
      completeProgramExecution(program.failExecution(ScriptErrorScriptSize))
    } else {
      val (nextProgram, nextOpCount) = program.script match {
        //if at any time we see that the program is not valid
        //cease script execution
        case _ if program.script.intersect(badOpCodes).nonEmpty =>
          (program.failExecution(ScriptErrorBadOpCode), opCount)
        //disabled operations
        case _
            if program.script
              .intersect(disabledOpCodes)
              .nonEmpty =>
          (program.failExecution(ScriptErrorDisabledOpCode), opCount)
        //program cannot contain a push operation > 520 bytes
        case _
            if program.script.exists(token =>
              token.bytes.size > MAX_PUSH_SIZE) =>
          (program.failExecution(ScriptErrorPushSize), opCount)
        //program stack size cannot be greater than 1000 elements
        case _ if (program.stack.size + program.altStack.size) > 1000 =>
          (program.failExecution(ScriptErrorStackSize), opCount)

        //no more script operations to run, return whether the program is valid and the final state of the program
        case Nil =>
          (program.toExecutedProgram, opCount)

        case _ if !program.shouldExecuteNextOperation =>
          (program.updateScript(program.script.tail), opCount)

        //stack operations
        case OP_DUP :: _ =>
          val programOrError = StackInterpreter.opDup(program)
          val newOpCount =
            calcOpCount(opCount, OP_DUP)
          (programOrError, newOpCount)

        case OP_DEPTH :: _ =>
          val programOrError = StackInterpreter.opDepth(program)
          val newOpCount =
            calcOpCount(opCount, OP_DEPTH)
          (programOrError, newOpCount)

        case OP_TOALTSTACK :: _ =>
          val programOrError = StackInterpreter.opToAltStack(program)
          val newOpCount =
            calcOpCount(opCount, OP_TOALTSTACK)
          (programOrError, newOpCount)

        case OP_FROMALTSTACK :: _ =>
          val programOrError = StackInterpreter.opFromAltStack(program)
          val newOpCount =
            calcOpCount(opCount, OP_FROMALTSTACK)
          (programOrError, newOpCount)

        case OP_DROP :: _ =>
          val programOrError = StackInterpreter.opDrop(program)
          val newOpCount =
            calcOpCount(opCount, OP_DROP)
          (programOrError, newOpCount)

        case OP_IFDUP :: _ =>
          val programOrError = StackInterpreter.opIfDup(program)
          val newOpCount =
            calcOpCount(opCount, OP_IFDUP)
          (programOrError, newOpCount)

        case OP_NIP :: _ =>
          val programOrError = StackInterpreter.opNip(program)
          val newOpCount =
            calcOpCount(opCount, OP_NIP)
          (programOrError, newOpCount)

        case OP_OVER :: _ =>
          val programOrError = StackInterpreter.opOver(program)
          val newOpCount =
            calcOpCount(opCount, OP_OVER)
          (programOrError, newOpCount)

        case OP_PICK :: _ =>
          val programOrError = StackInterpreter.opPick(program)
          val newOpCount =
            calcOpCount(opCount, OP_PICK)
          (programOrError, newOpCount)

        case OP_ROLL :: _ =>
          val programOrError = StackInterpreter.opRoll(program)
          val newOpCount =
            calcOpCount(opCount, OP_ROLL)
          (programOrError, newOpCount)

        case OP_ROT :: _ =>
          val programOrError = StackInterpreter.opRot(program)
          val newOpCount =
            calcOpCount(opCount, OP_ROT)
          (programOrError, newOpCount)

        case OP_2ROT :: _ =>
          val programOrError = StackInterpreter.op2Rot(program)
          val newOpCount =
            calcOpCount(opCount, OP_2ROT)
          (programOrError, newOpCount)

        case OP_2DROP :: _ =>
          val programOrError = StackInterpreter.op2Drop(program)
          val newOpCount =
            calcOpCount(opCount, OP_2DROP)
          (programOrError, newOpCount)

        case OP_SWAP :: _ =>
          val programOrError = StackInterpreter.opSwap(program)
          val newOpCount =
            calcOpCount(opCount, OP_SWAP)
          (programOrError, newOpCount)

        case OP_TUCK :: _ =>
          val programOrError = StackInterpreter.opTuck(program)
          val newOpCount =
            calcOpCount(opCount, OP_TUCK)
          (programOrError, newOpCount)

        case OP_2DUP :: _ =>
          val programOrError = StackInterpreter.op2Dup(program)
          val newOpCount =
            calcOpCount(opCount, OP_2DUP)
          (programOrError, newOpCount)

        case OP_3DUP :: _ =>
          val programOrError = StackInterpreter.op3Dup(program)
          val newOpCount =
            calcOpCount(opCount, OP_3DUP)
          (programOrError, newOpCount)

        case OP_2OVER :: _ =>
          val programOrError = StackInterpreter.op2Over(program)
          val newOpCount =
            calcOpCount(opCount, OP_2OVER)
          (programOrError, newOpCount)

        case OP_2SWAP :: _ =>
          val programOrError = StackInterpreter.op2Swap(program)
          val newOpCount =
            calcOpCount(opCount, OP_2SWAP)
          (programOrError, newOpCount)

        //arithmetic operations
        case OP_ADD :: _ =>
          val programOrError = ArithmeticInterpreter.opAdd(program)
          val newOpCount =
            calcOpCount(opCount, OP_ADD)
          (programOrError, newOpCount)

        case OP_1ADD :: _ =>
          val programOrError = ArithmeticInterpreter.op1Add(program)
          val newOpCount =
            calcOpCount(opCount, OP_1ADD)
          (programOrError, newOpCount)

        case OP_1SUB :: _ =>
          val programOrError = ArithmeticInterpreter.op1Sub(program)
          val newOpCount =
            calcOpCount(opCount, OP_1SUB)
          (programOrError, newOpCount)

        case OP_SUB :: _ =>
          val programOrError = ArithmeticInterpreter.opSub(program)
          val newOpCount =
            calcOpCount(opCount, OP_SUB)
          (programOrError, newOpCount)

        case OP_ABS :: _ =>
          val programOrError = ArithmeticInterpreter.opAbs(program)
          val newOpCount =
            calcOpCount(opCount, OP_ABS)
          (programOrError, newOpCount)

        case OP_NEGATE :: _ =>
          val programOrError = ArithmeticInterpreter.opNegate(program)
          val newOpCount =
            calcOpCount(opCount, OP_NEGATE)
          (programOrError, newOpCount)

        case OP_NOT :: _ =>
          val programOrError = ArithmeticInterpreter.opNot(program)
          val newOpCount =
            calcOpCount(opCount, OP_NOT)
          (programOrError, newOpCount)

        case OP_0NOTEQUAL :: _ =>
          val programOrError = ArithmeticInterpreter.op0NotEqual(program)
          val newOpCount =
            calcOpCount(opCount, OP_0NOTEQUAL)
          (programOrError, newOpCount)

        case OP_BOOLAND :: _ =>
          val programOrError = ArithmeticInterpreter.opBoolAnd(program)
          val newOpCount =
            calcOpCount(opCount, OP_BOOLAND)
          (programOrError, newOpCount)

        case OP_BOOLOR :: _ =>
          val programOrError = ArithmeticInterpreter.opBoolOr(program)
          val newOpCount =
            calcOpCount(opCount, OP_BOOLOR)
          (programOrError, newOpCount)

        case OP_NUMEQUAL :: _ =>
          val programOrError = ArithmeticInterpreter.opNumEqual(program)
          val newOpCount =
            calcOpCount(opCount, OP_NUMEQUAL)
          (programOrError, newOpCount)

        case OP_NUMEQUALVERIFY :: _ =>
          val programOrError = ArithmeticInterpreter.opNumEqualVerify(program)
          val newOpCount =
            calcOpCount(opCount, OP_NUMEQUALVERIFY)
          (programOrError, newOpCount)

        case OP_NUMNOTEQUAL :: _ =>
          val programOrError = ArithmeticInterpreter.opNumNotEqual(program)
          val newOpCount =
            calcOpCount(opCount, OP_NUMNOTEQUAL)
          (programOrError, newOpCount)

        case OP_LESSTHAN :: _ =>
          val programOrError = ArithmeticInterpreter.opLessThan(program)
          val newOpCount =
            calcOpCount(opCount, OP_LESSTHAN)
          (programOrError, newOpCount)

        case OP_GREATERTHAN :: _ =>
          val programOrError = ArithmeticInterpreter.opGreaterThan(program)
          val newOpCount =
            calcOpCount(opCount, OP_GREATERTHAN)
          (programOrError, newOpCount)

        case OP_LESSTHANOREQUAL :: _ =>
          val programOrError = ArithmeticInterpreter.opLessThanOrEqual(program)
          val newOpCount =
            calcOpCount(opCount, OP_LESSTHANOREQUAL)
          (programOrError, newOpCount)

        case OP_GREATERTHANOREQUAL :: _ =>
          val programOrError =
            ArithmeticInterpreter.opGreaterThanOrEqual(program)
          val newOpCount =
            calcOpCount(opCount, OP_GREATERTHANOREQUAL)
          (programOrError, newOpCount)

        case OP_MIN :: _ =>
          val programOrError = ArithmeticInterpreter.opMin(program)
          val newOpCount =
            calcOpCount(opCount, OP_MIN)
          (programOrError, newOpCount)

        case OP_MAX :: _ =>
          val programOrError = ArithmeticInterpreter.opMax(program)
          val newOpCount =
            calcOpCount(opCount, OP_MAX)
          (programOrError, newOpCount)

        case OP_WITHIN :: _ =>
          val programOrError = ArithmeticInterpreter.opWithin(program)
          val newOpCount =
            calcOpCount(opCount, OP_WITHIN)
          (programOrError, newOpCount)

        //bitwise operations
        case OP_EQUAL :: _ =>
          val programOrError = BitwiseInterpreter.opEqual(program)
          val newOpCount =
            calcOpCount(opCount, OP_EQUAL)
          (programOrError, newOpCount)

        case OP_EQUALVERIFY :: _ =>
          val programOrError = BitwiseInterpreter.opEqualVerify(program)
          val newOpCount =
            calcOpCount(opCount, OP_EQUALVERIFY)
          (programOrError, newOpCount)

        case OP_0 :: t =>
          val programOrError =
            program.updateStackAndScript(ScriptNumber.zero :: program.stack, t)
          val newOpCount =
            calcOpCount(opCount, OP_0)
          (programOrError, newOpCount)

        case (scriptNumberOp: ScriptNumberOperation) :: t =>
          val programOrError =
            program.updateStackAndScript(
              ScriptNumber(scriptNumberOp.toLong) :: program.stack,
              t)
          val newOpCount =
            calcOpCount(opCount, scriptNumberOp)
          (programOrError, newOpCount)

        case (bytesToPushOntoStack: BytesToPushOntoStack) :: _ =>
          val programOrError =
            ConstantInterpreter.pushScriptNumberBytesToStack(program)
          val newOpCount =
            calcOpCount(opCount, bytesToPushOntoStack)
          (programOrError, newOpCount)

        case (scriptNumber: ScriptNumber) :: t =>
          val programOrError =
            program.updateStackAndScript(scriptNumber :: program.stack, t)
          val newOpCount =
            calcOpCount(opCount, scriptNumber)
          (programOrError, newOpCount)

        case OP_PUSHDATA1 :: _ =>
          val programOrError = ConstantInterpreter.opPushData1(program)
          val newOpCount =
            calcOpCount(opCount, OP_PUSHDATA1)
          (programOrError, newOpCount)

        case OP_PUSHDATA2 :: _ =>
          val programOrError = ConstantInterpreter.opPushData2(program)
          val newOpCount =
            calcOpCount(opCount, OP_PUSHDATA2)
          (programOrError, newOpCount)

        case OP_PUSHDATA4 :: _ =>
          val programOrError = ConstantInterpreter.opPushData4(program)
          val newOpCount =
            calcOpCount(opCount, OP_PUSHDATA4)
          (programOrError, newOpCount)

        case (x: ScriptConstant) :: t =>
          val programOrError =
            program.updateStackAndScript(x :: program.stack, t)
          val newOpCount =
            calcOpCount(opCount, x)
          (programOrError, newOpCount)

        //control operations
        case OP_IF :: _ =>
          val programOrError = ControlOperationsInterpreter.opIf(program)
          val newOpCount =
            calcOpCount(opCount, OP_IF)
          (programOrError, newOpCount)

        case OP_NOTIF :: _ =>
          val programOrError = ControlOperationsInterpreter.opNotIf(program)
          val newOpCount =
            calcOpCount(opCount, OP_NOTIF)
          (programOrError, newOpCount)

        case OP_ELSE :: _ =>
          val programOrError = ControlOperationsInterpreter.opElse(program)
          val newOpCount =
            calcOpCount(opCount, OP_ELSE)
          (programOrError, newOpCount)

        case OP_ENDIF :: _ =>
          val programOrError = ControlOperationsInterpreter.opEndIf(program)
          val newOpCount =
            calcOpCount(opCount, OP_ENDIF)
          (programOrError, newOpCount)

        case OP_RETURN :: _ =>
          val programOrError = ControlOperationsInterpreter.opReturn(program)
          val newOpCount =
            calcOpCount(opCount, OP_RETURN)
          (programOrError, newOpCount)

        case OP_VERIFY :: _ =>
          val programOrError = ControlOperationsInterpreter.opVerify(program)
          val newOpCount =
            calcOpCount(opCount, OP_VERIFY)
          (programOrError, newOpCount)

        //crypto operations
        case OP_HASH160 :: _ =>
          val programOrError = CryptoInterpreter.opHash160(program)
          val newOpCount =
            calcOpCount(opCount, OP_HASH160)
          (programOrError, newOpCount)

        case OP_CHECKSIG :: _ =>
          val programOrError = CryptoInterpreter.opCheckSig(program)
          val newOpCount =
            calcOpCount(opCount, OP_CHECKSIG)
          (programOrError, newOpCount)

        case OP_CHECKSIGVERIFY :: _ =>
          val programOrError = CryptoInterpreter.opCheckSigVerify(program)
          val newOpCount =
            calcOpCount(opCount, OP_CHECKSIGVERIFY)
          (programOrError, newOpCount)
        case OP_CHECKSIGADD :: _ =>
          val programOrError = CryptoInterpreter.opCheckSigAdd(program)
          val newOpCount =
            calcOpCount(opCount, OP_CHECKSIGVERIFY)
          (programOrError, newOpCount)

        case OP_SHA1 :: _ =>
          val programOrError = CryptoInterpreter.opSha1(program)
          val newOpCount =
            calcOpCount(opCount, OP_SHA1)
          (programOrError, newOpCount)

        case OP_RIPEMD160 :: _ =>
          val programOrError = CryptoInterpreter.opRipeMd160(program)
          val newOpCount =
            calcOpCount(opCount, OP_RIPEMD160)
          (programOrError, newOpCount)

        case OP_SHA256 :: _ =>
          val programOrError = CryptoInterpreter.opSha256(program)
          val newOpCount =
            calcOpCount(opCount, OP_SHA256)
          (programOrError, newOpCount)

        case OP_HASH256 :: _ =>
          val programOrError = CryptoInterpreter.opHash256(program)
          val newOpCount =
            calcOpCount(opCount, OP_HASH256)
          (programOrError, newOpCount)

        case OP_CODESEPARATOR :: _ =>
          val programOrError = CryptoInterpreter.opCodeSeparator(program)
          val newOpCount =
            calcOpCount(opCount, OP_CODESEPARATOR)
          (programOrError, newOpCount)

        case OP_CHECKMULTISIG :: _ =>
          CryptoInterpreter.opCheckMultiSig(program) match {
            case newProgram: ExecutedScriptProgram =>
              //script was marked invalid for other reasons, don't need to update the opcount
              (newProgram, opCount)
            case newProgram: ExecutionInProgressScriptProgram =>
              val programOrError = newProgram
              val newOpCount =
                calcOpCount(opCount, OP_CHECKMULTISIG) + BitcoinScriptUtil
                  .numPossibleSignaturesOnStack(program)
                  .toInt
              (programOrError, newOpCount)

          }
        case OP_CHECKMULTISIGVERIFY :: _ =>
          CryptoInterpreter.opCheckMultiSigVerify(program) match {
            case newProgram: ExecutedScriptProgram =>
              //script was marked invalid for other reasons, don't need to update the opcount
              (newProgram, opCount)
            case newProgram: ExecutionInProgressScriptProgram =>
              val programOrError = newProgram
              val newOpCount =
                calcOpCount(opCount, OP_CHECKMULTISIGVERIFY) + BitcoinScriptUtil
                  .numPossibleSignaturesOnStack(program)
                  .toInt
              (programOrError, newOpCount)

          }
        //reserved operations
        case OP_NOP :: t =>
          //script discourage upgradeable flag does not apply to a OP_NOP
          val programOrError = program.updateScript(t)
          val newOpCount =
            calcOpCount(opCount, OP_NOP)
          (programOrError, newOpCount)

        //if we see an OP_NOP and the DISCOURAGE_UPGRADABLE_OP_NOPS flag is set we must fail our program
        case (nop: NOP) :: _
            if ScriptFlagUtil.discourageUpgradableNOPs(program.flags) =>
          (program.failExecution(ScriptErrorDiscourageUpgradableNOPs),
           calcOpCount(opCount, nop))
        case (nop: NOP) :: t =>
          val programOrError = program.updateScript(t)
          val newOpCount =
            calcOpCount(opCount, nop)
          (programOrError, newOpCount)

        case OP_RESERVED :: _ =>
          (program.failExecution(ScriptErrorBadOpCode),
           calcOpCount(opCount, OP_RESERVED))
        case OP_VER :: _ =>
          (program.failExecution(ScriptErrorBadOpCode),
           calcOpCount(opCount, OP_VER))
        case OP_RESERVED1 :: _ =>
          (program.failExecution(ScriptErrorBadOpCode),
           calcOpCount(opCount, OP_RESERVED1))
        case OP_RESERVED2 :: _ =>
          (program.failExecution(ScriptErrorBadOpCode),
           calcOpCount(opCount, OP_RESERVED2))

        case (reservedOperation: ReservedOperation) :: _ =>
          (program.failExecution(ScriptErrorBadOpCode),
           calcOpCount(opCount, reservedOperation))
        //splice operations
        case OP_SIZE :: _ =>
          val programOrError = SpliceInterpreter.opSize(program)
          val newOpCount =
            calcOpCount(opCount, OP_SIZE)
          (programOrError, newOpCount)

        //locktime operations
        case OP_CHECKLOCKTIMEVERIFY :: _ =>
          //check if CLTV is enforced yet
          if (ScriptFlagUtil.checkLockTimeVerifyEnabled(program.flags)) {
            val programOrError =
              LockTimeInterpreter.opCheckLockTimeVerify(program)
            val newOpCount =
              calcOpCount(opCount, OP_CHECKLOCKTIMEVERIFY)
            (programOrError, newOpCount)

          } //if not, check to see if we should discourage p
          else if (ScriptFlagUtil.discourageUpgradableNOPs(program.flags)) {
            (program.failExecution(ScriptErrorDiscourageUpgradableNOPs),
             calcOpCount(opCount, OP_CHECKLOCKTIMEVERIFY))
          } //in this case, just reat OP_CLTV just like a NOP and remove it from the stack
          else {
            val programOrError = program.updateScript(program.script.tail)
            val newOpCount =
              calcOpCount(opCount, OP_CHECKLOCKTIMEVERIFY)
            (programOrError, newOpCount)
          }

        case OP_CHECKSEQUENCEVERIFY :: _ =>
          //check if CLTV is enforced yet
          if (ScriptFlagUtil.checkSequenceVerifyEnabled(program.flags)) {
            val programOrError =
              LockTimeInterpreter.opCheckSequenceVerify(program)
            val newOpCount =
              calcOpCount(opCount, OP_CHECKSEQUENCEVERIFY)
            (programOrError, newOpCount)

          } //if not, check to see if we should discourage p
          else if (ScriptFlagUtil.discourageUpgradableNOPs(program.flags)) {
            (program.failExecution(ScriptErrorDiscourageUpgradableNOPs),
             calcOpCount(opCount, OP_CHECKSEQUENCEVERIFY))
          } //in this case, just read OP_CSV just like a NOP and remove it from the stack
          else {
            val programOrError = program.updateScript(program.script.tail)
            val newOpCount =
              calcOpCount(opCount, OP_CHECKSEQUENCEVERIFY)
            (programOrError, newOpCount)
          }

        case h :: _ => throw new RuntimeException(s"$h was unmatched")
      }

      nextProgram match {
        case p: ExecutedScriptProgram =>
          completeProgramExecution(p)
        case p: ExecutionInProgressScriptProgram => loop(p, nextOpCount)
      }
    }
  }

  /** Checks the validity of a transaction in accordance to bitcoin core's CheckTransaction function
    * https://github.com/bitcoin/bitcoin/blob/f7a21dae5dbf71d5bc00485215e84e6f2b309d0a/src/main.cpp#L939.
    */
  def checkTransaction(transaction: Transaction): Boolean = {
    val inputOutputsNotZero =
      !(transaction.inputs.isEmpty || transaction.outputs.isEmpty)
    val txNotLargerThanBlock = transaction.bytes.size < Consensus.maxBlockSize
    val txNotHeavierThanBlock = transaction.weight < Consensus.maxBlockWeight
    val outputsSpendValidAmountsOfMoney = !transaction.outputs.exists(o =>
      o.value < CurrencyUnits.zero || o.value > Consensus.maxMoney)

    val outputValues = transaction.outputs.map(_.value)
    val totalSpentByOutputs: CurrencyUnit =
      outputValues.fold(CurrencyUnits.zero)(_ + _)
    val allOutputsValidMoneyRange = validMoneyRange(totalSpentByOutputs)
    val prevOutputs = transaction.inputs.map(_.previousOutput)
    val noDuplicateInputs = prevOutputs.distinct.size == prevOutputs.size

    val isValidScriptSigForCoinbaseTx = if (transaction.isCoinbase) {
      transaction.inputs.head.scriptSignature.asmBytes.size >= 2 &&
      transaction.inputs.head.scriptSignature.asmBytes.size <= 100
    } else {
      !transaction.inputs.exists(_.previousOutput == EmptyTransactionOutPoint)
    }
    inputOutputsNotZero && txNotLargerThanBlock && txNotHeavierThanBlock && outputsSpendValidAmountsOfMoney &&
    allOutputsValidMoneyRange && noDuplicateInputs && isValidScriptSigForCoinbaseTx
  }

  /** Determines if the given currency unit is within the valid range for the system */
  def validMoneyRange(currencyUnit: CurrencyUnit): Boolean = {
    currencyUnit >= CurrencyUnits.zero && currencyUnit <= Consensus.maxMoney
  }

  /**  Calculates the new op count after the execution of the given
    * [[org.bitcoins.core.script.constant.ScriptToken ScriptToken]]
    */
  private def calcOpCount(oldOpCount: Int, token: ScriptToken): Int =
    if (BitcoinScriptUtil.countsTowardsScriptOpLimit(token)) {
      oldOpCount + 1
    } else {
      oldOpCount
    }

  /** Checks if the transaction contained a witness that we did not use
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1515-L1523]]
    * Return true if witness was NOT used, return false if witness was used.
    */
  private def hasUnexpectedWitness(program: ScriptProgram): Boolean = {
    val txSigComponent = program.txSignatureComponent
    txSigComponent match {
      case _: WitnessTxSigComponentRaw | _: TaprootTxSigComponent => false
      case w: WitnessTxSigComponentP2SH =>
        w.scriptSignature.redeemScript match {
          case _: WitnessScriptPubKey => false
          case _                      => true
        }
      case b: BaseTxSigComponent =>
        b.transaction match {
          case wtx: WitnessTransaction =>
            wtx.witness(txSigComponent.inputIndex.toInt).stack.nonEmpty
          case _: NonWitnessTransaction => false
        }
      case r: WitnessTxSigComponentRebuilt =>
        r.transaction.witness(txSigComponent.inputIndex.toInt).stack.nonEmpty
    }
  }

  /** Helper function used to rebuild a
    * [[org.bitcoins.core.crypto.WitnessTxSigComponentRebuilt WitnessTxSigComponentRebuilt]]
    * this converts a [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    * into it's corresponding [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    */
  private def rebuildWTxSigComponent(
      old: WitnessTxSigComponent,
      rebuildScriptPubKey: ScriptPubKey): Try[WitnessTxSigComponentRebuilt] = {
    val updatedOutput = TransactionOutput(old.output.value, rebuildScriptPubKey)
    old match {
      case wTxSigComponentRaw: WitnessTxSigComponentRaw =>
        Success(
          WitnessTxSigComponentRebuilt(old.transaction,
                                       old.inputIndex,
                                       updatedOutput,
                                       wTxSigComponentRaw.scriptPubKey,
                                       old.flags))
      case wTxSigComponentP2SH: WitnessTxSigComponentP2SH =>
        wTxSigComponentP2SH.witnessScriptPubKey.map {
          (wit: WitnessScriptPubKey) =>
            val updatedOutput =
              TransactionOutput(old.output.value, rebuildScriptPubKey)
            WitnessTxSigComponentRebuilt(old.transaction,
                                         old.inputIndex,
                                         updatedOutput,
                                         wit,
                                         old.flags)
        }

      case _: TaprootTxSigComponent =>
        sys.error(s"Taproot tx sig component does not need to be rebuilt")
    }
  }

  /** Logic to evaluate a witnesss version that has not been assigned yet */
  private def evaluateUnassignedWitness(
      txSigComponent: TxSigComponent): Try[ExecutedScriptProgram] = {
    val flags = txSigComponent.flags
    val discourageUpgradableWitnessVersion =
      ScriptFlagUtil.discourageUpgradableWitnessProgram(flags)
    if (discourageUpgradableWitnessVersion) {
      val executed = ExecutedScriptProgram(
        txSignatureComponent = txSigComponent,
        stack = Nil,
        script = Nil,
        originalScript = txSigComponent.scriptPubKey.asm.toList,
        altStack = Nil,
        flags = flags,
        lastCodeSeparator = None,
        codeSeparatorTapscriptIdx = None,
        error = Some(ScriptErrorDiscourageUpgradeableWitnessProgram)
      )
      Success(executed)
    } else {
      //if we are not discouraging upgradable ops, we just trivially return the program with an OP_TRUE on the stack
      //see: https://github.com/bitcoin/bitcoin/blob/b83264d9c7a8ddb79f64bd9540caddc8632ef31f/src/script/interpreter.cpp#L1386-L1389
      val inProgress = PreExecutionScriptProgram(
        txSignatureComponent = txSigComponent,
        stack = List(OP_TRUE),
        script = Nil,
        originalScript = txSigComponent.scriptPubKey.asm.toList,
        altStack = Nil,
        flags = flags
      )
      val evaluated = executeProgram(inProgress)
      Success(evaluated)
    }
  }
}

object ScriptInterpreter extends ScriptInterpreter {

  val bip341DisabledOpCodes = {
    Vector(OP_CAT,
           OP_SUBSTR,
           OP_LEFT,
           OP_RIGHT,
           OP_INVERT,
           OP_AND,
           OP_OR,
           OP_XOR,
           OP_RESERVED1,
           OP_RESERVED2,
           OP_2MUL,
           OP_2DIV,
           OP_MUL,
           OP_DIV,
           OP_MOD,
           OP_LSHIFT,
           OP_RSHIFT)
  }
}
