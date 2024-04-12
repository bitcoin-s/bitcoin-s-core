package org.bitcoins.core.util

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  EmptyScriptPubKey,
  _
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTMap
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{
  OP_CHECKMULTISIG,
  OP_CHECKMULTISIGVERIFY,
  OP_CHECKSIG,
  OP_CHECKSIGVERIFY
}
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptFlagUtil}
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{
  ScriptError,
  ScriptErrorPubKeyType,
  ScriptErrorWitnessPubKeyType,
  ScriptOk
}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.script.{
  ExecutionInProgressScriptProgram,
  PreExecutionScriptProgram
}
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.{ECDigitalSignature, ECPublicKeyBytes}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/** Created by chris on 3/2/16.
  */
trait BitcoinScriptUtil {

  /** Takes in a sequence of script tokens and converts them to their hexadecimal value */
  def asmToHex(asm: Seq[ScriptToken]): String = {
    val hex = asm.map(_.hex).mkString
    hex
  }

  /** Converts a sequence of script tokens to them to their byte values */
  def asmToBytes(asm: Seq[ScriptToken]): ByteVector =
    BytesUtil.decodeHex(asmToHex(asm))

  /** Filters out push operations in our sequence of script tokens
    * this removes
    * [[org.bitcoins.core.script.constant.OP_PUSHDATA1 OP_PUSHDATA1]],
    * [[org.bitcoins.core.script.constant.OP_PUSHDATA2 OP_PUSHDATA2]],
    * [[org.bitcoins.core.script.constant.OP_PUSHDATA4 OP_PUSHDATA4]],
    * and all [[org.bitcoins.core.script.constant.BytesToPushOntoStack ByteToPushOntoStack]] tokens
    */
  def filterPushOps(asm: Seq[ScriptToken]): Seq[ScriptToken] = {
    //TODO: This does not remove the following script number after a OP_PUSHDATA
    asm.filterNot(op =>
      op.isInstanceOf[BytesToPushOntoStack]
        || op == OP_PUSHDATA1
        || op == OP_PUSHDATA2
        || op == OP_PUSHDATA4)
  }

  /** Returns only the data ScriptTokens in a script that are pushed onto the stack */
  def getDataTokens(asm: Seq[ScriptToken]): Seq[ScriptToken] = {
    val builder = Vector.newBuilder[ScriptToken]

    asm.zipWithIndex.foreach { case (token, index) =>
      token match {
        case OP_PUSHDATA1 | OP_PUSHDATA2 | OP_PUSHDATA4 =>
          /* OP_PUSH_DATA[1|2|4] says that the next value is [1|2|4] bytes and indicates
           * how many bytes should be pushed onto the stack (meaning the data is 2 values away)
           */
          builder.+=(asm(index + 2))
        case _: BytesToPushOntoStack =>
          builder.+=(asm(index + 1))
        case _ => ()
      }
    }

    builder.result()
  }

  /** Returns true if the given script token counts towards our max script operations in a script
    * See
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L269-L271 interpreter.cpp#L269-L271]]
    * which is how Bitcoin Core handles this
    */
  def countsTowardsScriptOpLimit(token: ScriptToken): Boolean =
    token match {
      case scriptOp: ScriptOperation if scriptOp.opCode > OP_16.opCode => true
      case _: ScriptToken                                              => false
    }

  def countOpCodes(opcodes: Vector[ScriptToken]): Int = {
    opcodes.count(_.isInstanceOf[ScriptOperation])
  }

  def filterOpCodes(tokens: Vector[ScriptToken]): Vector[ScriptToken] = {
    tokens.filter(_.isInstanceOf[ScriptOperation])
  }

  /** Counts the amount of sigops in a script.
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/script.cpp#L156-L202 Bitcoin Core script.cpp]]
    * @param script the script whose sigops are being counted
    * @return the number of signature operations in the script
    */
  def countSigOps(script: Seq[ScriptToken]): Long = {
    val checkSigCount =
      script.count(token => token == OP_CHECKSIG || token == OP_CHECKSIGVERIFY)
    val multiSigOps = Seq(OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY)
    val multiSigCount: Long = script.zipWithIndex
      .map[Long] { case (token, index) =>
        if (multiSigOps.contains(token) && index != 0) {
          script(index - 1) match {
            case scriptNum: ScriptNumber => scriptNum.toLong
            case scriptConstant: ScriptConstant =>
              ScriptNumberUtil.toLong(scriptConstant.hex)
            case _: ScriptToken => Consensus.maxPublicKeysPerMultiSig
          }
        } else 0
      }
      .sum
    checkSigCount + multiSigCount
  }

  /** Parses the number of signatures on the stack
    * This can only be called when an [[org.bitcoins.core.script.crypto.OP_CHECKMULTISIG OP_CHECKMULTISIG]]
    * operation is about to be executed
    * on the stack
    * For instance if this was a 2/3 multisignature script, it would return the number 3
    */
  def numPossibleSignaturesOnStack(
      program: ExecutionInProgressScriptProgram): ScriptNumber = {
    require(
      program.script.headOption
        .contains(OP_CHECKMULTISIG) || program.script.headOption
        .contains(OP_CHECKMULTISIGVERIFY),
      "We can only parse the nubmer of signatures the stack when we are executing a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY op"
    )
    val nPossibleSignatures: ScriptNumber = program.stack.head match {
      case s: ScriptNumber   => s
      case s: ScriptConstant => ScriptNumber(s.bytes)
      case _: ScriptToken =>
        throw new RuntimeException(
          "n must be a script number or script constant for OP_CHECKMULTISIG")
    }
    nPossibleSignatures
  }

  /** Returns the number of required signatures on the stack, for instance if this was a
    * 2/3 multisignature script, it would return the number 2
    */
  def numRequiredSignaturesOnStack(
      program: ExecutionInProgressScriptProgram): ScriptNumber = {
    require(
      program.script.headOption
        .contains(OP_CHECKMULTISIG) || program.script.headOption
        .contains(OP_CHECKMULTISIGVERIFY),
      "We can only parse the nubmer of signatures the stack when we are executing a OP_CHECKMULTISIG or OP_CHECKMULTISIGVERIFY op"
    )
    val nPossibleSignatures = numPossibleSignaturesOnStack(program)
    val stackWithoutPubKeys = program.stack.tail
      .slice(nPossibleSignatures.toInt, program.stack.tail.size)
    val mRequiredSignatures: ScriptNumber = stackWithoutPubKeys.head match {
      case s: ScriptNumber   => s
      case s: ScriptConstant => ScriptNumber(s.bytes)
      case _: ScriptToken =>
        throw new RuntimeException(
          "m must be a script number or script constant for OP_CHECKMULTISIG")
    }
    mRequiredSignatures
  }

  /** Determines if a script contains only script operations
    * This is equivalent to
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/script.cpp#L213 Bitcoin Core script.cpp#L213]]
    */
  def isPushOnly(script: Seq[ScriptToken]): Boolean = {
    @tailrec
    def loop(tokens: Seq[ScriptToken]): Boolean =
      tokens match {
        case h +: t =>
          h match {
            case scriptOp: ScriptOperation =>
              if (scriptOp.opCode < OP_16.opCode) {
                loop(t)
              } else {
                false
              }

            case _: ScriptToken => loop(t)
          }
        case Nil => true
      }
    loop(script)
  }

  /** Determines if the token being pushed onto the stack is being pushed by the SMALLEST push operation possible
    * This is equivalent to
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L209 Bitcoin Core interpreter.cpp#L209]]
    * @param pushOp the operation that is pushing the data onto the stack
    * @param token the token that is being pushed onto the stack by the pushOp
    * @return
    */
  def isMinimalPush(pushOp: ScriptToken, token: ScriptToken): Boolean =
    token match {
      case scriptNumOp: ScriptNumberOperation =>
        scriptNumOp == pushOp
      case ScriptConstant.zero | ScriptConstant.negativeZero =>
        //weird case where OP_0 pushes an empty byte vector on the stack, NOT "00" or "81"
        //so we can push the constant "00" or "81" onto the stack with a BytesToPushOntoStack pushop
        pushOp == BytesToPushOntoStack(1)
      case _: ScriptToken
          if token.bytes.size == 1 && ScriptNumberOperation
            .fromNumber(token.toLong.toInt)
            .isDefined =>
        //could have used the ScriptNumberOperation to push the number onto the stack
        false
      case token: ScriptToken =>
        token.bytes.size match {
          case size if size == 0     => pushOp == OP_0
          case size if size <= 75    => token.bytes.size == pushOp.toLong
          case size if size <= 255   => pushOp == OP_PUSHDATA1
          case size if size <= 65535 => pushOp == OP_PUSHDATA2
          case _: Long               =>
            //default case is true because we have to use the largest push op as possible which is OP_PUSHDATA4
            true
        }
    }

  /** Calculates the push operation for the given [[org.bitcoins.core.script.constant.ScriptToken ScriptToken]] */
  def calculatePushOp(scriptToken: ScriptToken): Seq[ScriptToken] = {
    //push ops following an OP_PUSHDATA operation are interpreted as unsigned numbers
    val scriptTokenSize = UInt32(scriptToken.bytes.size)
    val bytes = scriptTokenSize.bytes
    if (
      scriptToken
        .isInstanceOf[ScriptNumberOperation] || scriptToken.bytes.size <= 0
    )
      Nil
    else if (scriptTokenSize <= UInt32(75))
      Seq(BytesToPushOntoStack(scriptToken.bytes.size))
    else if (scriptTokenSize <= UInt32(OP_PUSHDATA1.max)) {
      //we need the push op to be only 1 byte in size
      val pushConstant = ScriptConstant(
        BytesUtil.flipEndianness(bytes.slice(bytes.length - 1, bytes.length)))
      Seq(OP_PUSHDATA1, pushConstant)
    } else if (scriptTokenSize <= UInt32(OP_PUSHDATA2.max)) {
      //we need the push op to be only 2 bytes in size
      val pushConstant = ScriptConstant(
        BytesUtil.flipEndianness(bytes.slice(bytes.length - 2, bytes.length)))
      Seq(OP_PUSHDATA2, pushConstant)
    } else if (scriptTokenSize <= UInt32(OP_PUSHDATA4.max)) {
      val pushConstant = ScriptConstant(BytesUtil.flipEndianness(bytes))
      Seq(OP_PUSHDATA4, pushConstant)
    } else
      throw new IllegalArgumentException(
        "ScriptToken is to large for pushops, size: " + scriptTokenSize)
  }

  def calculatePushOp(bytes: ByteVector): Seq[ScriptToken] =
    calculatePushOp(ScriptConstant(bytes))

  /** Whenever a [[org.bitcoins.core.script.constant.ScriptConstant ScriptConstant]] is interpreted to a number
    * BIP62 could enforce that number to be encoded
    * in the smallest encoding possible
    * [[https://github.com/bitcoin/bitcoin/blob/a6a860796a44a2805a58391a009ba22752f64e32/src/script/script.h#L220-L237]]
    */
  def isShortestEncoding(constant: ScriptConstant): Boolean =
    isShortestEncoding(constant.bytes)

  def isShortestEncoding(bytes: ByteVector): Boolean = {
    // If the most-significant-byte - excluding the sign bit - is zero
    // then we're not minimal. Note how this test also rejects the
    // negative-zero encoding, 0x80.
    if (bytes.size > 0 && (bytes.last & 0x7f) == 0) {
      // One exception: if there's more than one byte and the most
      // significant bit of the second-most-significant-byte is set
      // it would conflict with the sign bit. An example of this case
      // is +-255, which encode to 0xff00 and 0xff80 respectively.
      // (big-endian).
      if (bytes.size <= 1 || (bytes(bytes.size - 2) & 0x80) == 0) {
        false
      } else true
    } else true
  }

  /** Whenever a script constant is interpreted to a number BIP62 should enforce that number to be encoded
    * in the smallest encoding possible
    * [[https://github.com/bitcoin/bitcoin/blob/a6a860796a44a2805a58391a009ba22752f64e32/src/script/script.h#L220-L237]]
    */
  def isShortestEncoding(hex: String): Boolean =
    isShortestEncoding(BytesUtil.decodeHex(hex))

  /** Checks if the token is minimially encoded */
  def isMinimalToken(token: ScriptToken): Boolean = {
    //see: https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L447-L452
    //https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2016-August/013014.html
    val tooBig = token.bytes.size > 1
    val sizeZero = token.bytes.isEmpty
    lazy val startsWithOne = token.bytes.head == 1

    !tooBig && (sizeZero || startsWithOne)
  }

  /** Checks the [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] encoding according to bitcoin core's function:
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L202]].
    */
  def checkPubKeyEncoding(
      key: ECPublicKeyBytes,
      program: ExecutionInProgressScriptProgram): Boolean =
    checkPubKeyEncoding(key, program.flags)

  def checkPubKeyEncoding(
      key: ECPublicKeyBytes,
      flags: Seq[ScriptFlag]): Boolean = {
    if (
      ScriptFlagUtil.requireStrictEncoding(flags) &&
      !isCompressedOrUncompressedPubKey(key)
    ) false
    else true
  }

  /** Returns true if the key is compressed or uncompressed, false otherwise
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L66]]
    * @param key the public key that is being checked
    * @return true if the key is compressed/uncompressed otherwise false
    */
  def isCompressedOrUncompressedPubKey(key: ECPublicKeyBytes): Boolean = {
    if (key.bytes.size < 33) {
      //  Non-canonical public key: too short
      return false
    }
    if (key.bytes.head == 0x04) {
      if (key.bytes.size != 65) {
        //  Non-canonical public key: invalid length for uncompressed key
        return false
      }
    } else if (isCompressedPubKey(key)) {
      return true
    } else {
      //  Non-canonical public key: neither compressed nor uncompressed
      return false
    }
    true
  }

  /** Checks if the given public key is a compressed public key */
  def isCompressedPubKey(key: ECPublicKeyBytes): Boolean = {
    (key.bytes.size == 33) && (key.bytes.head == 0x02 || key.bytes.head == 0x03)
  }

  def minimalScriptNumberRepresentation(num: ScriptNumber): ScriptNumber = {
    val op = ScriptNumberOperation.fromNumber(num.toLong)
    if (op.isDefined) op.get else num
  }

  /** Determines if the given pubkey is valid in accordance to the given
    * [[org.bitcoins.core.script.flag.ScriptFlag ScriptFlag]]s
    * and [[org.bitcoins.core.protocol.script.SignatureVersion SignatureVersion]].
    * Mimics this function inside of Bitcoin Core
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L214-L223]]
    */
  def isValidPubKeyEncoding(
      pubKey: ECPublicKeyBytes,
      sigVersion: SignatureVersion,
      flags: Seq[ScriptFlag]): Option[ScriptError] = {
    if (
      ScriptFlagUtil.requireStrictEncoding(flags) &&
      !BitcoinScriptUtil.isCompressedOrUncompressedPubKey(pubKey)
    ) {
      Some(ScriptErrorPubKeyType)
    } else if (
      ScriptFlagUtil.requireScriptVerifyWitnessPubKeyType(flags) &&
      !BitcoinScriptUtil.isCompressedPubKey(
        pubKey) && sigVersion == SigVersionWitnessV0
    ) {
      Some(ScriptErrorWitnessPubKeyType)
    } else None
  }

  /** Prepares the script we spending to be serialized for our transaction signature serialization algorithm
    * We need to check if the scriptSignature has a redeemScript
    * In that case, we need to pass the redeemScript to the TransactionSignatureChecker
    *
    * In the case we have a P2SH(P2WSH) we need to pass the witness's redeem script to the
    * [[org.bitcoins.core.crypto.TransactionSignatureChecker TransactionSignatureChecker]]
    * instead of passing the
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]] inside of the
    * [[org.bitcoins.core.protocol.script.P2SHScriptSignature P2SHScriptSignature]]'s redeem script.
    */
  def calculateScriptForChecking(
      txSignatureComponent: TxSigComponent,
      signature: ECDigitalSignature,
      script: Seq[ScriptToken]): Seq[ScriptToken] = {
    val scriptForChecking =
      calculateScriptForSigning(txSignatureComponent, script)
    txSignatureComponent.sigVersion match {
      case SigVersionBase =>
        removeSignatureFromScript(signature, scriptForChecking)
      case SigVersionWitnessV0 | SigVersionTaprootKeySpend |
          SigVersionTapscript =>
        //BIP143 removes requirement for calling FindAndDelete
        //https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#no-findanddelete
        scriptForChecking
    }
  }

  def calculateScriptForSigning(
      txSignatureComponent: TxSigComponent,
      script: Seq[ScriptToken]): Seq[ScriptToken] =
    txSignatureComponent.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        val p2shScriptSig = P2SHScriptSignature(
          txSignatureComponent.scriptSignature.bytes)

        p2shScriptSig.redeemScript match {
          case p2wpkh: P2WPKHWitnessSPKV0 =>
            //we treat p2sh(p2wpkh) differently for script signing than other spks
            //Please note that for a P2SH-P2WPKH, the scriptCode is always 26 bytes including the leading size byte,
            // as 0x1976a914{20-byte keyhash}88ac, NOT the redeemScript nor scriptPubKey
            //https://bitcoincore.org/en/segwit_wallet_dev/#signature-generation-and-verification-for-p2sh-p2wpkh

            P2PKHScriptPubKey(p2wpkh.pubKeyHash).asm

          case _: P2WSHWitnessSPKV0 =>
            val wtxSig =
              txSignatureComponent.asInstanceOf[WitnessTxSigComponentP2SH]

            val p2wshRedeem =
              ScriptPubKey.fromAsmBytes(wtxSig.witness.stack.head)
            p2wshRedeem.asm
          case _: ScriptPubKey =>
            val sigsRemoved = removeSignaturesFromScript(
              p2shScriptSig.signatures,
              p2shScriptSig.redeemScript.asm)
            sigsRemoved
        }

      case w: WitnessScriptPubKey =>
        txSignatureComponent match {
          case wtxSigComponent: WitnessTxSigComponent =>
            val scriptE = w.witnessVersion.rebuild(wtxSigComponent.witness, w)
            parseScriptEither(scriptE)
          case rWTxSigComponent: WitnessTxSigComponentRebuilt =>
            rWTxSigComponent.scriptPubKey.asm
          case _: BaseTxSigComponent =>
            //shouldn't have BaseTxSigComponent
            //with a witness scriptPubKey
            script
        }
      case _: P2PKHScriptPubKey | _: P2PKScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          _: CLTVScriptPubKey | _: CSVScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey =>
        script
    }

  def calculateScriptForSigning(
      spendingTransaction: Transaction,
      signingInfo: InputSigningInfo[InputInfo],
      script: Seq[ScriptToken]): Seq[ScriptToken] = {

    val idx = TxUtil.inputIndex(signingInfo.inputInfo, spendingTransaction)

    signingInfo.output.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        val p2sh = signingInfo.inputInfo.asInstanceOf[P2SHInputInfo]

        p2sh.redeemScript match {
          case p2wpkh: P2WPKHWitnessSPKV0 =>
            //we treat p2sh(p2wpkh) differently for script signing than other spks
            //Please note that for a P2SH-P2WPKH, the scriptCode is always 26 bytes including the leading size byte,
            // as 0x1976a914{20-byte keyhash}88ac, NOT the redeemScript nor scriptPubKey
            //https://bitcoincore.org/en/segwit_wallet_dev/#signature-generation-and-verification-for-p2sh-p2wpkh

            P2PKHScriptPubKey(p2wpkh.pubKeyHash).asm

          case _: P2WSHWitnessSPKV0 =>
            val wtx =
              spendingTransaction.asInstanceOf[WitnessTransaction]

            val p2wshRedeem =
              ScriptPubKey.fromAsmBytes(wtx.witness.witnesses(idx).stack.head)
            p2wshRedeem.asm
          case script: ScriptPubKey =>
            script.asm
        }

      case w: WitnessScriptPubKey =>
        val wtx = spendingTransaction.asInstanceOf[WitnessTransaction]
        val scriptT =
          w.witnessVersion.rebuild(wtx.witness.witnesses(idx), w)
        parseScriptEither(scriptT)

      case _: P2PKHScriptPubKey | _: P2PKScriptPubKey |
          _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
          _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
          _: CLTVScriptPubKey | _: CSVScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey =>
        script
    }
  }

  /** Removes the given [[ECDigitalSignature ECDigitalSignature]] from the list of
    * [[org.bitcoins.core.script.constant.ScriptToken ScriptToken]] if it exists.
    */
  def removeSignatureFromScript(
      signature: ECDigitalSignature,
      script: Seq[ScriptToken]): Seq[ScriptToken] = {
    if (script.contains(ScriptConstant(signature.hex))) {
      //replicates this line in bitcoin core
      //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L872
      val sigIndex = script.indexOf(ScriptConstant(signature.hex))
      //remove sig and it's corresponding BytesToPushOntoStack
      val sigRemoved =
        script.slice(0, sigIndex - 1) ++ script.slice(sigIndex + 1, script.size)
      sigRemoved
    } else script
  }

  /** Removes the list of [[ECDigitalSignature ECDigitalSignature]] from the list of
    * [[org.bitcoins.core.script.constant.ScriptToken ScriptToken]]
    */
  def removeSignaturesFromScript(
      sigs: Seq[ECDigitalSignature],
      script: Seq[ScriptToken]): Seq[ScriptToken] = {
    @tailrec
    def loop(
        remainingSigs: Seq[ECDigitalSignature],
        scriptTokens: Seq[ScriptToken]): Seq[ScriptToken] = {
      remainingSigs match {
        case Nil => scriptTokens
        case h +: t =>
          val newScriptTokens = removeSignatureFromScript(h, scriptTokens)
          loop(t, newScriptTokens)
      }
    }
    loop(sigs, script)
  }

  /** Removes the [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR OP_CODESEPARATOR]]
    * in the original script according to
    * the last code separator index in the script.
    */
  def removeOpCodeSeparator(
      program: ExecutionInProgressScriptProgram): Seq[ScriptToken] = {
    if (program.lastCodeSeparator.isDefined) {
      program.originalScript
        .slice(program.lastCodeSeparator.get + 1, program.originalScript.size)
    } else program.originalScript
  }

  private def parseScriptEither(
      scriptT: Either[ScriptError, ScriptPubKey]): Seq[ScriptToken] =
    scriptT match {
      case Right(scriptPubKey) => scriptPubKey.asm
      case Left(err) =>
        throw new IllegalArgumentException(
          s"Could not parse witness script, got err=$err")
    }

  /** Casts the given script token to a boolean value
    * Mimics this function inside of Bitcoin Core
    * [[https://github.com/bitcoin/bitcoin/blob/8c1dbc5e9ddbafb77e60e8c4e6eb275a3a76ac12/src/script/interpreter.cpp#L38]]
    * All bytes in the byte vector must be zero, unless it is the last byte, which can be 0 or 0x80 (negative zero)
    */
  def castToBool(token: ScriptToken): Boolean = {
    token.bytes.toArray.zipWithIndex.exists { case (b, index) =>
      val byteNotZero = b.toByte != 0
      val lastByteNotNegativeZero =
        !(index == token.bytes.size - 1 && b.toByte == 0x80.toByte)
      byteNotZero && lastByteNotNegativeZero
    }
  }

  /** Since witnesses are not run through the interpreter, replace
    * `OP_0`/`OP_1` with `ScriptNumber.zero`/`ScriptNumber.one`
    */
  def minimalIfOp(asm: Seq[ScriptToken]): Seq[ScriptToken] = {
    asm.map {
      case OP_0               => ScriptNumber.zero
      case OP_1               => ScriptNumber.one
      case token: ScriptToken => token
    }
  }

  /** Replaces the [[org.bitcoins.core.script.constant.OP_0 OP_0]] dummy for
    * [[org.bitcoins.core.script.crypto.OP_CHECKMULTISIG OP_CHECKMULTISIG]] with
    * [[org.bitcoins.core.script.constant.ScriptNumber.zero ScriptNumber.zero]]
    */
  def minimalDummy(asm: Seq[ScriptToken]): Seq[ScriptToken] = {
    if (asm.headOption.contains(OP_0)) ScriptNumber.zero +: asm.tail
    else asm
  }

  /** Checks that all the [[org.bitcoins.crypto.ECPublicKey ECPublicKey]] in this script
    * is compressed public keys, this is required for BIP143
    */
  def isOnlyCompressedPubKey(spk: ScriptPubKey): Boolean = {
    spk match {
      case p2pk: P2PKScriptPubKey => p2pk.publicKey.isCompressed
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        p2pkWithTimeout.pubKey.isCompressed && p2pkWithTimeout.timeoutPubKey.isCompressed
      case m: MultiSignatureScriptPubKey =>
        m.publicKeys.forall(_.isCompressed)
      case l: LockTimeScriptPubKey =>
        isOnlyCompressedPubKey(l.nestedScriptPubKey)
      case conditional: ConditionalScriptPubKey =>
        isOnlyCompressedPubKey(conditional.trueSPK) && isOnlyCompressedPubKey(
          conditional.falseSPK)
      case _: P2PKHScriptPubKey | _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
          _: P2WSHWitnessSPKV0 | _: UnassignedWitnessScriptPubKey |
          _: NonStandardScriptPubKey | _: WitnessCommitment |
          _: TaprootScriptPubKey | EmptyScriptPubKey =>
        true

    }
  }

  def parseScript[T <: Script](
      bytes: ByteVector,
      f: Vector[ScriptToken] => T): T = {
    val compactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)
    //TODO: Figure out a better way to do this, we can theoretically have numbers larger than Int.MaxValue,
    //but scala collections don't allow you to use 'slice' with longs
    val len = Try(compactSizeUInt.num.toInt).getOrElse(Int.MaxValue)
    val scriptPubKeyBytes =
      bytes.slice(compactSizeUInt.byteSize.toInt,
                  len + compactSizeUInt.byteSize.toInt)
    val script: Vector[ScriptToken] = ScriptParser.fromBytes(scriptPubKeyBytes)
    f(script)
  }

  def verifyScript(
      tx: Transaction,
      utxos: Seq[ScriptSignatureParams[InputInfo]]): Boolean = {
    val programs: Seq[PreExecutionScriptProgram] = tx.inputs.zipWithIndex.map {
      case (input: TransactionInput, idx: Int) =>
        val outpoint = input.previousOutput

        val creditingTx = utxos.find(u => u.outPoint == outpoint).get

        val output = creditingTx.output

        val spk = output.scriptPubKey

        val amount = output.value

        val txSigComponent = spk match {
          case witSPK: WitnessScriptPubKeyV0 =>
            val o = TransactionOutput(amount, witSPK)
            WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction],
                                     UInt32(idx),
                                     o,
                                     Policy.standardFlags)
          case _: UnassignedWitnessScriptPubKey | _: TaprootScriptPubKey =>
            ???
          case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
              _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
              _: WitnessCommitment | _: CSVScriptPubKey | _: CLTVScriptPubKey |
              _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
              EmptyScriptPubKey) =>
            val o = TransactionOutput(CurrencyUnits.zero, x)
            BaseTxSigComponent(tx, UInt32(idx), o, Policy.standardFlags)

          case _: P2SHScriptPubKey =>
            val p2shScriptSig =
              tx.inputs(idx).scriptSignature.asInstanceOf[P2SHScriptSignature]
            p2shScriptSig.redeemScript match {

              case _: WitnessScriptPubKey =>
                WitnessTxSigComponentP2SH(transaction =
                                            tx.asInstanceOf[WitnessTransaction],
                                          inputIndex = UInt32(idx),
                                          output = output,
                                          flags = Policy.standardFlags)

              case _ =>
                BaseTxSigComponent(tx,
                                   UInt32(idx),
                                   output,
                                   Policy.standardFlags)
            }
        }

        PreExecutionScriptProgram(txSigComponent)
    }
    ScriptInterpreter.runAllVerify(programs)
  }

  def verifyPSBTInputScript(
      tx: Transaction,
      inputMap: InputPSBTMap,
      index: Int,
      outputMap: PreviousOutputMap,
      flags: Seq[ScriptFlag] = Policy.standardFlags): Try[Transaction] = {

    val txIn = tx.inputs(index)

    val (preImages, condPath) =
      InputInfo.getHashPreImagesAndConditionalPath(tx, index)

    val inputInfo = inputMap.toInputInfo(txIn,
                                         conditionalPath = condPath,
                                         preImages = preImages)

    val txSigComponent = TxSigComponent(inputInfo, tx, outputMap, flags)

    val inputResult =
      ScriptInterpreter.run(PreExecutionScriptProgram(txSigComponent))

    if (inputResult == ScriptOk) {
      Success(tx)
    } else {
      Failure(new RuntimeException(s"Input $index was invalid: $inputResult"))
    }
  }
}

object BitcoinScriptUtil extends BitcoinScriptUtil
