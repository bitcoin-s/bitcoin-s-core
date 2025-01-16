package org.bitcoins.core.protocol.script

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.protocol.script.descriptor.DescriptorType
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, _}
import org.bitcoins.core.script.control._
import org.bitcoins.core.script.crypto.{
  OP_CHECKMULTISIG,
  OP_CHECKMULTISIGVERIFY,
  OP_CHECKSIG,
  OP_HASH160
}
import org.bitcoins.core.script.locktime.{
  OP_CHECKLOCKTIMEVERIFY,
  OP_CHECKSEQUENCEVERIFY
}
import org.bitcoins.core.script.reserved.UndefinedOP_NOP
import org.bitcoins.core.script.stack.{OP_DROP, OP_DUP}
import org.bitcoins.core.util._
import org.bitcoins.crypto._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/** Created by chris on 12/26/15.
  */
sealed abstract class ScriptPubKey extends Script {
  val scriptType: ScriptType
}

/** Trait for all Non-SegWit ScriptPubKeys */
sealed trait NonWitnessScriptPubKey extends ScriptPubKey

/** Trait for all raw, non-nested ScriptPubKeys (no P2SH)
  *
  * Note that all WitnessScriptPubKeys including P2WPKH are not considered to be
  * non-nested and hence not RawScriptPubKeys.
  */
sealed trait RawScriptPubKey extends NonWitnessScriptPubKey

/** Represents a
  * [[https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh pay-to-pubkey hash script pubkey]]
  *
  * Format: `OP_DUP OP_HASH160 <PubKeyHash> OP_EQUALVERIFY OP_CHECKSIG`
  */
sealed trait P2PKHScriptPubKey extends RawScriptPubKey {

  def pubKeyHash: Sha256Hash160Digest =
    Sha256Hash160Digest(asm(asm.length - 3).bytes)

  override def toString = s"${DescriptorType.PK.toString}(${pubKeyHash.hex})"
}

object P2PKHScriptPubKey extends ScriptFactory[P2PKHScriptPubKey] {

  private case class P2PKHScriptPubKeyImpl(
      override val asm: Vector[ScriptToken])
      extends P2PKHScriptPubKey {
    override val scriptType: ScriptType = ScriptType.PUBKEYHASH

    override def toString = s"${DescriptorType.PKH.toString}(${pubKeyHash.hex})"
  }

  def apply(pubKey: ECPublicKey): P2PKHScriptPubKey = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHScriptPubKey(hash)
  }

  def apply(hash: Sha256Hash160Digest): P2PKHScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm =
      Seq(OP_DUP, OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes),
                                                OP_EQUALVERIFY,
                                                OP_CHECKSIG)
    P2PKHScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKHScriptPubKey = {
    buildScript(asm.toVector,
                P2PKHScriptPubKeyImpl.apply,
                "Given asm was not a p2pkh scriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): P2PKHScriptPubKey = fromAsm(asm)

  /** Checks if the given asm matches the pattern for
    * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey P2PKHScriptPubKey]]
    */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    asm match {
      case Seq(OP_DUP,
               OP_HASH160,
               _: BytesToPushOntoStack,
               _: ScriptConstant,
               OP_EQUALVERIFY,
               OP_CHECKSIG) =>
        true
      case _ => false
    }
  }
}

/** Represents a multisignature script public key
  * https://bitcoin.org/en/developer-guide#multisig Format: <m> <A pubkey> [B
  * pubkey] [C pubkey...] <n> OP_CHECKMULTISIG
  */
sealed trait MultiSignatureScriptPubKey extends RawScriptPubKey {

  def requiredSigsScriptNumber: ScriptNumber = {
    val asmWithoutPushOps = asm.filterNot(_.isInstanceOf[BytesToPushOntoStack])
    val opCheckMultiSigIndex =
      if (asm.indexOf(OP_CHECKMULTISIG) != -1)
        asmWithoutPushOps.indexOf(OP_CHECKMULTISIG)
      else asmWithoutPushOps.indexOf(OP_CHECKMULTISIGVERIFY)
    // magic number 2 represents the maxSig operation and the OP_CHECKMULTISIG operation at the end of the asm
    val numSigsRequired = asmWithoutPushOps(opCheckMultiSigIndex - maxSigs - 2)
    numSigsRequired match {
      case x: ScriptNumber => x
      case c: ScriptConstant =>
        val sn = ScriptNumber(c.bytes)
        val inBounds =
          sn >= ScriptNumber.zero && sn.toInt <= Consensus.maxPublicKeysPerMultiSig
        if (inBounds) {
          sn
        } else {
          sys.error(
            s"Negative numSignaturesRequired given for MultiSignatureSPK, got=$sn")
        }
      case _ =>
        throw new RuntimeException(
          "The first element of the multisignature pubkey must be a script number operation\n" +
            "operation: " + numSigsRequired +
            "\nscriptPubKey: " + this)
    }
  }

  /** Returns the amount of required signatures for this multisignature script
    * pubkey output
    */
  def requiredSigs: Int = {
    requiredSigsScriptNumber.toInt
  }

  def maxSigsScriptNumber: ScriptNumber = {
    if (checkMultiSigIndex == -1 || checkMultiSigIndex == 0) {
      // means that we do not have a max signature requirement
      OP_0
    } else {
      asm(checkMultiSigIndex - 1) match {
        case x: ScriptNumber => x
        case c: ScriptConstant =>
          val maxSigs = ScriptNumber(c.bytes)
          val inBounds =
            maxSigs >= ScriptNumber.zero && maxSigs.toInt <= Consensus.maxPublicKeysPerMultiSig
          if (inBounds) {
            maxSigs
          } else {
            sys.error(
              s"Negative maxSigs given for MultiSignatureSPK, got=$maxSigs")
          }
        case x =>
          throw new RuntimeException(
            "The element preceding a OP_CHECKMULTISIG operation in a  multisignature pubkey must be a script number operation, got: " + x)
      }
    }
  }

  /** The maximum amount of signatures for this multisignature script pubkey
    * output
    */
  def maxSigs: Int = {
    maxSigsScriptNumber.toInt
  }

  /** Gives the `OP_CHECKMULTISIG` or `OP_CHECKMULTISIGVERIFY` index inside of
    * asm
    */
  private def checkMultiSigIndex: Int = {
    if (asm.indexOf(OP_CHECKMULTISIG) != -1) asm.indexOf(OP_CHECKMULTISIG)
    else asm.indexOf(OP_CHECKMULTISIGVERIFY)
  }

  /** Returns the public keys encoded into the `scriptPubKey` */
  def publicKeys: Seq[ECPublicKeyBytes] = {
    MultiSignatureScriptPubKey.parsePublicKeys(asm)
  }

  override def toString =
    s"${DescriptorType.Multi.toString}($requiredSigs,${publicKeys.mkString(",")})"
}

object MultiSignatureScriptPubKey
    extends ScriptFactory[MultiSignatureScriptPubKey] {

  private case class MultiSignatureScriptPubKeyImpl(
      override val asm: Vector[ScriptToken])
      extends MultiSignatureScriptPubKey {

    override val scriptType: ScriptType = ScriptType.MULTISIG

    override def toString =
      s"multi($requiredSigs,${publicKeys.map(_.hex).mkString(",")})"
  }

  def apply(
      requiredSigs: Int,
      pubKeys: Seq[ECPublicKey]): MultiSignatureScriptPubKey = {
    require(
      requiredSigs <= Consensus.maxPublicKeysPerMultiSig,
      "We cannot have more required signatures than: " +
        Consensus.maxPublicKeysPerMultiSig + " got: " + requiredSigs
    )
    require(
      pubKeys.length <= Consensus.maxPublicKeysPerMultiSig,
      "We cannot have more public keys than " +
        Consensus.maxPublicKeysPerMultiSig + " got: " + pubKeys.length
    )

    val required = ScriptNumberOperation.fromNumber(requiredSigs) match {
      case Some(scriptNumOp) => Seq(scriptNumOp)
      case None =>
        val scriptNum = ScriptNumber(requiredSigs)
        val pushOps = BitcoinScriptUtil.calculatePushOp(scriptNum.bytes)
        pushOps ++ Seq(scriptNum)
    }
    val possible = ScriptNumberOperation.fromNumber(pubKeys.length) match {
      case Some(scriptNumOp) => Seq(scriptNumOp)
      case None =>
        val scriptNum = ScriptNumber(pubKeys.length)
        val pushOps = BitcoinScriptUtil.calculatePushOp(scriptNum)
        pushOps ++ Seq(scriptNum)
    }
    val pubKeysWithPushOps: Seq[Seq[ScriptToken]] = for {
      pubKey <- pubKeys
      pushOps = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
      constant = ScriptConstant(pubKey.bytes)
    } yield pushOps ++ Seq(constant)
    val asm: Seq[ScriptToken] =
      required ++ pubKeysWithPushOps.flatten ++ possible ++ Seq(
        OP_CHECKMULTISIG)
    MultiSignatureScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): MultiSignatureScriptPubKey = {
    buildScript(asm.toVector,
                MultiSignatureScriptPubKeyImpl.apply,
                "Given asm was not a MultSignatureScriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): MultiSignatureScriptPubKey = fromAsm(asm)

  private val opCmsOPs = Vector(OP_CHECKMULTISIG, OP_CHECKMULTISIGVERIFY)

  /** Determines if the given script tokens are a multisignature `scriptPubKey`
    */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    if (asm.nonEmpty && opCmsOPs.exists(_ == asm.last)) {
      val cmsIdx = asm.size - 1
      // we need either the first or second asm operation to indicate how many signatures are required
      val hasRequiredSignaturesOpt: Option[Int] = {
        asm.headOption match {
          case None        => None
          case Some(token) =>
            // this is for the case that we have more than 16 public keys, the
            // first operation will be a push op, the second operation being the actual number of keys
            token match {
              case _: BytesToPushOntoStack =>
                isValidPubKeyNumber(asm.tail.head)
              case _ => isValidPubKeyNumber(token)
            }
        }
      }
      // the second to last asm operation should be the maximum amount of public keys
      val hasMaximumSignaturesOpt: Option[Int] = {
        val maxSigsIdx = asm.length - 2
        if (maxSigsIdx >= cmsIdx || maxSigsIdx <= 0) {
          None
        } else {
          asm(maxSigsIdx) match {
            case token: ScriptToken => isValidPubKeyNumber(token)
          }
        }
      }

      (hasRequiredSignaturesOpt, hasMaximumSignaturesOpt) match {
        case (Some(_), Some(maximumSignatures)) =>
          val isStandardOps = asm.forall(op =>
            op.isInstanceOf[ScriptConstant] || op
              .isInstanceOf[BytesToPushOntoStack] || op
              .isInstanceOf[ScriptNumber] || op == OP_CHECKMULTISIG ||
              op == OP_CHECKMULTISIGVERIFY)

          val result =
            asm.nonEmpty &&
              hasMaximumSignatures(maximumSignatures, asm) && isStandardOps
          result
        case (Some(_), None) => false
        case (None, Some(_)) => false
        case (None, None)    => false
      }
    } else {
      false
    }
  }

  def parsePublicKeys(asm: Seq[ScriptToken]): Seq[ECPublicKeyBytes] = {
    val keys = asm
      .dropRight(2) // drop maxSigs, OP_CMS op
      .filter(_.isInstanceOf[ScriptConstant])
      .slice(1, asm.length) // drop requiredSigs
      .map(t => ECPublicKeyBytes(t.bytes))
    keys
  }

  private def hasMaximumSignatures(
      maxSigs: Int,
      asm: Seq[ScriptToken]): Boolean = {
    parsePublicKeys(asm).size == maxSigs
  }

  /** Checks that the given script token is with the range of the maximum amount
    * of public keys we can have in a
    * [[org.bitcoins.core.protocol.script.MultiSignatureScriptPubKey MultiSignatureScriptPubKey]]
    */
  @tailrec private def isValidPubKeyNumber(token: ScriptToken): Option[Int] = {
    token match {
      case sn: ScriptNumber =>
        if (
          sn >= ScriptNumber.zero && sn <= ScriptNumber(
            Consensus.maxPublicKeysPerMultiSig)
        ) {
          Some(sn.toInt)
        } else {
          None
        }
      case constant: ScriptConstant =>
        // Consensus.maxPublicKeysPerMultiSig is at most 20, which can be represented by 2 bytes
        if (constant.bytes.size <= 2) {
          val sn = ScriptNumber.fromBytes(constant.bytes)
          isValidPubKeyNumber(sn)
        } else {
          None
        }
      case _: ScriptToken => None
    }
  }
}

/** Represents a
  * [[https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh pay-to-scripthash public key]]
  * Format: `OP_HASH160 <Hash160(redeemScript)> OP_EQUAL`
  */
sealed trait P2SHScriptPubKey extends NonWitnessScriptPubKey {

  /** The hash of the script for which this scriptPubKey is being created from
    */
  def scriptHash: Sha256Hash160Digest =
    Sha256Hash160Digest(asm(asm.length - 2).bytes)

  override def toString = s"${DescriptorType.SH.toString}(${scriptHash.hex})"
}

object P2SHScriptPubKey extends ScriptFactory[P2SHScriptPubKey] {

  private case class P2SHScriptPubKeyImpl(override val asm: Vector[ScriptToken])
      extends P2SHScriptPubKey {
    override val scriptType: ScriptType = ScriptType.SCRIPTHASH
  }

  def apply(scriptPubKey: ScriptPubKey): P2SHScriptPubKey = {
    require(!scriptPubKey.isInstanceOf[P2SHScriptPubKey],
            s"Cannot do p2sh(p2sh()), got=$scriptPubKey")
    val hash = CryptoUtil.sha256Hash160(scriptPubKey.asmBytes)
    P2SHScriptPubKey(hash)
  }

  def apply(hash: Sha256Hash160Digest): P2SHScriptPubKey = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    val asm =
      Seq(OP_HASH160) ++ pushOps ++ Seq(ScriptConstant(hash.bytes), OP_EQUAL)
    P2SHScriptPubKey(asm)
  }

  /** Checks if the given asm matches the pattern for
    * [[org.bitcoins.core.protocol.script.P2SHScriptPubKey P2SHScriptPubKey]]
    */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean =
    asm match {
      case Seq(OP_HASH160,
               _: BytesToPushOntoStack,
               _: ScriptConstant,
               OP_EQUAL) =>
        true
      case _ => false
    }

  def fromAsm(asm: Seq[ScriptToken]): P2SHScriptPubKey = {
    buildScript(asm.toVector,
                P2SHScriptPubKeyImpl.apply,
                "Given asm was not a p2sh scriptPubkey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): P2SHScriptPubKey = fromAsm(asm)
}

/** Represents a
  * [[https://bitcoin.org/en/developer-guide#pubkey pay to public key script public key]]
  * Format: `<pubkey> OP_CHECKSIG`
  */
sealed trait P2PKScriptPubKey extends RawScriptPubKey {

  def publicKey: ECPublicKeyBytes =
    ECPublicKeyBytes(BitcoinScriptUtil.filterPushOps(asm).head.bytes)

  override def toString = s"${DescriptorType.PK.toString}(${publicKey.hex})"

}

object P2PKScriptPubKey extends ScriptFactory[P2PKScriptPubKey] {

  private case class P2PKScriptPubKeyImpl(override val asm: Vector[ScriptToken])
      extends P2PKScriptPubKey {
    override val scriptType: ScriptType = ScriptType.PUBKEY
  }

  def apply(pubKey: PublicKey): P2PKScriptPubKey = {
    // comeback and review the type on this constructor,
    // depending on the output type, the key type isn't safe
    // i.e xonly can't be used pre-taproot and vice versa
    val pushOps = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
    val asm = pushOps ++ Seq(ScriptConstant(pubKey.bytes), OP_CHECKSIG)
    P2PKScriptPubKey(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptPubKey = {
    buildScript(asm.toVector,
                P2PKScriptPubKeyImpl.apply,
                "Given asm was not a p2pk scriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): P2PKScriptPubKey = fromAsm(asm)

  /** Sees if the given asm matches the
    * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey P2PKHScriptPubKey]]
    * pattern
    */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean =
    asm match {
      case Seq(_: BytesToPushOntoStack, _: ScriptConstant, OP_CHECKSIG) => true
      case _                                                            => false
    }

  /** Builds a P2PKScriptPubKey from a specific branch of a P2PKWithTimeout.
    * Useful for when decomposing a script into spending branches.
    */
  private[core] def fromP2PKWithTimeout(
      p2pkWithTimeout: P2PKWithTimeoutScriptPubKey,
      timeoutBranch: Boolean): P2PKScriptPubKey = {
    val pubKeyBytes =
      if (timeoutBranch) p2pkWithTimeout.timeoutPubKey
      else p2pkWithTimeout.pubKey
    val pushOps = BitcoinScriptUtil.calculatePushOp(pubKeyBytes.bytes)
    val asm = pushOps ++ Seq(ScriptConstant(pubKeyBytes.bytes), OP_CHECKSIG)

    P2PKScriptPubKey(asm)
  }

}

sealed trait LockTimeScriptPubKey extends RawScriptPubKey {

  /** Determines the nested `ScriptPubKey` inside the `LockTimeScriptPubKey` */
  def nestedScriptPubKey: RawScriptPubKey = {
    asm.head match {
      case _: ScriptNumberOperation => RawScriptPubKey(asm.slice(3, asm.length))
      case _: ScriptToken           => RawScriptPubKey(asm.slice(4, asm.length))
    }
  }

  /** The relative locktime value (i.e. the amount of time the output should
    * remain unspendable)
    */
  def locktime: ScriptNumber = {
    asm.head match {
      case scriptNumOp: ScriptNumberOperation =>
        ScriptNumber(scriptNumOp.toLong)
      case _: BytesToPushOntoStack => ScriptNumber(asm(1).bytes)
      case _: ScriptConstant | _: ScriptOperation =>
        throw new IllegalArgumentException(
          "In a LockTimeScriptPubKey, " +
            "the first asm must be either a ScriptNumberOperation (i.e. OP_5), or the BytesToPushOntoStack " +
            "for the proceeding ScriptConstant.")
    }
  }
}

object LockTimeScriptPubKey extends ScriptFactory[LockTimeScriptPubKey] {

  def fromAsm(asm: Seq[ScriptToken]): LockTimeScriptPubKey = {
    require(isValidAsm(asm))
    if (asm.contains(OP_CHECKLOCKTIMEVERIFY)) CLTVScriptPubKey(asm)
    else if (asm.contains(OP_CHECKSEQUENCEVERIFY)) CSVScriptPubKey(asm)
    else
      throw new IllegalArgumentException(
        "Given asm was not a LockTimeScriptPubKey, got: " + asm)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    CLTVScriptPubKey.isValidAsm(asm) || CSVScriptPubKey
      .isValidAsm(asm)
  }
}

/** Represents a scriptPubKey that contains `OP_CHECKLOCKTIMEVERIFY.` Adds an
  * absolute/defined locktime condition to any scriptPubKey.
  * [[https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki BIP65]]
  * Format: `<locktime> OP_CLTV OP_DROP <scriptPubKey>`
  */
sealed trait CLTVScriptPubKey extends LockTimeScriptPubKey

object CLTVScriptPubKey extends ScriptFactory[CLTVScriptPubKey] {

  private case class CLTVScriptPubKeyImpl(override val asm: Vector[ScriptToken])
      extends CLTVScriptPubKey {
    override val scriptType: ScriptType = ScriptType.CLTV

    override def toString = s"CLTVScriptPubKey($locktime, $nestedScriptPubKey)"
  }

  def fromAsm(asm: Seq[ScriptToken]): CLTVScriptPubKey = {
    buildScript(asm.toVector,
                CLTVScriptPubKeyImpl.apply,
                "Given asm was not a CLTVScriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): CLTVScriptPubKey = fromAsm(asm)

  def apply(
      locktime: ScriptNumber,
      scriptPubKey: ScriptPubKey): CLTVScriptPubKey = {
    val scriptOp = BitcoinScriptUtil.minimalScriptNumberRepresentation(locktime)

    val scriptNum: Seq[ScriptToken] =
      if (scriptOp.isInstanceOf[ScriptNumberOperation]) {
        Seq(scriptOp)
      } else {
        val pushOpsLockTime = BitcoinScriptUtil.calculatePushOp(locktime.bytes)
        pushOpsLockTime ++ Seq(ScriptConstant(locktime.bytes))
      }

    val cltvAsm = Seq(OP_CHECKLOCKTIMEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = scriptNum ++ cltvAsm ++ scriptPubKeyAsm
    CLTVScriptPubKey(asm)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    if (asm.isEmpty) {
      false
    } else if (asm.head.isInstanceOf[BytesToPushOntoStack]) {
      val tailTokens = asm.slice(4, asm.length)
      if (
        P2SHScriptPubKey.isValidAsm(tailTokens) || tailTokens
          .contains(OP_CHECKLOCKTIMEVERIFY)
      ) {
        false
      } else {
        asm.slice(0, 4) match {
          case Seq(_: BytesToPushOntoStack,
                   s: ScriptConstant,
                   OP_CHECKLOCKTIMEVERIFY,
                   OP_DROP) =>
            // can only have up to 5 byte numbers for CLTV
            s.byteSize <= 5 && validScriptAfterLockTime(tailTokens)
          case _ => false
        }
      }

    } else {
      val tailTokens = asm.slice(3, asm.length)
      if (
        P2SHScriptPubKey.isValidAsm(tailTokens) || tailTokens
          .contains(OP_CHECKLOCKTIMEVERIFY)
      ) {
        false
      } else {
        asm.slice(0, 3) match {
          case Seq(_: ScriptNumberOperation, OP_CHECKLOCKTIMEVERIFY, OP_DROP) =>
            validScriptAfterLockTime(tailTokens)
          case _ => false
        }
      }

    }
  }

  /** We need this check because sometimes we can get very lucky in having a non
    * valid lock time script that has the first 4 bytes as a valid locktime
    * script and then the bytes after the first 4 bytes gets lucky and is parsed
    * by our [[org.bitcoins.core.serializers.script.ScriptParser ScriptParser]]
    * A good way to see if this is _actually_ a valid script is by checking if
    * we have any
    * [[org.bitcoins.core.script.reserved.UndefinedOP_NOP UndefinedOP_NOP]] in
    * the script, which means we definitely don't have a valid locktime script
    *
    * See this example of what happened before we added this check:
    * [[https://travis-ci.org/bitcoin-s/bitcoin-s-core/builds/201652191#L2526 Travis CI]]
    */
  def validScriptAfterLockTime(asm: Seq[ScriptToken]): Boolean = {
    !asm.exists(_.isInstanceOf[UndefinedOP_NOP])
  }
}

/** Represents a scriptPubKey that contains
  * [[org.bitcoins.core.script.locktime.OP_CHECKSEQUENCEVERIFY OP_CHECKSEQUENCEVERIFY]]
  * Adds a relative lockTime condition to any `scriptPubKey`.
  * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki BIP112]]
  * Format: `<locktime> OP_CSV OP_DROP <scriptPubKey>`
  */
sealed trait CSVScriptPubKey extends LockTimeScriptPubKey

object CSVScriptPubKey extends ScriptFactory[CSVScriptPubKey] {

  private case class CSVScriptPubKeyImpl(override val asm: Vector[ScriptToken])
      extends CSVScriptPubKey {
    override val scriptType: ScriptType = ScriptType.CSV

    override def toString = s"CSVScriptPubKey($locktime, $nestedScriptPubKey)"
  }

  def fromAsm(asm: Seq[ScriptToken]): CSVScriptPubKey = {
    buildScript(asm.toVector,
                CSVScriptPubKeyImpl.apply,
                "Given asm was not a CSVScriptPubKey, got: " + asm)
  }

  def apply(asm: Seq[ScriptToken]): CSVScriptPubKey = fromAsm(asm)

  def apply(
      relativeLockTime: ScriptNumber,
      scriptPubKey: ScriptPubKey): CSVScriptPubKey = {
    val scriptOp =
      BitcoinScriptUtil.minimalScriptNumberRepresentation(relativeLockTime)

    val scriptNum: Seq[ScriptToken] =
      if (scriptOp.isInstanceOf[ScriptNumberOperation]) {
        Seq(scriptOp)
      } else {
        val pushOpsLockTime =
          BitcoinScriptUtil.calculatePushOp(relativeLockTime.bytes)
        pushOpsLockTime ++ Seq(ScriptConstant(relativeLockTime.bytes))
      }

    val csvAsm = Seq(OP_CHECKSEQUENCEVERIFY, OP_DROP)
    val scriptPubKeyAsm = scriptPubKey.asm
    val asm = scriptNum ++ csvAsm ++ scriptPubKeyAsm
    CSVScriptPubKey(asm)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    if (asm.head.isInstanceOf[BytesToPushOntoStack]) {
      val tailTokens = asm.slice(4, asm.length)
      if (
        P2SHScriptPubKey.isValidAsm(tailTokens) || tailTokens
          .contains(OP_CHECKSEQUENCEVERIFY)
      ) {
        false
      } else {
        asm.slice(0, 4) match {
          case Seq(_: BytesToPushOntoStack,
                   s: ScriptConstant,
                   OP_CHECKSEQUENCEVERIFY,
                   OP_DROP) =>
            // check that the byteSize of the ScriptNum is less than or equal to 5
            // as per BIP112
            s.byteSize <= 5 &&
            CLTVScriptPubKey.validScriptAfterLockTime(tailTokens)
          case _ => false
        }
      }

    } else {
      val tailTokens = asm.slice(3, asm.length)
      if (
        P2SHScriptPubKey.isValidAsm(tailTokens) || tailTokens
          .contains(OP_CHECKSEQUENCEVERIFY)
      ) {
        false
      } else {
        asm.slice(0, 3) match {
          case Seq(_: ScriptNumberOperation, OP_CHECKSEQUENCEVERIFY, OP_DROP) =>
            CLTVScriptPubKey.validScriptAfterLockTime(tailTokens)
          case _ => false
        }
      }
    }
  }

}

/** Currently only supports a single OP_IF ... OP_ELSE ... OP_ENDIF ScriptPubKey
  */
sealed trait ConditionalScriptPubKey extends RawScriptPubKey {
  def conditional: ConditionalOperation

  require(asm.headOption.contains(conditional),
          s"ConditionalScriptPubKey must begin with $conditional")
  require(asm.last.equals(OP_ENDIF),
          "ConditionalScriptPubKey must end in OP_ENDIF")

  val (isValidConditional: Boolean, opElseIndexOpt: Option[Int]) = {
    ConditionalScriptPubKey.isConditionalScriptPubKeyWithElseIndex(asm,
                                                                   conditional)
  }

  require(isValidConditional, "Must be valid ConditionalScriptPubKey syntax")
  require(opElseIndexOpt.isDefined,
          "ConditionalScriptPubKey has to contain OP_ELSE asm token")

  val opElseIndex: Int = opElseIndexOpt.get

  require(
    !WitnessScriptPubKey
      .isValidAsm(trueSPK.asm) && !WitnessScriptPubKey
      .isValidAsm(falseSPK.asm),
    "ConditionalScriptPubKey cannot wrap SegWit ScriptPubKey"
  )

  def firstSPK: RawScriptPubKey = {
    RawScriptPubKey
      .fromAsm(asm.slice(1, opElseIndex))
  }

  def secondSPK: RawScriptPubKey = {
    RawScriptPubKey
      .fromAsm(asm.slice(opElseIndex + 1, asm.length - 1))
  }

  def trueSPK: RawScriptPubKey = {
    conditional match {
      case OP_IF    => firstSPK
      case OP_NOTIF => secondSPK
    }
  }

  def falseSPK: RawScriptPubKey = {
    conditional match {
      case OP_IF    => secondSPK
      case OP_NOTIF => firstSPK
    }
  }
}

object ConditionalScriptPubKey {

  /** Validates the correctness of the conditional syntax. If valid, also
    * returns the index of the first outer-most OP_ELSE
    */
  def isConditionalScriptPubKeyWithElseIndex(
      asm: Seq[ScriptToken],
      conditional: ConditionalOperation): (Boolean, Option[Int]) = {
    val headIsConditional = asm.headOption.contains(conditional)
    lazy val endsWithEndIf = asm.last == OP_ENDIF

    var opElseIndexOpt: Option[Int] = None

    lazy val validConditionalTree = {
      // Already validate that the first token is OP_IF, go to tail
      // and start with depth 1 and no OP_ELSE found for that depth
      val opElsePendingOpt = asm.zipWithIndex.tail
        .foldLeft[Option[Vector[Boolean]]](Some(Vector(false))) {
          case (None, _) => // Invalid tree case, do no computation
            None
          case (Some(Vector()),
                _
              ) => // Case of additional asm after final OP_ENDIF
            None
          case (Some(opElseFoundAtDepth), (OP_IF | OP_NOTIF, _)) =>
            // Increase depth by one with OP_ELSE yet to be found for this depth
            Some(opElseFoundAtDepth :+ false)
          case (Some(opElseFoundAtDepth), (OP_ELSE, asmIndex)) =>
            if (opElseFoundAtDepth == Vector(false)) {
              // If first OP_ELSE at depth 1, set opElseIndex
              opElseIndexOpt = Some(asmIndex)
            }

            if (opElseFoundAtDepth.last) {
              // If OP_ELSE already found at this depth, invalid
              None
            } else {
              // Otherwise, set to found at this depth
              Some(
                opElseFoundAtDepth.updated(opElseFoundAtDepth.length - 1, true))
            }
          case (Some(opElseFoundAtDepth), (OP_ENDIF, _)) =>
            if (opElseFoundAtDepth.last) {
              // If OP_ELSE found at this depth then valid, decrease depth by 1
              Some(opElseFoundAtDepth.dropRight(1))
            } else {
              // Otherwise, invalid
              None
            }
          case (Some(opElseFoundAtDepth), (_, _)) =>
            // Token not related to conditional structure, ignore
            Some(opElseFoundAtDepth)
        }

      // We should end on OP_ENDIF which will take us to depth 0
      opElsePendingOpt.contains(Vector.empty)
    }

    (headIsConditional && endsWithEndIf && validConditionalTree, opElseIndexOpt)
  }

  def apply(
      conditional: ConditionalOperation,
      trueSPK: RawScriptPubKey,
      falseSPK: RawScriptPubKey): ConditionalScriptPubKey = {
    conditional match {
      case OP_IF =>
        (trueSPK, falseSPK) match {
          case (multisig: MultiSignatureScriptPubKey,
                timeout: CLTVScriptPubKey) =>
            MultiSignatureWithTimeoutScriptPubKey(multisig, timeout)
          case _ =>
            NonStandardIfConditionalScriptPubKey(trueSPK, falseSPK)
        }
      case OP_NOTIF =>
        NonStandardNotIfConditionalScriptPubKey(falseSPK, trueSPK)
    }
  }
}

sealed trait IfConditionalScriptPubKey extends ConditionalScriptPubKey {
  override def conditional: ConditionalOperation = OP_IF
}

object NonStandardIfConditionalScriptPubKey
    extends ScriptFactory[IfConditionalScriptPubKey] {

  private case class NonStandardIfConditionalScriptPubKeyImpl(
      override val asm: Vector[ScriptToken])
      extends IfConditionalScriptPubKey {
    override val scriptType: ScriptType = ScriptType.NONSTANDARD_IF_CONDITIONAL

    override def toString: String =
      s"IfConditionalScriptPubKey($trueSPK, $falseSPK)"
  }

  override def fromAsm(asm: Seq[ScriptToken]): IfConditionalScriptPubKey = {
    buildScript(
      asm = asm.toVector,
      constructor = NonStandardIfConditionalScriptPubKeyImpl.apply,
      errorMsg =
        "Given asm was not a NonStandardIfConditionalScriptPubKey, got: " + asm
    )
  }

  def apply(
      trueSPK: RawScriptPubKey,
      falseSPK: RawScriptPubKey): IfConditionalScriptPubKey = {
    val asm =
      Vector(OP_IF) ++ trueSPK.asm ++ Vector(OP_ELSE) ++ falseSPK.asm ++ Vector(
        OP_ENDIF)

    fromAsm(asm)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    val validIf = ConditionalScriptPubKey
      .isConditionalScriptPubKeyWithElseIndex(asm, OP_IF)
      ._1

    val isMultiSigWithTimeout = MultiSignatureWithTimeoutScriptPubKey
      .isValidAsm(asm)

    validIf && !isMultiSigWithTimeout
  }
}

sealed trait MultiSignatureWithTimeoutScriptPubKey
    extends IfConditionalScriptPubKey {
  require(MultiSignatureScriptPubKey.isValidAsm(super.firstSPK.asm),
          "True case must be MultiSignatureSPK")
  require(CLTVScriptPubKey.isValidAsm(super.secondSPK.asm),
          "False case must be CLTVSPK")

  override def firstSPK: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey.fromAsm(asm.slice(1, opElseIndex))
  }

  override def secondSPK: CLTVScriptPubKey = {
    CLTVScriptPubKey.fromAsm(asm.slice(opElseIndex + 1, asm.length - 1))
  }

  override def trueSPK: MultiSignatureScriptPubKey = firstSPK
  override def falseSPK: CLTVScriptPubKey = secondSPK

  def multiSigSPK: MultiSignatureScriptPubKey = firstSPK
  def timeoutSPK: CLTVScriptPubKey = secondSPK

  def requiredSigs: Int = multiSigSPK.requiredSigs
  def maxSigs: Int = multiSigSPK.maxSigs
  def timeout: Int = timeoutSPK.locktime.toInt
}

object MultiSignatureWithTimeoutScriptPubKey
    extends ScriptFactory[MultiSignatureWithTimeoutScriptPubKey] {

  private case class MultiSignatureWithTimeoutScriptPubKeyImpl(
      override val asm: Vector[ScriptToken])
      extends MultiSignatureWithTimeoutScriptPubKey {
    override val scriptType: ScriptType = ScriptType.MULTISIG_WITH_TIMEOUT

    override def toString: String =
      s"MultiSignatureWithTimeoutScriptPubKey($trueSPK, $falseSPK)"
  }

  override def fromAsm(
      asm: Seq[ScriptToken]): MultiSignatureWithTimeoutScriptPubKey = {
    buildScript(
      asm = asm.toVector,
      constructor = MultiSignatureWithTimeoutScriptPubKeyImpl.apply,
      errorMsg =
        s"Given asm was not a MultiSignatureWithTimeoutScriptPubKey, got: $asm"
    )
  }

  def apply(
      multiSigSPK: MultiSignatureScriptPubKey,
      cltvSPK: CLTVScriptPubKey): MultiSignatureWithTimeoutScriptPubKey = {
    val asm = Vector(OP_IF) ++ multiSigSPK.asm ++ Vector(
      OP_ELSE) ++ cltvSPK.asm ++ Vector(OP_ENDIF)

    fromAsm(asm)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    val (validIf, opElseIndexOpt) = ConditionalScriptPubKey
      .isConditionalScriptPubKeyWithElseIndex(asm, OP_IF)

    lazy val validMultiSigWithCLTV = opElseIndexOpt match {
      case Some(opElseIndex) =>
        MultiSignatureScriptPubKey
          .isValidAsm(asm.slice(1, opElseIndex)) && CLTVScriptPubKey
          .isValidAsm(asm.slice(opElseIndex + 1, asm.length - 1))
      case _ => false
    }

    validIf && validMultiSigWithCLTV
  }
}

sealed trait NotIfConditionalScriptPubKey extends ConditionalScriptPubKey {
  override def conditional: ConditionalOperation = OP_NOTIF
}

object NonStandardNotIfConditionalScriptPubKey
    extends ScriptFactory[NotIfConditionalScriptPubKey] {

  private case class NonStandardNotIfConditionalScriptPubKeyImpl(
      override val asm: Vector[ScriptToken])
      extends NotIfConditionalScriptPubKey {
    override val scriptType: ScriptType = ScriptType.NOT_IF_CONDITIONAL

    override def toString: String =
      s"NotIfConditionalScriptPubKey($falseSPK, $trueSPK)"
  }

  override def fromAsm(asm: Seq[ScriptToken]): NotIfConditionalScriptPubKey = {
    buildScript(
      asm = asm.toVector,
      constructor = NonStandardNotIfConditionalScriptPubKeyImpl.apply,
      errorMsg = "Given asm was not a NotIfConditionalScriptPubKey, got: " + asm
    )
  }

  def apply(
      falseSPK: RawScriptPubKey,
      trueSPK: RawScriptPubKey): NotIfConditionalScriptPubKey = {
    val asm = Vector(OP_NOTIF) ++ falseSPK.asm ++ Vector(
      OP_ELSE) ++ trueSPK.asm ++ Vector(OP_ENDIF)

    fromAsm(asm)
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    ConditionalScriptPubKey
      .isConditionalScriptPubKeyWithElseIndex(asm, OP_NOTIF)
      ._1
  }
}

/** The type for ScriptPubKeys of the form: OP_IF <Public Key> OP_ELSE <Timeout>
  * OP_CHECKSEQUENCEVERIFY OP_DROP <Timeout Public Key> OP_ENDIF OP_CHECKSIG
  */
sealed trait P2PKWithTimeoutScriptPubKey extends RawScriptPubKey {

  lazy val pubKey: ECPublicKeyBytes =
    ECPublicKeyBytes(asm(2).bytes)

  private lazy val smallCSVOpt: Option[Long] = {
    asm(4) match {
      case num: ScriptNumberOperation => Some(num.toLong)
      case _: ScriptToken             => None
    }
  }

  lazy val lockTime: ScriptNumber = {
    smallCSVOpt
      .map(ScriptNumber.apply)
      .getOrElse(ScriptNumber(asm(5).bytes))
  }

  lazy val timeoutPubKey: ECPublicKeyBytes = {
    smallCSVOpt match {
      case Some(_) => ECPublicKeyBytes(asm(8).bytes)
      case None    => ECPublicKeyBytes(asm(9).bytes)
    }
  }
}

object P2PKWithTimeoutScriptPubKey
    extends ScriptFactory[P2PKWithTimeoutScriptPubKey] {

  private case class P2PKWithTimeoutScriptPubKeyImpl(asm: Vector[ScriptToken])
      extends P2PKWithTimeoutScriptPubKey {
    override val scriptType: ScriptType = ScriptType.PUBKEY_WITH_TIMEOUT
  }

  override def fromAsm(asm: Seq[ScriptToken]): P2PKWithTimeoutScriptPubKey = {
    buildScript(
      asm = asm.toVector,
      constructor = P2PKWithTimeoutScriptPubKeyImpl.apply,
      errorMsg = s"Given asm was not a P2PKWithTimeoutScriptPubKey, got $asm"
    )
  }

  def apply(
      pubKey: ECPublicKey,
      lockTime: ScriptNumber,
      timeoutPubKey: ECPublicKey): P2PKWithTimeoutScriptPubKey = {
    val timeoutAsm = CSVScriptPubKey(lockTime, EmptyScriptPubKey).asm.toVector
    val pubKeyAsm = BitcoinScriptUtil
      .calculatePushOp(pubKey.bytes)
      .toVector ++ Vector(ScriptConstant(pubKey.bytes))
    val timeoutPubKeyAsm = BitcoinScriptUtil
      .calculatePushOp(timeoutPubKey.bytes)
      .toVector ++ Vector(ScriptConstant(timeoutPubKey.bytes))

    P2PKWithTimeoutScriptPubKeyImpl(
      Vector(Vector(OP_IF),
             pubKeyAsm,
             Vector(OP_ELSE),
             timeoutAsm,
             timeoutPubKeyAsm,
             Vector(OP_ENDIF, OP_CHECKSIG)).flatten
    )
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    if (asm.length < 5) {
      false
    } else {
      val (smallCSVOpt, requiredSize) = asm(4) match {
        case num: ScriptNumberOperation => (Some(num.toLong), 11)
        case _: ScriptToken             => (None, 12)
      }

      if (asm.length == requiredSize) {
        val lockTimeTry = smallCSVOpt match {
          case Some(num) => Success(ScriptNumber(num))
          case None      => Try(ScriptNumber.fromBytes(asm(5).bytes))
        }

        val timeoutPubKeyTry = Try {
          smallCSVOpt match {
            case Some(_) => ECPublicKey.fromBytes(asm(8).bytes)
            case None    => ECPublicKey.fromBytes(asm(9).bytes)
          }
        }

        if (timeoutPubKeyTry.isSuccess && lockTimeTry.isSuccess) {
          // this check is expensive, so do it only if we need it
          val pubKeyTry = Try(ECPublicKey.fromBytes(asm(2).bytes))
          pubKeyTry match {
            case Success(pubKey) =>
              asm == P2PKWithTimeoutScriptPubKey(pubKey,
                                                 lockTimeTry.get,
                                                 timeoutPubKeyTry.get).asm
            case Failure(_) => false
          }
        } else {
          false
        }
      } else {
        false
      }
    }
  }
}

sealed trait NonStandardScriptPubKey extends RawScriptPubKey

object NonStandardScriptPubKey extends ScriptFactory[NonStandardScriptPubKey] {

  private case class NonStandardScriptPubKeyImpl(
      override val asm: Vector[ScriptToken])
      extends NonStandardScriptPubKey {
    override val scriptType: ScriptType = ScriptType.NONSTANDARD

    override def toString = s"NonStandardScriptPubKey($asm)"
  }

  def fromAsm(asm: Seq[ScriptToken]): NonStandardScriptPubKey = {
    // everything can be a NonStandardScriptPubkey, thus the trivially true function
    buildScript(asm.toVector, NonStandardScriptPubKeyImpl.apply, "")
  }

  def apply(asm: Seq[ScriptToken]): NonStandardScriptPubKey = fromAsm(asm)

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = true
}

/** Represents the empty ScriptPubKey */
case object EmptyScriptPubKey extends RawScriptPubKey {
  override def asm: Seq[ScriptToken] = Vector.empty

  override val scriptType: ScriptType = ScriptType.NULLDATA
}

object RawScriptPubKey extends ScriptFactory[RawScriptPubKey] {
  val empty: RawScriptPubKey = fromAsm(Nil)

  def fromAsm(asm: Seq[ScriptToken]): RawScriptPubKey =
    asm match {
      case Nil => EmptyScriptPubKey
      case _ if P2PKWithTimeoutScriptPubKey.isValidAsm(asm) =>
        P2PKWithTimeoutScriptPubKey.fromAsm(asm)
      case _
          if MultiSignatureWithTimeoutScriptPubKey
            .isValidAsm(asm) =>
        MultiSignatureWithTimeoutScriptPubKey.fromAsm(asm)
      case _
          if NonStandardIfConditionalScriptPubKey
            .isValidAsm(asm) =>
        NonStandardIfConditionalScriptPubKey.fromAsm(asm)
      case _
          if NonStandardNotIfConditionalScriptPubKey
            .isValidAsm(asm) =>
        NonStandardNotIfConditionalScriptPubKey.fromAsm(asm)
      case _ if P2PKHScriptPubKey.isValidAsm(asm) =>
        P2PKHScriptPubKey(asm)
      case _ if P2PKScriptPubKey.isValidAsm(asm) =>
        P2PKScriptPubKey(asm)
      case _ if MultiSignatureScriptPubKey.isValidAsm(asm) =>
        MultiSignatureScriptPubKey(asm)
      case _ if CLTVScriptPubKey.isValidAsm(asm) =>
        CLTVScriptPubKey(asm)
      case _ if CSVScriptPubKey.isValidAsm(asm) => CSVScriptPubKey(asm)
      case _ if WitnessCommitment.isValidAsm(asm) =>
        WitnessCommitment(asm)
      case _ => NonStandardScriptPubKey(asm)
    }

  def apply(asm: Seq[ScriptToken]): RawScriptPubKey = fromAsm(asm)

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    !WitnessScriptPubKey.isValidAsm(asm) && !P2SHScriptPubKey.isValidAsm(asm)
  }
}

object NonWitnessScriptPubKey extends ScriptFactory[NonWitnessScriptPubKey] {
  val empty: NonWitnessScriptPubKey = fromAsm(Nil)

  override def fromAsm(asm: Seq[ScriptToken]): NonWitnessScriptPubKey = {
    if (P2SHScriptPubKey.isValidAsm(asm)) {
      P2SHScriptPubKey(asm)
    } else {
      RawScriptPubKey.fromAsm(asm)
    }
  }

  def apply(asm: Seq[ScriptToken]): NonWitnessScriptPubKey = fromAsm(asm)

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    !WitnessScriptPubKey.isValidAsm(asm)
  }
}

/** Factory companion object used to create
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] objects
  */
object ScriptPubKey extends ScriptFactory[ScriptPubKey] {
  val empty: ScriptPubKey = fromAsm(Nil)

  /** Creates a `scriptPubKey` from its asm representation */
  override def fromAsm(asm: Seq[ScriptToken]): ScriptPubKey = {
    val nonWitnessScriptPubKey = NonWitnessScriptPubKey.fromAsm(asm)
    if (
      nonWitnessScriptPubKey
        .isInstanceOf[NonStandardScriptPubKey] && WitnessScriptPubKey
        .isValidAsm(asm)
    ) {
      WitnessScriptPubKey(asm)
    } else {
      nonWitnessScriptPubKey
    }
  }

  def apply(asm: Seq[ScriptToken]): ScriptPubKey = fromAsm(asm)

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = true

}

/** This type represents a
  * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] to evaluate
  * a [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]]
  */
sealed trait WitnessScriptPubKey extends ScriptPubKey {
  def witnessProgram: Seq[ScriptToken]
  def witnessVersion: WitnessVersion = WitnessVersion(asm.head)
}

object WitnessScriptPubKey extends ScriptFactory[WitnessScriptPubKey] {

  /** Witness scripts must begin with one of these operations, see
    * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP141]]
    */
  val validWitVersions: Seq[ScriptNumberOperation] = Seq(OP_0,
                                                         OP_1,
                                                         OP_2,
                                                         OP_3,
                                                         OP_4,
                                                         OP_5,
                                                         OP_6,
                                                         OP_7,
                                                         OP_8,
                                                         OP_9,
                                                         OP_10,
                                                         OP_11,
                                                         OP_12,
                                                         OP_13,
                                                         OP_14,
                                                         OP_15,
                                                         OP_16)

  val unassignedWitVersions: Seq[ScriptNumberOperation] =
    validWitVersions.tail.tail

  def apply(asm: Seq[ScriptToken]): WitnessScriptPubKey = fromAsm(asm)

  def fromAsm(asm: Seq[ScriptToken]): WitnessScriptPubKey =
    asm match {
      case _ if P2WPKHWitnessSPKV0.isValidAsm(asm) =>
        P2WPKHWitnessSPKV0.fromAsm(asm)
      case _ if P2WSHWitnessSPKV0.isValidAsm(asm) =>
        P2WSHWitnessSPKV0.fromAsm(asm)
      case _ if TaprootScriptPubKey.isValidAsm(asm) =>
        TaprootScriptPubKey.fromAsm(asm)
      case _ if WitnessScriptPubKey.isValidAsm(asm) =>
        UnassignedWitnessScriptPubKey(asm)
      case _ =>
        throw new IllegalArgumentException(
          "Given asm was not a WitnessScriptPubKey, got: " + asm)
    }

  /** Checks if the given asm is a valid
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    * Mimics
    * [[https://github.com/bitcoin/bitcoin/blob/14d01309bed59afb08651f2b701ff90371b15b20/src/script/script.cpp#L223-L237 this function]]
    * inside of Bitcoin Core
    */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {

    // multisig spk with zero public keys
    // OP_0, BytesToPushOntoStackImpl(3), ScriptConstantImpl(ByteVector(3 bytes, 0x0000ae)

    // multisig spk with one public key
    // List(OP_0, BytesToPushOntoStackImpl(37),
    // ScriptConstantImpl(ByteVector(37 bytes, 0x0021020afd6012af90835558c68365a370b7e6cd1c0d4664a8656c8c7847185cb5db6651ae)))

    // we can also have a LockTimeScriptPubKey with a nested 0 public key multisig script, need to check that as well

    // it turns out this method gets called a lot when we attempt
    // to type check spks, so let's do a cheap check before deserializing
    // everything to a byte vector which is expensive
    val firstOp = asm.headOption
    if (!validWitVersions.contains(firstOp.getOrElse(OP_1NEGATE))) {
      false
    } else {
      val bytes = BytesUtil.toByteVector(asm)
      if (bytes.length < 4 || bytes.length > 42) false
      else if (MultiSignatureScriptPubKey.isValidAsm(asm)) false
      else if (LockTimeScriptPubKey.isValidAsm(asm)) false
      else if (asm(1).toLong + 2 == bytes.size) true
      else false
    }
  }
}

/** Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#witness-program BIP141 Witness program]]
  */
sealed abstract class WitnessScriptPubKeyV0 extends WitnessScriptPubKey {
  override def witnessProgram: Seq[ScriptToken] = asm.tail.tail
}

object WitnessScriptPubKeyV0 extends ScriptFactory[WitnessScriptPubKeyV0] {

  /** Mimics the function to determine if a
    * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] contains a
    * witness A witness program is any valid
    * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] that
    * consists of a 1 byte push op and then a data push between 2 and 40 bytes
    * Verison 0 witness program need to have an OP_0 as the first operation
    * [[https://github.com/bitcoin/bitcoin/blob/449f9b8debcceb61a92043bc7031528a53627c47/src/script/script.cpp#L215-L229]]
    */
  def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    asm.headOption.contains(OP_0) && WitnessScriptPubKey.isValidAsm(asm)
  }

  def apply(asm: Seq[ScriptToken]): WitnessScriptPubKeyV0 = fromAsm(asm)

  override def fromAsm(asm: Seq[ScriptToken]): WitnessScriptPubKeyV0 =
    asm match {
      case _ if P2WPKHWitnessSPKV0.isValidAsm(asm) =>
        P2WPKHWitnessSPKV0.fromAsm(asm)
      case _ if P2WSHWitnessSPKV0.isValidAsm(asm) =>
        P2WSHWitnessSPKV0.fromAsm(asm)
      case _ =>
        sys.error(
          s"The given asm was not a valid WitnessScriptPubKeyV0, got asm=$asm")
    }
}

/** Represents the pay-to-witness-pubkeyhash script pubkey type as defined in
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#P2WPKH BIP141]]
  */
sealed abstract class P2WPKHWitnessSPKV0 extends WitnessScriptPubKeyV0 {
  def pubKeyHash: Sha256Hash160Digest = Sha256Hash160Digest(asm(2).bytes)
  override def toString = s"${DescriptorType.WPKH.toString}(${pubKeyHash.hex})"
}

object P2WPKHWitnessSPKV0 extends ScriptFactory[P2WPKHWitnessSPKV0] {

  private case class P2WPKHWitnessSPKV0Impl(
      override val asm: Vector[ScriptToken])
      extends P2WPKHWitnessSPKV0 {
    override val scriptType: ScriptType = ScriptType.WITNESS_V0_KEYHASH
  }

  override def fromAsm(asm: Seq[ScriptToken]): P2WPKHWitnessSPKV0 = {
    buildScript(asm.toVector,
                P2WPKHWitnessSPKV0Impl.apply,
                s"Given asm was not a P2WPKHWitnessSPKV0, got $asm")
  }

  def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    val asmBytes = BytesUtil.toByteVector(asm)
    asmBytes.size == 22 && WitnessScriptPubKeyV0.isValidAsm(asm)
  }

  def fromHash(hash: Sha256Hash160Digest): P2WPKHWitnessSPKV0 = {
    val pushop = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    fromAsm(Seq(OP_0) ++ pushop ++ Seq(ScriptConstant(hash.bytes)))
  }

  /** Builds a P2WPKH SPK from raw ECPublicKeyBytes (unsafe). */
  private[core] def apply(pubKey: ECPublicKeyBytes): P2WPKHWitnessSPKV0 = {
    // https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#restrictions-on-public-key-type
    require(
      pubKey.isCompressed,
      s"Public key must be compressed to be used in a segwit script, see BIP143")
    P2WPKHWitnessSPKV0(pubKey.toPublicKey)
  }

  /** Creates a P2WPKH witness script pubkey */
  def apply(pubKey: ECPublicKey): P2WPKHWitnessSPKV0 = {
    require(
      pubKey.isCompressed,
      s"Public key must be compressed to be used in a segwit script, see BIP143")
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    val pushop = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    fromAsm(Seq(OP_0) ++ pushop ++ Seq(ScriptConstant(hash.bytes)))
  }
}

/** Reprents the pay-to-witness-scripthash script pubkey type as defined in
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#p2wsh BIP141]]
  */
sealed abstract class P2WSHWitnessSPKV0 extends WitnessScriptPubKeyV0 {
  def scriptHash: Sha256Digest = Sha256Digest(asm(2).bytes)
  override def toString = s"${DescriptorType.WSH.toString}(${scriptHash.hex})"
}

object P2WSHWitnessSPKV0 extends ScriptFactory[P2WSHWitnessSPKV0] {

  private case class P2WSHWitnessSPKV0Impl(
      override val asm: Vector[ScriptToken])
      extends P2WSHWitnessSPKV0 {
    override val scriptType: ScriptType = ScriptType.WITNESS_V0_SCRIPTHASH
  }

  override def fromAsm(asm: Seq[ScriptToken]): P2WSHWitnessSPKV0 = {
    buildScript(asm.toVector,
                P2WSHWitnessSPKV0Impl.apply,
                s"Given asm was not a P2WSHWitnessSPKV0, got $asm")
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    val asmBytes = BytesUtil.toByteVector(asm)
    WitnessScriptPubKeyV0.isValidAsm(asm) &&
    asmBytes.size == 34
  }

  def fromHash(hash: Sha256Digest): P2WSHWitnessSPKV0 = {
    val pushop = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    fromAsm(Seq(OP_0) ++ pushop ++ Seq(ScriptConstant(hash.bytes)))
  }

  def apply(spk: RawScriptPubKey): P2WSHWitnessSPKV0 = {
    require(
      BitcoinScriptUtil.isOnlyCompressedPubKey(spk),
      s"Public key must be compressed to be used in a segwit script, see BIP143")
    val hash = CryptoUtil.sha256(spk.asmBytes)
    val pushop = BitcoinScriptUtil.calculatePushOp(hash.bytes)
    fromAsm(Seq(OP_0) ++ pushop ++ Seq(ScriptConstant(hash.bytes)))
  }
}

case class TaprootScriptPubKey(override val asm: Vector[ScriptToken])
    extends WitnessScriptPubKey {
  require(
    witnessVersion == WitnessVersion1,
    s"Taproot scriptpubkeys must have witnessVersion OP_1, got=$witnessVersion")
  require(bytes.length == 35,
          s"Taproot spks must have length 35, got=${bytes.length}")
  override def witnessProgram: Seq[ScriptToken] = asm.tail.tail
  override val scriptType: ScriptType = ScriptType.WITNESS_V1_TAPROOT

  val pubKey: XOnlyPubKey = {
    require(asm(2).bytes.length == 32,
            s"pubKeyBytes must be 32 bytes in length, got=${asm(2).byteSize}")
    XOnlyPubKey.fromBytes(asm(2).bytes)
  }

  override def toString = s"${DescriptorType.TR.toString}(${pubKey.hex})"
}

object TaprootScriptPubKey extends ScriptFactory[TaprootScriptPubKey] {

  override def fromAsm(asm: Seq[ScriptToken]): TaprootScriptPubKey = {
    buildScript(asm.toVector,
                TaprootScriptPubKey.apply,
                s"Given asm was not a TaprootScriptPubKey, got $asm")
  }

  def apply(xOnlyPubKey: XOnlyPubKey): TaprootScriptPubKey = {
    fromPubKey(xOnlyPubKey)
  }

  def fromPubKey(xOnlyPubKey: XOnlyPubKey): TaprootScriptPubKey = {
    val pushOp = BitcoinScriptUtil.calculatePushOp(xOnlyPubKey.bytes)
    val asm = OP_1 +: (pushOp ++ Vector(ScriptConstant(xOnlyPubKey.bytes)))
    fromAsm(asm)
  }

  def fromPubKey(schnorrPublicKey: SchnorrPublicKey): TaprootScriptPubKey = {
    fromPubKey(schnorrPublicKey.toXOnly)
  }

  def fromInternalKeyTapscriptTree(
      internal: XOnlyPubKey,
      tree: TapscriptTree): (KeyParity, TaprootScriptPubKey) = {
    val merkleRoot = TaprootScriptPath.computeFullTreeMerkleRoot(tree)
    val (parity, tweak) = internal.createTapTweak(Some(merkleRoot))
    (parity, fromPubKey(tweak))
  }

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    val asmBytes = BytesUtil.toByteVector(asm)
    asm.length == 3 &&
    asm.headOption.contains(OP_1) &&
    WitnessScriptPubKey.isValidAsm(asm) &&
    asmBytes.size == 34 &&
    // have to make sure we have a valid xonly pubkey, not just 32 bytes
    XOnlyPubKey.fromBytesT(asm(2).bytes).isSuccess
  }

  /** Computes a [[TaprootScriptPubKey]] from a given internal key with no
    * [[TaprootScriptPath]] If the spending conditions do not require a script
    * path, the output key should commit to an unspendable script path instead
    * of having no script path. This can be achieved by computing the output key
    * point as Q = P + int(hashTapTweak(bytes(P)))G.
    * @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#constructing-and-spending-taproot-outputs]]
    */
  def fromInternalKey(xOnlyPubKey: XOnlyPubKey): TaprootScriptPubKey = {
    val hash = CryptoUtil.tapTweakHash(xOnlyPubKey.bytes)
    val pubKey = CryptoParams.getG
      .multiply(FieldElement(hash.bytes))
      .add(xOnlyPubKey.publicKey)
      .toXOnly
    TaprootScriptPubKey(pubKey)
  }

  def computeMerkleRoot(controlBlock: ControlBlock): Sha256Digest = {
    computeMerkleRootHelper(controlBlock.hashes)
  }

  @tailrec
  private def computeMerkleRootHelper(
      hashes: Vector[Sha256Digest]): Sha256Digest = {
    require(hashes.length > 0,
            s"Cannot computeMerkleRoot() with 0 hashes=$hashes")
    if (hashes.length > 1) {
      val result: Vector[Sha256Digest] = hashes
        .grouped(2)
        .map { grouped =>
          // need to recurse?
          CryptoUtil.tapBranchHash(grouped(0).bytes ++ grouped(1).bytes)
        }
        .toVector
      computeMerkleRootHelper(result)
    } else {
      hashes.head
    }
  }
}

/** Type to represent all
  * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]s
  * we have not used yet in the bitcoin protocol
  */
sealed trait UnassignedWitnessScriptPubKey extends WitnessScriptPubKey {
  override def witnessProgram: Seq[ScriptToken] = asm.tail.tail
}

object UnassignedWitnessScriptPubKey
    extends ScriptFactory[UnassignedWitnessScriptPubKey] {

  private case class UnassignedWitnessScriptPubKeyImpl(
      override val asm: Vector[ScriptToken])
      extends UnassignedWitnessScriptPubKey {
    override val scriptType: ScriptType = ScriptType.WITNESS_UNKNOWN

    override def toString = s"UnassignedWitnessScriptPubKey($asm)"
  }

  override def fromAsm(asm: Seq[ScriptToken]): UnassignedWitnessScriptPubKey = {
    buildScript(
      asm.toVector,
      UnassignedWitnessScriptPubKeyImpl.apply,
      "Given asm was not a valid witness script pubkey: " + asm
    )
  }
  def apply(asm: Seq[ScriptToken]): UnassignedWitnessScriptPubKey = fromAsm(asm)

  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    WitnessScriptPubKey.isValidAsm(asm)
  }
}

/** This trait represents the witness commitment found in the coinbase
  * transaction This is needed to commit to the wtxids of all of the witness
  * transactions, since the merkle tree does not commit to the witnesses for all
  * [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]
  * See BIP141 for more info
  * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#commitment-structure]]
  */
sealed trait WitnessCommitment extends RawScriptPubKey {

  /** The commitment to the
    * [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]s
    * in the [[org.bitcoins.core.protocol.blockchain.Block Block]]
    */
  def witnessRootHash: DoubleSha256Digest =
    DoubleSha256Digest(asm(2).bytes.splitAt(4)._2)
}

object WitnessCommitment extends ScriptFactory[WitnessCommitment] {

  private case class WitnessCommitmentImpl(
      override val asm: Vector[ScriptToken])
      extends WitnessCommitment {
    override val scriptType: ScriptType = ScriptType.WITNESS_COMMITMENT

    override def toString = s"WitnessCommitment(${witnessRootHash.hex})"
  }

  /** Every witness commitment must start with this header, see
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki BIP141]]
    * for details
    */
  private val commitmentHeader = "aa21a9ed"

  def apply(asm: Seq[ScriptToken]): WitnessCommitment = fromAsm(asm)

  override def fromAsm(asm: Seq[ScriptToken]): WitnessCommitment = {
    buildScript(asm.toVector,
                WitnessCommitmentImpl.apply,
                "Given asm was not a valid witness commitment, got: " + asm)
  }

  def apply(hash: DoubleSha256Digest): WitnessCommitment = {
    WitnessCommitment(
      Seq(OP_RETURN,
          BytesToPushOntoStack(36),
          ScriptConstant(commitmentHeader + hash.hex)))
  }

  /** This determines if the given asm has the correct witness structure
    * according to
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#commitment-structure BIP141]]
    */
  override def isValidAsm(asm: Seq[ScriptToken]): Boolean = {
    if (asm.size < 3) false
    else {
      val minCommitmentSize = 38
      val asmBytes = BytesUtil.toByteVector(asm)
      val (opReturn, pushOp, constant) = (asm.head, asm(1), asm(2))
      opReturn == OP_RETURN && pushOp == BytesToPushOntoStack(36) &&
      constant.hex.take(
        8) == commitmentHeader && asmBytes.size >= minCommitmentSize
    }
  }
}
