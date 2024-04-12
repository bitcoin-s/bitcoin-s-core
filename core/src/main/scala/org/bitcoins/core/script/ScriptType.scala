package org.bitcoins.core.script

import org.bitcoins.crypto.StringFactory

/** The different Bitcoin Script type variations
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/fa6180188b8ab89af97860e6497716405a48bab6/src/script/standard.h#L56 standard.h]]
  *     and [[https://github.com/bitcoin/bitcoin/blob/03732f8644a449af34f4df1bb3b8915fb15ef22c/src/script/standard.cpp#L27 standarc.cpp]]
  *     from Bitcoin Core
  */
sealed abstract class ScriptType {
  import org.bitcoins.core.script.ScriptType._

  override def toString: String =
    this match {
      case NONSTANDARD                => "nonstandard"
      case PUBKEY                     => "pubkey"
      case PUBKEYHASH                 => "pubkeyhash"
      case SCRIPTHASH                 => "scripthash"
      case MULTISIG                   => "multisig"
      case CLTV                       => "cltv"
      case CSV                        => "csv"
      case NONSTANDARD_IF_CONDITIONAL => "nonstandard_if_conditional"
      case NOT_IF_CONDITIONAL         => "not_if_conditional"
      case MULTISIG_WITH_TIMEOUT      => "multisig_with_timeout"
      case PUBKEY_WITH_TIMEOUT        => "pubkey_with_timeout"
      case NULLDATA                   => "nulldata"
      case WITNESS_V0_KEYHASH         => "witness_v0_keyhash"
      case WITNESS_V0_SCRIPTHASH      => "witness_v0_scripthash"
      case WITNESS_V1_TAPROOT         => "witness_v1_taproot"
      case WITNESS_UNKNOWN            => "witness_unknown"
      case WITNESS_COMMITMENT         => "witness_commitment"
    }
}

/** The different Bitcoin Script type variations
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/fa6180188b8ab89af97860e6497716405a48bab6/src/script/standard.h#L56 standard.h]]
  *     and [[https://github.com/bitcoin/bitcoin/blob/03732f8644a449af34f4df1bb3b8915fb15ef22c/src/script/standard.cpp#L27 standarc.cpp]]
  *     from Bitcoin Core
  */
object ScriptType extends StringFactory[ScriptType] {

  private[script] val all: Seq[ScriptType] = Vector(
    NONSTANDARD,
    PUBKEY,
    PUBKEYHASH,
    SCRIPTHASH,
    MULTISIG,
    NULLDATA,
    WITNESS_V0_KEYHASH,
    WITNESS_V0_SCRIPTHASH,
    WITNESS_V1_TAPROOT,
    WITNESS_UNKNOWN,
    CLTV,
    CSV,
    NONSTANDARD_IF_CONDITIONAL,
    NOT_IF_CONDITIONAL,
    MULTISIG_WITH_TIMEOUT,
    PUBKEY_WITH_TIMEOUT,
    WITNESS_COMMITMENT
  )

  override def fromStringOpt(string: String): Option[ScriptType] =
    all.find(_.toString == string)

  /** Throws if given string is invalid */
  override def fromString(string: String): ScriptType =
    fromStringOpt(string) match {
      case Some(scriptType) => scriptType
      case None             => sys.error(s"Could not find scriptType=${string}")
    }

  case object NONSTANDARD extends ScriptType

  // ╔ "standard" transaction/script types
  // V
  case object PUBKEY extends ScriptType
  case object PUBKEYHASH extends ScriptType
  case object SCRIPTHASH extends ScriptType
  case object MULTISIG extends ScriptType

  case object CLTV extends ScriptType
  case object CSV extends ScriptType
  case object NONSTANDARD_IF_CONDITIONAL extends ScriptType
  case object NOT_IF_CONDITIONAL extends ScriptType
  case object MULTISIG_WITH_TIMEOUT extends ScriptType

  case object PUBKEY_WITH_TIMEOUT extends ScriptType

  /** unspendable OP_RETURN script that carries data */
  case object NULLDATA extends ScriptType
  case object WITNESS_V0_KEYHASH extends ScriptType
  case object WITNESS_V0_SCRIPTHASH extends ScriptType
  case object WITNESS_V1_TAPROOT extends ScriptType

  /** Only for Witness versions not already defined */
  case object WITNESS_UNKNOWN extends ScriptType

  case object WITNESS_COMMITMENT extends ScriptType
}
