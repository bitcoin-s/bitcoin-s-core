package org.bitcoins.core.psbt

import org.bitcoins.crypto.Factory
import scodec.bits.ByteVector

/** A PSBTKeyId refers to the first byte of a key that signifies which kind of key-value map
  * is in a given PSBTRecord
  */
sealed trait PSBTKeyId {
  def byte: Byte
  type RecordType <: PSBTRecord
}

sealed trait PSBTKeyIdFactory[KeyIdType <: PSBTKeyId]
    extends Factory[KeyIdType] {
  def fromByte(byte: Byte): KeyIdType
  def unknownKey: KeyIdType

  override def fromBytes(key: ByteVector): KeyIdType = {
    if (key.isEmpty) {
      unknownKey
    } else {
      fromByte(key.head)
    }
  }
}

sealed trait PSBTGlobalKeyId extends PSBTKeyId {
  type RecordType <: GlobalPSBTRecord
}

object PSBTGlobalKeyId extends PSBTKeyIdFactory[PSBTGlobalKeyId] {

  override def fromByte(byte: Byte): PSBTGlobalKeyId =
    byte match {
      case UnsignedTransactionKeyId.byte => UnsignedTransactionKeyId
      case XPubKeyKeyId.byte             => XPubKeyKeyId
      case VersionKeyId.byte             => VersionKeyId
      case _: Byte                       => UnknownKeyId
    }

  override def unknownKey: PSBTGlobalKeyId = UnknownKeyId

  case object UnsignedTransactionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = GlobalPSBTRecord.UnsignedTransaction
  }

  case object XPubKeyKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = GlobalPSBTRecord.XPubKey
  }

  case object VersionKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = 0xfb.byteValue
    type RecordType = GlobalPSBTRecord.Version
  }

  case object UnknownKeyId extends PSBTGlobalKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = GlobalPSBTRecord.Unknown
  }
}

sealed trait PSBTInputKeyId extends PSBTKeyId {
  type RecordType <: InputPSBTRecord
}

object PSBTInputKeyId extends PSBTKeyIdFactory[PSBTInputKeyId] {

  override def unknownKey: PSBTInputKeyId = UnknownKeyId

  override def fromByte(byte: Byte): PSBTInputKeyId =
    byte match {
      case NonWitnessUTXOKeyId.byte            => NonWitnessUTXOKeyId
      case WitnessUTXOKeyId.byte               => WitnessUTXOKeyId
      case PartialSignatureKeyId.byte          => PartialSignatureKeyId
      case SigHashTypeKeyId.byte               => SigHashTypeKeyId
      case RedeemScriptKeyId.byte              => RedeemScriptKeyId
      case WitnessScriptKeyId.byte             => WitnessScriptKeyId
      case BIP32DerivationPathKeyId.byte       => BIP32DerivationPathKeyId
      case FinalizedScriptSigKeyId.byte        => FinalizedScriptSigKeyId
      case FinalizedScriptWitnessKeyId.byte    => FinalizedScriptWitnessKeyId
      case ProofOfReservesCommitmentKeyId.byte => ProofOfReservesCommitmentKeyId
      case RIPEMD160PreImageKeyId.byte         => RIPEMD160PreImageKeyId
      case SHA256PreImageKeyId.byte            => SHA256PreImageKeyId
      case HASH160PreImageKeyId.byte           => HASH160PreImageKeyId
      case HASH256PreImageKeyId.byte           => HASH256PreImageKeyId
      case TRKeySpendSignatureKeyId.byte       => TRKeySpendSignatureKeyId
      case TRScriptSpendSignatureKeyId.byte    => TRScriptSpendSignatureKeyId
      case TRLeafScriptKeyId.byte              => TRLeafScriptKeyId
      case TRBIP32DerivationPathKeyId.byte     => TRBIP32DerivationPathKeyId
      case TRInternalKeyKeyId.byte             => TRInternalKeyKeyId
      case TRMerkelRootKeyId.byte              => TRMerkelRootKeyId
      case _: Byte                             => UnknownKeyId

    }

  case object NonWitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = InputPSBTRecord.NonWitnessOrUnknownUTXO
  }

  case object WitnessUTXOKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = InputPSBTRecord.WitnessUTXO
  }

  case object PartialSignatureKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x02.byteValue
    type RecordType = InputPSBTRecord.PartialSignature
  }

  case object SigHashTypeKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x03.byteValue
    type RecordType = InputPSBTRecord.SigHashType
  }

  case object RedeemScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x04.byteValue
    type RecordType = InputPSBTRecord.RedeemScript
  }

  case object WitnessScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x05.byteValue
    type RecordType = InputPSBTRecord.WitnessScript
  }

  case object BIP32DerivationPathKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x06.byteValue
    type RecordType = InputPSBTRecord.BIP32DerivationPath
  }

  case object FinalizedScriptSigKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x07.byteValue
    type RecordType = InputPSBTRecord.FinalizedScriptSig
  }

  case object FinalizedScriptWitnessKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x08.byteValue
    type RecordType = InputPSBTRecord.FinalizedScriptWitness
  }

  case object ProofOfReservesCommitmentKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x09.byteValue
    type RecordType = InputPSBTRecord.ProofOfReservesCommitment
  }

  case object RIPEMD160PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0a.byteValue
    type RecordType = InputPSBTRecord.RIPEMD160PreImage
  }

  case object SHA256PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0b.byteValue
    type RecordType = InputPSBTRecord.SHA256PreImage
  }

  case object HASH160PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0c.byteValue
    type RecordType = InputPSBTRecord.HASH160PreImage
  }

  case object HASH256PreImageKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x0d.byteValue
    type RecordType = InputPSBTRecord.HASH256PreImage
  }

  final case object TRKeySpendSignatureKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x13.byteValue
    type RecordType = InputPSBTRecord.TRKeySpendSignature
  }

  final case object TRScriptSpendSignatureKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x14.byteValue
    type RecordType = InputPSBTRecord.TRScriptSpendSignature
  }

  final case object TRLeafScriptKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x15.byteValue
    type RecordType = InputPSBTRecord.TRLeafScript
  }

  final case object TRBIP32DerivationPathKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x16.byteValue
    type RecordType = InputPSBTRecord.TRBIP32DerivationPath
  }

  final case object TRInternalKeyKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x17.byteValue
    type RecordType = InputPSBTRecord.TRInternalKey
  }

  final case object TRMerkelRootKeyId extends PSBTInputKeyId {
    override val byte: Byte = 0x18.byteValue
    type RecordType = InputPSBTRecord.TRMerkelRoot
  }

  case object UnknownKeyId extends PSBTInputKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = InputPSBTRecord.Unknown
  }
}

sealed trait PSBTOutputKeyId extends PSBTKeyId {
  type RecordType <: OutputPSBTRecord
}

object PSBTOutputKeyId extends PSBTKeyIdFactory[PSBTOutputKeyId] {

  override def unknownKey: PSBTOutputKeyId = UnknownKeyId

  override def fromByte(byte: Byte): PSBTOutputKeyId =
    byte match {
      case RedeemScriptKeyId.byte          => RedeemScriptKeyId
      case WitnessScriptKeyId.byte         => WitnessScriptKeyId
      case BIP32DerivationPathKeyId.byte   => BIP32DerivationPathKeyId
      case TRInternalKeyKeyId.byte         => TRInternalKeyKeyId
      case TaprootTreeKeyId.byte           => TaprootTreeKeyId
      case TRBIP32DerivationPathKeyId.byte => TRBIP32DerivationPathKeyId
      case _: Byte                         => UnknownKeyId
    }

  case object RedeemScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x00.byteValue
    type RecordType = OutputPSBTRecord.RedeemScript
  }

  case object WitnessScriptKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x01.byteValue
    type RecordType = OutputPSBTRecord.WitnessScript
  }

  case object BIP32DerivationPathKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x02.byteValue
    type RecordType = OutputPSBTRecord.BIP32DerivationPath
  }

  final case object TRInternalKeyKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x05.byteValue
    type RecordType = OutputPSBTRecord.TRInternalKey
  }

  final case object TaprootTreeKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x06.byteValue
    type RecordType = OutputPSBTRecord.TRInternalKey
  }

  final case object TRBIP32DerivationPathKeyId extends PSBTOutputKeyId {
    override val byte: Byte = 0x07.byteValue
    type RecordType = OutputPSBTRecord.TRBIP32DerivationPath
  }

  case object UnknownKeyId extends PSBTOutputKeyId {
    override val byte: Byte = Byte.MaxValue
    type RecordType = OutputPSBTRecord.Unknown
  }
}
