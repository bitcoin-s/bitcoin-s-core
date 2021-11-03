package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted._
import org.bitcoins.crypto._

/** Specifies the set of oracles and their corresponding announcements
  * and parameters to be used in a DLC.
  */
sealed trait OracleInfo extends DLCSpecTypeSerializable[OracleInfoTLV] {

  /** The number of oracles required for execution */
  def threshold: Int

  /** The total number of oracles to choose from */
  def numOracles: Int

  /** Each oracle's corresponding SingleOracleInfo */
  def singleOracleInfos: Vector[SingleOracleInfo]
}

/** Specifies a set of oracles for an Enumerated Outcome DLC */
sealed trait EnumOracleInfo extends OracleInfo {
  override def singleOracleInfos: Vector[EnumSingleOracleInfo]
}

/** Specifies a set of oracles for an Numeric Outcome DLC */
sealed trait NumericOracleInfo extends OracleInfo {
  override def singleOracleInfos: Vector[NumericSingleOracleInfo]
}

object OracleInfo
    extends DLCSpecTypeDeserializable[OracleInfoTLV, OracleInfo](
      OracleInfoTLV) {

  override def fromSubType(tlv: OracleInfoTLV): OracleInfo = {
    tlv match {
      case tlv: OracleInfoV0TLV => SingleOracleInfo.fromSubType(tlv)
      case tlv: OracleInfoV1TLV =>
        tlv.oracleParamsOpt match {
          case SomeTLV(_) =>
            NumericMultiOracleInfo.fromSubType(tlv)
          case NoneTLV =>
            ExactMultiOracleInfo.fromSubType(tlv)
        }

    }
  }

  def getOracleParamsOpt(oracleInfo: OracleInfo): Option[OracleParamsV0TLV] = {
    oracleInfo match {
      case _: SingleOracleInfo | _: NumericExactMultiOracleInfo |
          _: EnumMultiOracleInfo =>
        None
      case numeric: NumericMultiOracleInfo =>
        numeric.oracleParamsOpt.map { case v0: OracleParamsV0TLV =>
          v0
        }.toOption
    }
  }
}

/** Specifies a single oracles' information through an announcement */
sealed trait SingleOracleInfo
    extends OracleInfo
    with DLCSpecTypeSerializable[OracleInfoV0TLV] {
  override val numOracles: Int = 1
  override val threshold: Int = 1

  override def singleOracleInfos: Vector[this.type] = Vector(this)

  def announcement: OracleAnnouncementTLV

  /** The oracle's public key */
  def publicKey: SchnorrPublicKey = announcement.publicKey

  /** The oracle's pre-committed nonces, in the correct order */
  def nonces: OrderedNonces = announcement.eventTLV.nonces

  /** The order of the given sigs should correspond to the given outcome. */
  def verifySigs(outcome: DLCOutcomeType, sigs: OracleSignatures): Boolean

  /** Computes the signature point (aka signature anticipation) for a given outcome.
    * This point is used for adaptor signing.
    */
  def sigPoint(outcome: DLCOutcomeType): ECPublicKey = {
    publicKey.computeSigPoint(outcome.serialized, nonces.vec)
  }

  /** Computes the sum of all nonces used in a given outcome */
  def aggregateNonce(outcome: DLCOutcomeType): SchnorrNonce = {
    nonces
      .take(outcome.serialized.length)
      .map(_.publicKey)
      .reduce(_.add(_))
      .schnorrNonce
  }

  override def toSubType: OracleInfoV0TLV =
    OracleInfoV0TLV(announcement, DLCSerializationVersion.current)
}

object SingleOracleInfo
    extends DLCSpecTypeDeserializable[OracleInfoV0TLV, SingleOracleInfo](
      OracleInfoV0TLV) {

  def apply(announcement: OracleAnnouncementTLV): SingleOracleInfo = {
    announcement.eventTLV.eventDescriptor match {
      case _: EnumEventDescriptorV0TLV =>
        EnumSingleOracleInfo(announcement)
      case _: NumericEventDescriptorTLV =>
        NumericSingleOracleInfo(announcement)
    }
  }

  def apply(tlv: OracleInfoV0TLV): SingleOracleInfo = {
    SingleOracleInfo(tlv.announcement)
  }

  override def fromSubType(tlv: OracleInfoV0TLV): SingleOracleInfo = {
    SingleOracleInfo(tlv)
  }
}

/** Specifies a single oracles' information for an Enumerated Outcome DLC
  * through an announcement
  */
case class EnumSingleOracleInfo(announcement: OracleAnnouncementTLV)
    extends SingleOracleInfo
    with EnumOracleInfo {
  require(announcement.eventTLV.eventDescriptor
            .isInstanceOf[EnumEventDescriptorV0TLV],
          s"Enum OracleInfo requires EnumEventDescriptor, $announcement")

  val nonce: SchnorrNonce = announcement.eventTLV.nonces.head

  /** @inheritdoc */
  override def verifySigs(
      outcome: DLCOutcomeType,
      sigs: OracleSignatures): Boolean = {
    outcome match {
      case EnumOutcome(outcome) =>
        sigs match {
          case _: NumericOracleSignatures =>
            throw new IllegalArgumentException(
              s"Expected one signature, got $sigs")
          case EnumOracleSignature(_, sig) =>
            if (sig.rx != nonce) {
              throw new IllegalArgumentException(
                s"Expected R value of $nonce, got $sig")
            } else {
              publicKey.verify(CryptoUtil.sha256DLCAttestation(outcome).bytes,
                               sig)
            }
        }
      case UnsignedNumericOutcome(_) | _: SignedNumericOutcome =>
        throw new IllegalArgumentException(
          s"Expected EnumOutcome, got $outcome")
    }
  }
}

object EnumSingleOracleInfo
    extends DLCSpecTypeDeserializable[OracleInfoV0TLV, EnumSingleOracleInfo](
      OracleInfoV0TLV) {

  def dummyForKeys(
      privKey: ECPrivateKey,
      nonce: SchnorrNonce,
      events: Vector[EnumOutcome]): EnumSingleOracleInfo = {
    EnumSingleOracleInfo(
      OracleAnnouncementV0TLV
        .dummyForEventsAndKeys(privKey, nonce, events))
  }

  override def fromSubType(tlv: OracleInfoV0TLV): EnumSingleOracleInfo = {
    EnumSingleOracleInfo(tlv.announcement)
  }
}

/** Specifies a single oracles' information for an Numeric Outcome DLC
  * through an announcement
  */
case class NumericSingleOracleInfo(announcement: OracleAnnouncementTLV)
    extends SingleOracleInfo
    with NumericOracleInfo {
  require(announcement.eventTLV.eventDescriptor
            .isInstanceOf[NumericEventDescriptorTLV],
          s"Numeric OracleInfo requires NumericEventDescriptor, $announcement")

  /** @inheritdoc */
  override def verifySigs(
      outcome: DLCOutcomeType,
      sigs: OracleSignatures): Boolean = {
    require(sigs.nonEmpty, "At least one signature is required")
    require(
      sigs.length <= nonces.length,
      s"Too many signatures (expected at most ${nonces.length}), got $sigs")

    outcome match {
      case EnumOutcome(_) | _: SignedNumericOutcome =>
        throw new IllegalArgumentException(
          s"Expected numeric outcome, got $outcome")
      case UnsignedNumericOutcome(digits) =>
        digits
          .zip(sigs.take(digits.length).zip(nonces.take(digits.length)))
          .foldLeft(digits.length <= sigs.length) {
            case (result, (digit, (sig, nonce))) =>
              require(sig.rx == nonce,
                      s"Unexpected nonce in ${sig.hex}, expected ${nonce.hex}")

              result && publicKey.verify(
                CryptoUtil.sha256DLCAttestation(digit.toString).bytes,
                sig)
          }
    }
  }
}

object NumericSingleOracleInfo {

  def dummyForKeys(
      privKey: ECPrivateKey,
      nonces: Vector[SchnorrNonce]): NumericSingleOracleInfo = {
    NumericSingleOracleInfo(
      OracleAnnouncementV0TLV.dummyForKeys(privKey, nonces))
  }
}

/** Represents the oracle information for more than one oracle through
  * multiple announcements.
  */
sealed trait MultiOracleInfo[+T <: SingleOracleInfo]
    extends OracleInfo
    with DLCSpecTypeSerializable[MultiOracleInfoTLV] {
  override def numOracles: Int = announcements.length

  def announcements: OrderedAnnouncements

  require(
    announcements.length >= threshold,
    s"Cannot have threshold ($threshold) above the number of oracles (${announcements.length})")

  // Override this with a val to invoke requirements
  def singleOracleInfos: Vector[T]
}

/** Represents the oracle information for more than one oracle where
  * all oracles sign exactly corresponding messages.
  */
sealed trait ExactMultiOracleInfo[+T <: SingleOracleInfo]
    extends MultiOracleInfo[T]
    with DLCSpecTypeSerializable[OracleInfoV1TLV] {

  override def toSubType: OracleInfoV1TLV =
    OracleInfoV1TLV(threshold,
                    announcements,
                    NoneTLV,
                    DLCSerializationVersion.current)
}

object ExactMultiOracleInfo
    extends DLCSpecTypeDeserializable[
      OracleInfoV1TLV,
      ExactMultiOracleInfo[SingleOracleInfo]](OracleInfoV1TLV) {

  def apply(tlv: OracleInfoV1TLV): ExactMultiOracleInfo[SingleOracleInfo] = {
    tlv.oracles.head.eventTLV.eventDescriptor match {
      case _: EnumEventDescriptorV0TLV =>
        EnumMultiOracleInfo(tlv.threshold, tlv.oracles)
      case _: NumericEventDescriptorTLV =>
        NumericExactMultiOracleInfo(tlv.threshold, tlv.oracles)
    }
  }

  override def fromSubType(
      tlv: OracleInfoV1TLV): ExactMultiOracleInfo[SingleOracleInfo] = {
    ExactMultiOracleInfo(tlv)
  }
}

/** Represents the oracle information for more than one oracle where
  * all oracles sign exactly corresponding messages from isomorphic Enums.
  */
case class EnumMultiOracleInfo(
    threshold: Int,
    announcements: OrderedAnnouncements)
    extends ExactMultiOracleInfo[EnumSingleOracleInfo]
    with EnumOracleInfo {

  override val singleOracleInfos: Vector[EnumSingleOracleInfo] =
    announcements.toVector.map(EnumSingleOracleInfo.apply)
}

/** Represents the oracle information for more than one oracle where
  * all oracles sign exactly equal numeric outcomes.
  */
case class NumericExactMultiOracleInfo(
    threshold: Int,
    announcements: OrderedAnnouncements)
    extends ExactMultiOracleInfo[NumericSingleOracleInfo]
    with NumericOracleInfo {

  val singleOracleInfos: Vector[NumericSingleOracleInfo] =
    announcements.toVector.map(NumericSingleOracleInfo.apply)
}

/** Represents the oracle information and parameters for more than
  * one oracle where the oracles may be signing slightly different numeric outcomes.
  */
case class NumericMultiOracleInfo(
    threshold: Int,
    announcements: OrderedAnnouncements,
    oracleParamsOpt: OptionTLV[OracleParamsTLV])
    extends MultiOracleInfo[NumericSingleOracleInfo]
    with DLCSpecTypeSerializable[OracleInfoV1TLV]
    with NumericOracleInfo {

  override val singleOracleInfos: Vector[NumericSingleOracleInfo] =
    announcements.toVector.map(NumericSingleOracleInfo.apply)

  override def toSubType: OracleInfoV1TLV = {
    oracleParamsOpt match {
      case SomeTLV(params) =>
        params match {
          case v0: OracleParamsV0TLV =>
            OracleInfoV1TLV(threshold,
                            announcements,
                            SomeTLV(v0),
                            DLCSerializationVersion.current)
        }
      case NoneTLV =>
        OracleInfoV1TLV(threshold,
                        announcements,
                        NoneTLV,
                        DLCSerializationVersion.current)
    }
  }
}

object NumericMultiOracleInfo
    extends DLCSpecTypeDeserializable[OracleInfoV1TLV, NumericMultiOracleInfo](
      OracleInfoV1TLV) {

  def apply(
      threshold: Int,
      announcements: OrderedAnnouncements,
      paramsOpt: Option[OracleParamsTLV]): NumericMultiOracleInfo = {
    new NumericMultiOracleInfo(threshold, announcements, OptionTLV(paramsOpt))
  }

  override def fromSubType(tlv: OracleInfoV1TLV): NumericMultiOracleInfo = {
    NumericMultiOracleInfo(tlv.threshold, tlv.oracles, tlv.oracleParamsOpt)
  }
}