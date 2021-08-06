package org.bitcoins.core.api.dlcoracle

import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.core.util.sorted.{OrderedNonces, OrderedSchnorrSignatures}
import org.bitcoins.crypto._

import java.time.Instant

/** Represents an event that the oracle has committed to
  * Contains all the necessary information to construct
  * all the oracle TLV messages
  */
sealed trait OracleEvent {

  /** The nonces the oracle is committing to for this event */
  def nonces: OrderedNonces

  /** The oracle's public key */
  def pubkey: SchnorrPublicKey

  /** The name given to this event, may be a URI */
  def eventName: String

  /** The version of signing for this event */
  def signingVersion: SigningVersion

  /** The earliest expected time an outcome will be signed */
  def maturationTime: Instant

  /** A signature by the oracle of the hash of nonce and event name */
  def announcementSignature: SchnorrDigitalSignature

  def eventDescriptorTLV: EventDescriptorTLV

  def eventTLV: OracleEventTLV = {
    require(eventDbsOpt.isDefined,
            s"Event dbs must be defined to figure out ordering of nonces")
    val v0NonceOrder = eventDbsOpt.get.sortBy(_.nonceIndex).map(_.nonce)
    OracleEventV0TLV(v0NonceOrder,
                     UInt32(maturationTime.getEpochSecond),
                     eventDescriptorTLV,
                     eventName)
  }

  def announcementTLV: OracleAnnouncementTLV = {
    eventTLV match {
      case v0TLV: OracleEventV0TLV =>
        OracleAnnouncementV0TLV(announcementSignature, pubkey, v0TLV)
    }
  }

  /** These are needed for old announcements/attesatations that do not follow the requirement
    * to order nonces
    */
  protected def eventDbsOpt: Option[Vector[EventDb]]
}

/** An oracle event that has not been signed yet */
sealed trait PendingOracleEvent extends OracleEvent

/** An oracle event that has been signed */
sealed trait CompletedOracleEvent extends OracleEvent {
  def attestations: Vector[FieldElement]

  require(attestations.size == nonces.size,
          "Must have a signature for every nonce")

  def signatures: OrderedSchnorrSignatures = {
    val unsorted = nonces.toVector
      .zip(attestations)
      .map(sigPieces => SchnorrDigitalSignature(sigPieces._1, sigPieces._2))
    OrderedSchnorrSignatures.fromUnsorted(unsorted)
  }

  def oracleAttestmentV0TLV: OracleAttestmentV0TLV = {

    announcementTLV match {
      case ann: OracleAnnouncementV0TLV =>
        //v0 announcements do not have a invariant stating that nonces neeed to be sorted
        //a specific way, so we need to use the unsorted variant to make sure
        //announcementSignatures evaluate to true
        val unsorted = ann.eventTLV.nonces
          .zip(attestations)
          .map(sigPieces => SchnorrDigitalSignature(sigPieces._1, sigPieces._2))
        OracleAttestmentV0TLV(eventName,
                              pubkey,
                              unsorted,
                              outcomes.map(_.outcomeString))
    }
  }

  def outcomes: Vector[DLCAttestationType]

  def dlcOutcome: DLCOutcomeType
}

sealed trait EnumV0OracleEvent extends OracleEvent {
  override def eventDescriptorTLV: EnumEventDescriptorV0TLV
  def nonce: SchnorrNonce

  final override def nonces: OrderedNonces = OrderedNonces(nonce)
}

case class PendingEnumV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: EnumEventDescriptorV0TLV,
    eventDbsOpt: Option[Vector[EventDb]])
    extends PendingOracleEvent
    with EnumV0OracleEvent

case class CompletedEnumV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: EnumEventDescriptorV0TLV,
    outcome: EnumAttestation,
    attestation: FieldElement,
    eventDbsOpt: Option[Vector[EventDb]])
    extends CompletedOracleEvent
    with EnumV0OracleEvent {
  require(
    OracleEvent.verifyAttestations(announcementTLV,
                                   oracleAttestmentV0TLV,
                                   signingVersion),
    s"Signatures given are invalid, eventId=${announcementTLV.eventTLV.eventId}"
  )

  override def attestations: Vector[FieldElement] = Vector(attestation)

  override def outcomes: Vector[DLCAttestationType] = Vector(outcome)

  override def dlcOutcome: DLCOutcomeType = EnumOutcome(outcome.outcomeString)
}

sealed trait DigitDecompositionV0OracleEvent extends OracleEvent {
  override def eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV
}

case class PendingDigitDecompositionV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonces: OrderedNonces,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV,
    eventDbsOpt: Option[Vector[EventDb]])
    extends PendingOracleEvent
    with DigitDecompositionV0OracleEvent

case class CompletedDigitDecompositionV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonces: OrderedNonces,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV,
    dlcOutcome: NumericDLCOutcomeType,
    attestations: Vector[FieldElement],
    eventDbsOpt: Option[Vector[EventDb]])
    extends CompletedOracleEvent
    with DigitDecompositionV0OracleEvent {

  require(
    OracleEvent.verifyAttestations(announcementTLV,
                                   oracleAttestmentV0TLV,
                                   signingVersion),
    s"Signatures given are invalid for eventId=${announcementTLV.eventTLV.eventId}"
  )

  val outcomeBase10: Long = {
    val (digits, positive) = dlcOutcome match {
      case UnsignedNumericOutcome(digits) =>
        (digits, true)
      case SignedNumericOutcome(positive, digits) =>
        (digits, positive)
    }

    val base = eventDescriptorTLV.base.toInt
    val numDigits = eventDescriptorTLV.numDigits.toInt

    val num = NumberUtil.fromDigits(digits, base, numDigits)

    if (positive) num
    else num * -1
  }

  override def outcomes: Vector[DigitDecompositionAttestationType] =
    dlcOutcome match {
      case UnsignedNumericOutcome(digits) =>
        digits.map(DigitDecompositionAttestation.apply(_))
      case SignedNumericOutcome(positive, digits) =>
        val sign = DigitDecompositionSignAttestation(positive)
        sign +: digits.map(DigitDecompositionAttestation.apply(_))
    }
}

object OracleEvent {

  def fromEventDbs(eventDbs: Vector[EventDb]): OracleEvent = {
    val eventDb = eventDbs.head
    require(eventDbs.forall(_.eventDescriptorTLV == eventDb.eventDescriptorTLV),
            "EventDbs must all refer to the same event")

    (eventDb.eventDescriptorTLV, eventDb.attestationOpt) match {
      case (enumEvent: EnumEventDescriptorV0TLV, Some(sig)) =>
        require(eventDbs.size == 1, "Enum events may only have one eventDb")
        CompletedEnumV0OracleEvent(
          eventDb.pubkey,
          eventDb.nonce,
          eventDb.eventName,
          eventDb.signingVersion,
          eventDb.maturationTime,
          eventDb.announcementSignature,
          enumEvent,
          EnumAttestation(eventDb.outcomeOpt.get),
          sig,
          Some(eventDbs)
        )
      case (enumEvent: EnumEventDescriptorV0TLV, None) =>
        require(eventDbs.size == 1, "Enum events may only have one eventDb")
        PendingEnumV0OracleEvent(eventDb.pubkey,
                                 eventDb.nonce,
                                 eventDb.eventName,
                                 eventDb.signingVersion,
                                 eventDb.maturationTime,
                                 eventDb.announcementSignature,
                                 enumEvent,
                                 Some(eventDbs))
      case (decomp: DigitDecompositionEventDescriptorV0TLV, Some(_)) =>
        require(eventDbs.forall(_.attestationOpt.isDefined),
                "Cannot have a partially signed event")
        val sortedEventDbs = eventDbs.sortBy(_.nonceIndex)

        val attestations = sortedEventDbs.flatMap(_.attestationOpt)

        val dlcOutcome = decomp match {
          case _: SignedDigitDecompositionEventDescriptor =>
            val positive = sortedEventDbs.head.outcomeOpt.get == "+"
            val digits = sortedEventDbs.tail.map { eventDb =>
              eventDb.outcomeOpt.get.toInt
            }
            SignedNumericOutcome(positive, digits)
          case _: UnsignedDigitDecompositionEventDescriptor =>
            val digits = sortedEventDbs.map { eventDb =>
              eventDb.outcomeOpt.get.toInt
            }
            UnsignedNumericOutcome(digits)
        }
        CompletedDigitDecompositionV0OracleEvent(
          eventDb.pubkey,
          OrderedNonces.fromUnsorted(sortedEventDbs.map(_.nonce)),
          eventDb.eventName,
          eventDb.signingVersion,
          eventDb.maturationTime,
          eventDb.announcementSignature,
          decomp,
          dlcOutcome,
          attestations,
          Some(eventDbs)
        )
      case (decomp: DigitDecompositionEventDescriptorV0TLV, None) =>
        require(eventDbs.forall(_.attestationOpt.isEmpty),
                "Cannot have a partially signed event")

        val sortedEventDbs = eventDbs.sortBy(_.nonceIndex)

        PendingDigitDecompositionV0OracleEvent(
          eventDb.pubkey,
          OrderedNonces.fromUnsorted(sortedEventDbs.map(_.nonce)),
          eventDb.eventName,
          eventDb.signingVersion,
          eventDb.maturationTime,
          eventDb.announcementSignature,
          decomp,
          Some(eventDbs)
        )
    }
  }

  /** Verifies if the given attestations sign the outcomes of the given oracle announcement.
    */
  def verifyAttestations(
      announcement: OracleAnnouncementTLV,
      attestationTLV: OracleAttestmentTLV,
      signingVersion: SigningVersion): Boolean = {
    val tlvOutcomes = attestationTLV.outcomes
    val attestations = attestationTLV match {
      case v0: OracleAttestmentV0TLV =>
        v0.unsortedSignatures
    }
    val nonces = announcement.eventTLV match {
      case v0: OracleEventV0TLV =>
        v0.nonces
    }
    if (
      announcement.publicKey != attestationTLV.publicKey ||
      nonces.size != attestations.size ||
      nonces != attestations.map(_.rx)
    ) {
      false
    } else {
      announcement.eventTLV.eventDescriptor match {
        case enumEvent: EnumEventDescriptorV0TLV =>
          require(attestations.size == 1)

          val sig = attestations.head
          enumEvent.outcomes.exists { outcome =>
            val attestationType = EnumAttestation(outcome)
            val hash =
              signingVersion.calcOutcomeHash(attestationType.bytes)
            announcement.publicKey.verify(hash,
                                          sig) && outcome == tlvOutcomes.head
          }

        case dd: DigitDecompositionEventDescriptorV0TLV =>
          require(attestations.nonEmpty)

          val (validSign, attestationsToVerify, outcomesToVerify) =
            dd match {
              case _: SignedDigitDecompositionEventDescriptor =>
                val signOutcomes = Vector(
                  DigitDecompositionSignAttestation(true),
                  DigitDecompositionSignAttestation(false))

                val validSign = signOutcomes.exists { attestationType =>
                  val hash =
                    signingVersion.calcOutcomeHash(attestationType.bytes)
                  announcement.publicKey.verify(
                    hash,
                    attestations.head) && tlvOutcomes.head.toString == attestationType.outcomeString
                }

                (validSign, attestations.tail, tlvOutcomes.tail)
              case _: UnsignedDigitDecompositionEventDescriptor =>
                (true, attestations, tlvOutcomes)
            }

          lazy val digitOutcomes =
            0.until(dd.base.toInt)
              .map(DigitDecompositionAttestation.apply)

          lazy val validDigits =
            attestationsToVerify.zip(outcomesToVerify).forall {
              case (sig, outcome) =>
                digitOutcomes.exists { attestationType =>
                  val hash =
                    signingVersion.calcOutcomeHash(attestationType.bytes)
                  announcement.publicKey.verify(
                    hash,
                    sig) && attestationType.outcomeString == outcome.toString
                }
            }

          validSign && validDigits
      }
    }
  }
}
