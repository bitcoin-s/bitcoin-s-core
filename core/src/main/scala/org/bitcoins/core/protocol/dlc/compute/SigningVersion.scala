package org.bitcoins.core.protocol.dlc.compute

import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.{CryptoUtil, SchnorrNonce, StringFactory}
import scodec.bits.ByteVector

sealed abstract class SigningVersion {

  /** Calculates the tweak for the oracle's pre-committed nonce */
  def calcNonceTweak(nonce: SchnorrNonce, eventName: String): ByteVector

  /** Calculates the bytes to sign for an OracleAnnouncement */
  def calcAnnouncementHash(eventTLV: OracleEventTLV): ByteVector

  /** Calculates the bytes to sign for an event outcome */
  def calcOutcomeHash(bytes: ByteVector): ByteVector

  /** Calculates the bytes to sign for an event outcome */
  final def calcOutcomeHash(string: String): ByteVector = {
    calcOutcomeHash(CryptoUtil.serializeForHash(string))
  }
}

object SigningVersion extends StringFactory[SigningVersion] {

  /** Initial signing version that was created, not a part of any spec */
  case object Mock extends SigningVersion {

    override def calcNonceTweak(
        nonce: SchnorrNonce,
        eventName: String): ByteVector = {
      val bytes = nonce.bytes ++ CryptoUtil.serializeForHash(eventName)

      CryptoUtil.taggedSha256(bytes, "DLCv0/Nonce").bytes
    }

    override def calcAnnouncementHash(eventTLV: OracleEventTLV): ByteVector =
      CryptoUtil.taggedSha256(eventTLV.bytes, "DLCv0/Announcement").bytes

    override def calcOutcomeHash(byteVector: ByteVector): ByteVector =
      CryptoUtil.taggedSha256(byteVector, "DLCv0/Outcome").bytes
  }

  /** Used before we had an actual signing algorithm in the spec */
  case object BasicSHA256SigningVersion extends SigningVersion {

    override def calcNonceTweak(
        nonce: SchnorrNonce,
        eventName: String): ByteVector = {
      val bytes = nonce.bytes ++ CryptoUtil.serializeForHash(eventName)

      CryptoUtil.taggedSha256(bytes, "BasicSHA256").bytes
    }

    override def calcAnnouncementHash(eventTLV: OracleEventTLV): ByteVector =
      CryptoUtil.sha256(eventTLV.bytes).bytes

    override def calcOutcomeHash(byteVector: ByteVector): ByteVector = {
      CryptoUtil.sha256(byteVector).bytes
    }
  }

  /** V0 DLC Oracle singing algo, specified in https://github.com/discreetlogcontracts/dlcspecs/pull/113 */
  case object DLCOracleV0SigningVersion extends SigningVersion {

    override def calcNonceTweak(
        nonce: SchnorrNonce,
        eventName: String): ByteVector = {
      val bytes = nonce.bytes ++ CryptoUtil.serializeForHash(eventName)

      CryptoUtil.taggedSha256(bytes, "DLC/oracle/nonce/v0").bytes
    }

    override def calcAnnouncementHash(eventTLV: OracleEventTLV): ByteVector =
      CryptoUtil.sha256DLCAnnouncement(eventTLV.bytes).bytes

    override def calcOutcomeHash(byteVector: ByteVector): ByteVector = {
      CryptoUtil.sha256DLCAttestation(byteVector).bytes
    }
  }

  val latest: SigningVersion = DLCOracleV0SigningVersion

  val all: Vector[SigningVersion] =
    Vector(Mock, BasicSHA256SigningVersion, DLCOracleV0SigningVersion)

  override def fromStringOpt(str: String): Option[SigningVersion] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }

  override def fromString(string: String): SigningVersion = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find signing version for string=$string")
    }
  }
}
