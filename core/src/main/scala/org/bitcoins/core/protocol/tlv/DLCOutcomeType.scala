package org.bitcoins.core.protocol.tlv

import org.bitcoins.crypto.CryptoUtil
import scodec.bits.ByteVector

/** Represents a DLC event that could be signed by an oracle */
sealed trait DLCOutcomeType {

  /** The ByteVectors to be signed by the oracle using pre-committed nonces */
  def serialized: Vector[ByteVector]
}

/** An outcome from an enumerated event type */
case class EnumOutcome(outcome: String) extends DLCOutcomeType {

  override lazy val serialized: Vector[ByteVector] =
    Vector(CryptoUtil.serializeForHash(outcome))
}

sealed trait NumericDLCOutcomeType extends DLCOutcomeType

/** An outcome from a multi-nonce unsigned numeric event type.
  *
  * If digits.length is less than the total number of digits to be signed by
  * the oracle then this outcome represents all outcomes prefixed by the given
  * digits.
  *
  * I.e. the Vector[Int] is always the most significant digits.
  */
case class UnsignedNumericOutcome(digits: Vector[Int])
    extends NumericDLCOutcomeType {

  override lazy val serialized: Vector[ByteVector] =
    digits.map(digit => CryptoUtil.serializeForHash(digit.toString))
}

/** An outcome from a multi-nonce signed numeric event type.
  *
  * If digits.length is less than the total number of digits to be signed by
  * the oracle then this outcome represents all outcomes prefixed by the given
  * digits.
  *
  * I.e. the Vector[Int] is always the most significant digits.
  */
case class SignedNumericOutcome(positive: Boolean, digits: Vector[Int])
    extends NumericDLCOutcomeType {

  private val signOutcomeStr = if (positive) "+" else "-"

  override lazy val serialized: Vector[ByteVector] =
    CryptoUtil.serializeForHash(signOutcomeStr) +:
      digits.map(digit => CryptoUtil.serializeForHash(digit.toString))
}
