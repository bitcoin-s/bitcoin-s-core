package org.bitcoins.core.gcs

import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.crypto.{Factory, NetworkElement, StringFactory}
import scodec.bits._

/** Filter types for BIP158 block content filters
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters BIP158]]
  */
sealed abstract class FilterType extends NetworkElement {
  val M: UInt64
  val P: UInt8
}

object FilterType extends Factory[FilterType] with StringFactory[FilterType] {

  val knownFilterTypes: Map[FilterType, Short] = Map(Basic -> 0.toShort)

  val knownFilterTypeCodes: Map[Short, FilterType] = Map(0.toShort -> Basic)

  def fromBytes(bytes: ByteVector): FilterType =
    bytes match {
      case Basic.bytes => Basic
      case other: ByteVector =>
        throw new IllegalArgumentException(
          s"'${other.toHex}' is not a known filter type")
    }

  /** Currently the only defined filter type */
  case object Basic extends FilterType {
    val bytes: ByteVector = hex"0x00"

    val M: UInt64 = UInt64(784931)
    val P: UInt8 = UInt8(19)
  }

  def getCode(filterType: FilterType): Short =
    knownFilterTypes.get(filterType) match {
      case Some(code) => code
      case None =>
        throw new IllegalArgumentException(
          s"Unknown filter type: ${filterType}")
    }

  def byCode(code: Short): FilterType =
    knownFilterTypeCodes.get(code) match {
      case Some(filterType) => filterType
      case None =>
        throw new IllegalArgumentException(s"Unknown filter type code: ${code}")
    }

  override def fromString(string: String): FilterType = {
    fromStringOpt(string) match {
      case Some(filterType) => filterType
      case None =>
        sys.error(s"Could not parse string=$string to known filter type")
    }
  }

  override def fromStringOpt(string: String): Option[FilterType] = {
    knownFilterTypes
      .map(_._1)
      .find(_.toString.toLowerCase == string.toLowerCase)
  }

}
