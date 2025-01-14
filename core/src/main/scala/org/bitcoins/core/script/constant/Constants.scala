package org.bitcoins.core.script.constant

import org.bitcoins.core.number.{Int64, NumberCache}
import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.util.{BitcoinScriptUtil, BytesUtil}
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/** Created by chris on 1/6/16.
  */
/** This is the root class of Script. Every element in the Script language is a
  * ScriptToken - think of this the same way you think about Object in Java.
  */
sealed trait ScriptToken extends NetworkElement {

  /** The byte representation of this [[ScriptToken]]. */
  def bytes: ByteVector

  /** The conversion from the byte representation of a [[ScriptToken]] to a
    * number.
    */
  def toLong: Long = ScriptNumberUtil.toLong(bytes)
}

/** A script operation is an instruction that takes an input and gives an output
  * Think of these as functions.
  */
trait ScriptOperation extends ScriptToken {
  def opCode: Int

  override lazy val bytes: ByteVector = ByteVector.fromByte(toByte)

  lazy val toByte: Byte = opCode.toByte
}

/** A constant in the Script language for instance as String or a number. */
sealed abstract class ScriptConstant extends ScriptToken {

  /** Returns if the [[ScriptConstant]] is encoded in the shortest possible way.
    */
  def isShortestEncoding: Boolean = BitcoinScriptUtil.isShortestEncoding(this)
}

/** Represents a [[ScriptNumber]] in the Script language. */
sealed abstract class ScriptNumber
    extends ScriptConstant
    with Ordered[ScriptNumber] {

  def +(that: ScriptNumber): ScriptNumber =
    ScriptNumber(underlying + that.underlying)

  def unary_- = ScriptNumber(-underlying)

  def -(that: ScriptNumber): ScriptNumber =
    ScriptNumber(underlying - that.underlying)

  def *(that: ScriptNumber): ScriptNumber =
    ScriptNumber(underlying * that.underlying)

  override def compare(that: ScriptNumber): Int =
    underlying compare that.underlying

  def <(that: Int64): Boolean = underlying < that.toLong

  def <=(that: Int64): Boolean = underlying <= that.toLong

  def >(that: Int64): Boolean = underlying > that.toLong

  def >=(that: Int64): Boolean = underlying >= that.toLong

  def &(that: ScriptNumber): ScriptNumber =
    ScriptNumber(underlying & that.underlying)

  def &(that: Int64): ScriptNumber = ScriptNumber(underlying & that.toLong)

  def |(that: ScriptNumber): ScriptNumber =
    ScriptNumber(underlying | that.underlying)

  /** This equality just checks that the underlying scala numbers are
    * equivalent, NOT if the numbers are bitwise equivalent in Script. For
    * instance ScriptNumber(0x01).numEqual(ScriptNumber(0x00000000001)) == true
    * but (ScriptNumber(0x01) == (ScriptNumber(0x00000000001))) == false.
    */
  def numEqual(that: ScriptNumber): Boolean = underlying == that.underlying

  def toInt = {
    val l = toLong
    require(l <= Int.MaxValue && l >= Int.MinValue)
    l.toInt
  }

  override def toLong = underlying

  /** The underlying number of the [[ScriptNumber]]. */
  protected val underlying: Long
}

object ScriptNumber
    extends Factory[ScriptNumber]
    with NumberCache[ScriptNumber] {

  private case class ScriptNumberImpl(underlying: Long, bytes: ByteVector)
      extends ScriptNumber

  /** Represents the number zero inside of bitcoin's script language. */
  lazy val zero: ScriptNumber = checkCached(0)

  /** Represents the number one inside of bitcoin's script language. */
  lazy val one: ScriptNumber = checkCached(1)

  /** Represents the number negative one inside of bitcoin's script language. */
  lazy val negativeOne: ScriptNumber = checkCached(-1)

  /** Bitcoin has a numbering system which has a negative zero. */
  lazy val negativeZero: ScriptNumber = fromHex("80")

  override def fromBytes(bytes: ByteVector) = {
    if (bytes.isEmpty) zero
    else if (BitcoinScriptUtil.isShortestEncoding(bytes)) {
      // if it's the shortest encoding possible, use our cache
      checkCached(ScriptNumberUtil.toLong(bytes))
    } else {
      // else we need to preserve the byte level encoding
      // as Script at the consensus level does not
      // enforce minimal encoding of numbers
      ScriptNumberImpl(ScriptNumberUtil.toLong(bytes), bytes)
    }
  }

  def fromBytes(
      bytes: ByteVector,
      requireMinimal: Boolean): Try[ScriptNumber] = {
    if (requireMinimal && !BitcoinScriptUtil.isShortestEncoding(bytes)) {
      Failure(new IllegalArgumentException(
        s"The given hex was not the shortest encoding for the script number: ${bytes.toHex}"))
    } else if (requireMinimal) {
      // our cache contains minimal encoded script numbers
      // so we can check our cache to try and avoid allocating
      val number = ScriptNumberUtil.toLong(bytes)
      Success(checkCached(number))
    } else {
      // if minimal encoding is not required, unfortunately we need to
      // store the byte representation that came off the wire.
      Try(fromBytes(bytes))
    }
  }

  def apply(underlying: Long): ScriptNumber = {
    checkCached(underlying)
  }

  def apply(bytes: ByteVector, requireMinimal: Boolean): Try[ScriptNumber] =
    apply(BytesUtil.encodeHex(bytes), requireMinimal)

  def apply(hex: String, requireMinimal: Boolean): Try[ScriptNumber] = {
    if (requireMinimal && !BitcoinScriptUtil.isShortestEncoding(hex)) {
      fromBytes(ByteVector.fromValidHex(hex), requireMinimal)
    } else {
      Try(fromHex(hex))
    }
  }

  override def fromNativeNumber(long: Long): ScriptNumber = {
    ScriptNumberImpl(long)
  }

  /** Companion object for [[ScriptNumberImpl]] that gives us access to more
    * constructor types for the [[ScriptNumberImpl]] case class.
    */
  private object ScriptNumberImpl {

    def apply(hex: String): ScriptNumber =
      ScriptNumberImpl(ScriptNumberUtil.toLong(hex), BytesUtil.decodeHex(hex))

    def apply(bytes: ByteVector): ScriptNumber =
      ScriptNumberImpl(ScriptNumberUtil.toLong(bytes))

    def apply(underlying: Long): ScriptNumber = {
      ScriptNumberImpl(underlying,
                       ScriptNumberUtil.longToByteVector(underlying))
    }

    def apply(int64: Int64): ScriptNumber = checkCached(int64.toLong)
  }

}

/** The next byte contains the number of bytes to be pushed onto the stack. */
case object OP_PUSHDATA1 extends ScriptOperation {
  override val opCode: Int = 76

  /** The maximum amount of bytes OP_PUSHDATA1 can push onto the stack. */
  def max = 255
}

/** The next two bytes contain the number of bytes to be pushed onto the stack.
  */
case object OP_PUSHDATA2 extends ScriptOperation {
  override val opCode: Int = 77

  /** The max amount of data that OP_PUSHDATA2 can push onto the stack. */
  def max = 65535
}

/** The next four bytes contain the number of bytes to be pushed onto the stack.
  */
case object OP_PUSHDATA4 extends ScriptOperation {
  override val opCode: Int = 78

  /** The maximum amount of data that OP_PUSHDATA4 can be push on the stack. */
  def max = 4294967295L
}

/** Represents a [[ScriptNumberOperation]] where the number in the operation
  * is pushed onto the stack i.e. OP_0 would be push 0 onto the stack, OP_1
  * would be push 1 onto the stack.
  */
sealed abstract class ScriptNumberOperation
    extends ScriptNumber
    with ScriptOperation {
  override def hex = opCode.toHexString

  /** This is required so that OP_TRUE == OP_1 and OP_FALSE == OP_0 will both be
    * true
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case number: ScriptNumber =>
        number.toLong == toLong
      case _ =>
        super.equals(obj)
    }
  }
}

/** An empty array of bytes is pushed onto the stack. (This is not a no-op: an
  * item is added to the stack.)
  */
case object OP_0 extends ScriptNumberOperation {
  override val opCode: Int = 0

  override val hex: String = "00"

  override val underlying: Long = 0
}

/** An empty array of bytes is pushed onto the stack. (This is not a no-op: an
  * item is added to the stack.)
  */
case object OP_FALSE extends ScriptNumberOperation {
  override val opCode = OP_0.opCode

  override val hex = OP_0.hex

  override val underlying = OP_0.underlying

  override lazy val bytes = OP_0.bytes
}

/** The number 1 is pushed onto the stack. */
case object OP_TRUE extends ScriptNumberOperation {
  override val opCode = 81

  override val underlying: Long = 1
}

/** The number -1 is pushed onto the stack. */
case object OP_1NEGATE extends ScriptNumberOperation {
  override val opCode: Int = 79

  override val underlying: Long = -1
}

/** The number 1 is pushed onto the stack. */
case object OP_1 extends ScriptNumberOperation {
  override val opCode: Int = OP_TRUE.opCode

  override val underlying: Long = OP_TRUE.underlying
}

/** The number 2 is pushed onto the stack. */
case object OP_2 extends ScriptNumberOperation {
  override val opCode: Int = 82

  override val underlying: Long = 2
}

/** The number 3 is pushed onto the stack. */
case object OP_3 extends ScriptNumberOperation {
  override val opCode: Int = 83

  override val underlying: Long = 3
}

/** The number 4 is pushed onto the stack. */
case object OP_4 extends ScriptNumberOperation {
  override val opCode: Int = 84

  override val underlying: Long = 4
}

/** The number 5 is pushed onto the stack. */
case object OP_5 extends ScriptNumberOperation {
  override val opCode: Int = 85

  override val underlying: Long = 5
}

/** The number 6 is pushed onto the stack. */
case object OP_6 extends ScriptNumberOperation {
  override val opCode: Int = 86

  override val underlying: Long = 6
}

/** The number 7 is pushed onto the stack. */
case object OP_7 extends ScriptNumberOperation {
  override val opCode: Int = 87

  override val underlying: Long = 7
}

/** The number 8 is pushed onto the stack. */
case object OP_8 extends ScriptNumberOperation {
  override val opCode: Int = 88

  override val underlying: Long = 8
}

/** The number 9 is pushed onto the stack. */
case object OP_9 extends ScriptNumberOperation {
  override val opCode: Int = 89

  override val underlying: Long = 9
}

/** The number 10 is pushed onto the stack. */
case object OP_10 extends ScriptNumberOperation {
  override val opCode: Int = 90

  override val underlying: Long = 10
}

/** The number 11 is pushed onto the stack. */
case object OP_11 extends ScriptNumberOperation {
  override val opCode: Int = 91

  override val underlying: Long = 11
}

/** The number 12 is pushed onto the stack. */
case object OP_12 extends ScriptNumberOperation {
  override val opCode: Int = 92

  override val underlying: Long = 12
}

/** The number 13 is pushed onto the stack. */
case object OP_13 extends ScriptNumberOperation {
  override val opCode: Int = 93

  override val underlying: Long = 13
}

/** The number 14 is pushed onto the stack. */
case object OP_14 extends ScriptNumberOperation {
  override val opCode: Int = 94

  override val underlying: Long = 14
}

/** The number 15 is pushed onto the stack. */
case object OP_15 extends ScriptNumberOperation {
  override val opCode: Int = 95

  override val underlying: Long = 15
}

/** The number 16 is pushed onto the stack. */
case object OP_16 extends ScriptNumberOperation {
  override val opCode: Int = 96

  override val underlying: Long = 16
}

object ScriptNumberOperation
    extends ScriptOperationFactory[ScriptNumberOperation] {

  /** Finds the [[ScriptNumberOperation]] based on the given integer. */
  def fromNumber(long: Long): Option[ScriptNumberOperation] =
    operations.find(_.toLong == long)

  override val operations: scala.collection.immutable.Vector[
    org.bitcoins.core.script.constant.ScriptNumberOperation
      with Product
      with java.io.Serializable] = Vector(OP_0,
                                          OP_1,
                                          OP_1NEGATE,
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

}

object ScriptConstant extends Factory[ScriptConstant] {

  lazy val zero = ScriptConstant("00")
  lazy val negativeZero = ScriptConstant("80")
  lazy val negativeOne = ScriptConstant("81")

  /** Creates a [[ScriptConstant]] from a sequence of bytes. */
  def fromBytes(bytes: ByteVector): ScriptConstant = ScriptConstantImpl(bytes)

  /** Represent a public key or hash of a public key on our stack. */
  private case class ScriptConstantImpl(bytes: ByteVector)
      extends ScriptConstant

}
