package org.bitcoins.core.number

import org.bitcoins.core._
import org.bitcoins.core.util.{BytesUtil, NumberUtil}
import org.bitcoins.crypto.{CryptoBytesUtil, Factory, NetworkElement}
import scodec.bits.{ByteOrdering, ByteVector}

/** Created by chris on 6/4/16.
  */
/** This abstract class is meant to represent a signed and unsigned number in C
  * This is useful for dealing with codebases/protocols that rely on C's
  * unsigned integer types
  */
sealed abstract class Number[T <: Number[T]]
    extends NetworkElement
    with Ordered[T]
    with BasicArithmetic[T] {
  type A = BigInt

  /** The underlying scala number used to hold the number */
  protected def underlying: A

  def toByte: Byte = toBigInt.bigInteger.byteValueExact()
  def toInt: Int = toBigInt.bigInteger.intExact
  def toLong: Long = toBigInt.bigInteger.longExact
  def toBigInt: BigInt = underlying

  /** This is used to determine the valid amount of bytes in a number for
    * instance a UInt8 has an andMask of 0xff a UInt32 has an andMask of
    * 0xffffffff
    */
  def andMask: BigInt

  /** Factory function to create the underlying T, for instance a UInt32. This
    * method must check if the parameter is in the required range.
    */
  def apply: A => T

  override def +(num: T): T = apply(underlying + num.underlying)
  override def -(num: T): T = apply(underlying - num.underlying)
  override def *(factor: BigInt): T = apply(underlying * factor)
  override def *(num: T): T = apply(underlying * num.underlying)

  def /(num: T): T = apply(underlying / num.underlying)

  override def compare(num: T): Int = underlying compare num.underlying

  def <<(num: Int): T = this.<<(apply(num))
  def >>(num: Int): T = this.>>(apply(num))

  def <<(num: T): T = {
    val toInt = num.toInt
    apply((underlying << toInt) & andMask)
  }

  def >>(num: T): T = {
    // this check is for weird behavior with the jvm and shift rights
    // https://stackoverflow.com/questions/47519140/bitwise-shift-right-with-long-not-equaling-zero/47519728#47519728
    if (num.toLong > 63) apply(0)
    else {
      val toInt = num.toInt
      apply(underlying >> toInt)
    }
  }

  def |(num: T): T = apply(underlying | num.underlying)
  def &(num: T): T = apply(underlying & num.underlying)
  def unary_- : T = apply(-underlying)

  def isSigned: Boolean

  def ^(num: T): T = {
    val xor = bytes.xor(num.bytes)
    if (isSigned) {
      val r = xor.toLong(signed = isSigned)
      apply(r)
    } else {
      val r = BigInt(1, xor.toArray)
      apply(r)
    }
  }

  def xor(num: T): T = {
    ^(num)
  }

  def xor(num: Long): T = xor(apply(num))

  def ^(num: Long): T = ^(apply(num))

  def truncatedBytes: ByteVector = bytes.dropWhile(_ == 0x00)
}

/** Represents a signed number in our number system Instances of this are
  * [[Int32]] or [[Int64]]
  */
sealed abstract class SignedNumber[T <: Number[T]] extends Number[T] {
  final override def isSigned: Boolean = true
}

/** Represents an unsigned number in our number system Instances of this are
  * [[UInt32]] or [[UInt64]]
  */
sealed abstract class UnsignedNumber[T <: Number[T]] extends Number[T] {
  final override def isSigned: Boolean = false
}

/** This number type is useful for dealing with
  * [[org.bitcoins.core.util.Bech32]] related applications. The native encoding
  * for Bech32 is a 5 bit number which is what this abstraction is meant to be
  * used for
  */
sealed abstract class UInt5 extends UnsignedNumber[UInt5] {
  override def apply: A => UInt5 = UInt5(_)

  override val andMask: BigInt = 0x1f

  def byte: Byte = toInt.toByte

  def toUInt8: UInt8 = UInt8(toInt)

  override val bytes: ByteVector = toUInt8.bytes
}

sealed abstract class UInt8 extends UnsignedNumber[UInt8] {
  override def apply: A => UInt8 = UInt8(_)

  override val bytes: ByteVector = ByteVector.fromInt(toInt, size = 1)

  override val andMask = 0xff

  def toUInt5: UInt5 = {
    // this will throw if not in range of a UInt5, come back and look later
    UInt5(toInt)
  }
}

/** Represents a uint16_t in C
  */
sealed abstract class UInt16 extends UnsignedNumber[UInt16] {
  override def apply: A => UInt16 = UInt16(_)
  override val bytes: ByteVector = ByteVector.fromInt(toInt, size = 2)

  override val andMask = 0xffffL
}

/** Represents a uint32_t in C
  */
sealed abstract class UInt32 extends UnsignedNumber[UInt32] {
  override def apply: A => UInt32 = UInt32(_)
  override val bytes: ByteVector = ByteVector.fromLong(toLong, 4)
  override val andMask = 0xffffffffL
}

/** Represents a uint64_t in C
  */
sealed abstract class UInt64 extends UnsignedNumber[UInt64] {

  override val bytes: ByteVector = {
    if (underlying.isValidLong) {
      // optimization, if our number fits into a long
      // we can get much better performance from ByteVector
      ByteVector.fromLong(underlying.toLong, 8)
    } else {
      // else just do what we were doing before
      ByteVector.fromValidHex(encodeHex(bigInt = underlying))
    }
  }
  override def apply: A => UInt64 = UInt64(_)
  override val andMask = 0xffffffffffffffffL

  /** Converts a [[BigInt]] to a 8 byte hex representation. [[BigInt]] will only
    * allocate 1 byte for numbers like 1 which require 1 byte, giving us the hex
    * representation 01 this function pads the hex chars to be 0000000000000001
    *
    * @param bigInt
    *   The number to encode
    * @return
    *   The hex encoded number
    */
  private def encodeHex(bigInt: BigInt): String = {
    val hex = BytesUtil.encodeHex(bigInt)
    if (hex.length == 18) {
      // means that encodeHex(BigInt) padded an extra byte, giving us 9 bytes instead of 8
      hex.slice(2, hex.length)
    } else {
      val needed = 16 - hex.length
      CryptoBytesUtil.addPadding(needed, hex)
    }
  }
}

/** Represents a int32_t in C
  */
sealed abstract class Int32 extends SignedNumber[Int32] {
  override def apply: A => Int32 = Int32(_)
  override val andMask = 0xffffffff
  override val bytes: ByteVector = ByteVector.fromInt(i = toInt, size = 4)
  def toUInt32: UInt32 = UInt32.fromBytes(bytes)
}

/** Represents a int64_t in C
  */
sealed abstract class Int64 extends SignedNumber[Int64] {
  override def apply: A => Int64 = Int64(_)
  override val andMask = 0xffffffffffffffffL
  override val bytes: ByteVector = ByteVector.fromLong(l = toLong, size = 8)
}

/** Represents number types that are bounded by minimum and maximum values
  *
  * @tparam T
  *   Type of the numbers
  */
trait Bounded[T] {
  def min: T
  def max: T
}

trait BaseNumbers[T] {
  def zero: T
  def one: T
}

/** Should be implemented inside of any companion object for a number
  */
trait NumberObject[T <: Number[T]] extends BaseNumbers[T] {
  type A = BigInt

  def isInBound(num: A): Boolean
}

object UInt5
    extends Factory[UInt5]
    with NumberObject[UInt5]
    with Bounded[UInt5]
    with NumberCache[UInt5] {

  private case class UInt5Impl(underlying: BigInt) extends UInt5 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }

  lazy val zero = checkCached(0)
  lazy val one = checkCached(1)

  private lazy val minUnderlying: A = 0
  private lazy val maxUnderlying: A = 31

  lazy val min = UInt5(minUnderlying)
  lazy val max = UInt5(maxUnderlying)

  /** Max number for UInt5 is 31 */
  final override val maxCached: Long = 31

  final override def fromNativeNumber(long: Long): UInt5 = {
    UInt5Impl(long)
  }

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  def apply(byte: Byte): UInt5 = fromByte(byte)

  def apply(bigInt: BigInt): UInt5 = {
    require(
      bigInt.toByteArray.length == 1,
      s"To create a uint5 from a BigInt it must be less than 32. Got: ${bigInt.toString}")

    UInt5.fromByte(bigInt.toByteArray.head)
  }

  override def fromBytes(bytes: ByteVector): UInt5 = {
    require(
      bytes.size == 1,
      s"To create a uint5 from a ByteVector it must be of size one ${bytes.length}")
    UInt5.fromByte(bytes.head)
  }

  def fromByte(byte: Byte): UInt5 = {
    checkCached(byte)
  }

  def toUInt5(b: Byte): UInt5 = {
    fromByte(b)
  }

  def toUInt5s(bytes: ByteVector): Vector[UInt5] = {
    bytes.toArray.map(toUInt5).toVector
  }
}

object UInt8
    extends Factory[UInt8]
    with NumberObject[UInt8]
    with Bounded[UInt8]
    with NumberCache[UInt8] {

  private case class UInt8Impl(underlying: BigInt) extends UInt8 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }
  lazy val zero = checkCached(0.toShort)
  lazy val one = checkCached(1.toShort)

  private lazy val minUnderlying: A = 0
  private lazy val maxUnderlying: A = 255

  lazy val min = UInt8(minUnderlying)
  lazy val max = UInt8(maxUnderlying)

  final override def fromNativeNumber(long: Long): UInt8 = {
    UInt8Impl(long)
  }

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  def apply(short: Short): UInt8 = checkCached(short)

  def apply(byte: Byte): UInt8 = toUInt8(byte)

  def apply(bigint: BigInt): UInt8 = UInt8Impl(bigint)

  override def fromBytes(bytes: ByteVector): UInt8 = {
    require(
      bytes.size == 1,
      "Can only create a uint8 from a byte array of size one, got: " + bytes)
    UInt8(NumberUtil.toUnsignedInt(bytes))
  }

  def toUInt8(byte: Byte): UInt8 = {
    val unsigned = ByteVector.fromByte(byte).toInt(signed = false)
    checkCached(unsigned)
  }

  def toByte(uInt8: UInt8): Byte = uInt8.underlying.toByte

  def toBytes(us: Seq[UInt8]): ByteVector = {
    ByteVector(us.map(toByte))
  }

  def toUInt8s(bytes: ByteVector): Vector[UInt8] = {
    bytes.toArray.map(toUInt8).toVector
  }
}

object UInt16
    extends Factory[UInt16]
    with NumberObject[UInt16]
    with Bounded[UInt16]
    with NumberCache[UInt16] {

  private case class UInt16Impl(underlying: BigInt) extends UInt16 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }

  lazy val zero = checkCached(0)
  lazy val one = checkCached(1)
  lazy val two = checkCached(2)

  private lazy val minUnderlying: A = 0
  private lazy val maxUnderlying: A = BigInt(65535L)

  lazy val min = zero
  lazy val max = UInt16(maxUnderlying)

  final override def fromNativeNumber(long: Long): UInt16 = {
    UInt16Impl(long)
  }

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): UInt16 = {
    require(
      bytes.size <= 2,
      "UInt16 byte array was too large, got: " + BytesUtil.encodeHex(bytes))
    UInt16(bytes.toLong(signed = false, ordering = ByteOrdering.BigEndian))
  }

  def fromTruncatedBytes(bytes: ByteVector): UInt16 = {
    fromBytes(bytes.padLeft(2))
  }

  def apply(long: Long): UInt16 = {
    checkCached(long)
  }

  def apply(bigInt: BigInt): UInt16 = {
    UInt16Impl(bigInt)
  }
}

object UInt32
    extends Factory[UInt32]
    with NumberObject[UInt32]
    with Bounded[UInt32]
    with NumberCache[UInt32] {

  private case class UInt32Impl(underlying: BigInt) extends UInt32 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }

  lazy val zero: UInt32 = checkCached(0)
  lazy val one: UInt32 = checkCached(1)
  lazy val two: UInt32 = checkCached(2)

  private lazy val minUnderlying: A = 0
  private lazy val maxUnderlying: A = BigInt(4294967295L)

  lazy val min = zero
  lazy val max = UInt32(maxUnderlying)

  final override def fromNativeNumber(long: Long): UInt32 = {
    UInt32Impl(long)
  }

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): UInt32 = {
    require(
      bytes.size <= 4,
      "UInt32 byte array was too large, got: " + BytesUtil.encodeHex(bytes))
    UInt32(bytes.toLong(signed = false, ordering = ByteOrdering.BigEndian))
  }

  def fromTruncatedBytes(bytes: ByteVector): UInt32 = {
    fromBytes(bytes.padLeft(4))
  }

  def apply(int: Int): UInt32 = {
    apply(int.toLong)
  }

  def apply(long: Long): UInt32 = {
    checkCached(long)
  }

  def apply(bigInt: BigInt): UInt32 = {
    UInt32Impl(bigInt)
  }
}

object UInt64
    extends Factory[UInt64]
    with NumberObject[UInt64]
    with Bounded[UInt64]
    with NumberCacheBigInt[UInt64] {

  private case class UInt64Impl(underlying: BigInt) extends UInt64 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }

  lazy val zero = checkCached(0)
  lazy val one = checkCached(1)

  private lazy val minUnderlying: A = 0
  private lazy val maxUnderlying: A = BigInt("18446744073709551615")

  lazy val min = UInt64(minUnderlying)
  lazy val max = UInt64(maxUnderlying)

  lazy val twentyThree = UInt64(BigInt(23)) // p2sh compact size uint
  lazy val twentyFive = UInt64(BigInt(25)) // p2pkh compact size uint
  lazy val oneHundredFive = UInt64(BigInt(105)) // multisig spk 3 public keys
  lazy val thirtyFour = UInt64(BigInt(34)) // p2wsh compact size uint
  lazy val twentyTwo = UInt64(BigInt(22)) // p2pwpkh compact size uint

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): UInt64 = {
    require(bytes.size <= 8)
    UInt64(NumberUtil.toUnsignedInt(bytes))
  }

  def fromTruncatedBytes(bytes: ByteVector): UInt64 = {
    fromBytes(bytes.padLeft(8))
  }

  def apply(long: Long): UInt64 = {
    checkCached(long)
  }

  def apply(num: BigInt): UInt64 = checkCachedBigInt(num)

  final override def fromBigInt(bigInt: BigInt): UInt64 = {
    UInt64Impl(bigInt)
  }

  final override def fromNativeNumber(long: Long): UInt64 = {
    UInt64Impl(long)
  }
}

object Int32
    extends Factory[Int32]
    with NumberObject[Int32]
    with Bounded[Int32]
    with NumberCache[Int32] {

  private case class Int32Impl(underlying: BigInt) extends Int32 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }
  val negOne = Int32(-1)

  override val zero: Int32 = checkCached(0)
  override val one: Int32 = checkCached(1)
  val two = checkCached(2)

  private lazy val minUnderlying: A = BigInt(-2147483648)
  private lazy val maxUnderlying: A = BigInt(2147483647)

  lazy val min = Int32(minUnderlying)
  lazy val max = Int32(maxUnderlying)

  final override def fromNativeNumber(long: Long): Int32 = {
    Int32Impl(long)
  }

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): Int32 = {
    require(bytes.size <= 4, "We cannot have an Int32 be larger than 4 bytes")
    Int32(bytes.toInt(signed = true, ordering = ByteOrdering.BigEndian))
  }

  def apply(int: Int): Int32 = {
    checkCached(int)
  }

  def apply(bigInt: BigInt): Int32 = Int32Impl(bigInt)
}

object Int64
    extends Factory[Int64]
    with NumberObject[Int64]
    with Bounded[Int64]
    with NumberCache[Int64] {

  private case class Int64Impl(underlying: BigInt) extends Int64 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }

  lazy val zero = checkCached(0)
  lazy val one = checkCached(1)

  private lazy val minUnderlying: A = -9223372036854775808L
  private lazy val maxUnderlying: A = 9223372036854775807L

  lazy val min = Int64(minUnderlying)
  lazy val max = Int64(maxUnderlying)

  final override def fromNativeNumber(long: Long): Int64 = {
    Int64Impl(long)
  }

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): Int64 = {
    require(bytes.size <= 8, "We cannot have an Int64 be larger than 8 bytes")
    Int64(BigInt(bytes.toArray).toLong)
  }

  def apply(long: Long): Int64 = {
    checkCached(long)
  }

  def apply(bigInt: BigInt): Int64 = Int64Impl(bigInt)
}
