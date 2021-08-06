package org.bitcoins.core.util

import org.bitcoins.core.number._
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.blockchain.BlockHeader.TargetDifficultyHelper
import org.bitcoins.crypto.CryptoNumberUtil
import scodec.bits.{BitVector, ByteVector}

import java.math.BigInteger
import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.{Failure, Success, Try}

/** Created by chris on 2/8/16.
  */
sealed abstract class NumberUtil extends CryptoNumberUtil {

  /** Takes 2^^num. */
  def pow2(exponent: Int): BigInt = {
    require(
      exponent < 64,
      "We cannot have anything larger than 2^64 - 1 in a long, you tried to do 2^" + exponent)
    BigInt(1) << exponent
  }

  /** Converts a sequence of [[scala.Byte Byte]] to a [[scala.Int Int]]. */
  def toInt(bytes: ByteVector): Int = toBigInt(bytes).toInt

  /** Converts a hex string to a [[scala.Int Int]]. */
  def toInt(hex: String): Int = toInt(BytesUtil.decodeHex(hex))

  /** Converts a sequence of [[scala.Byte Byte]] to a [[scala.Long Long]]. */
  def toLong(bytes: ByteVector): Long = toBigInt(bytes).toLong

  /** Converts a hex string to a [[scala.Long Long]]. */
  def toLong(hex: String): Long = toLong(BytesUtil.decodeHex(hex))

  /** Converts a sequence uint8 `from` base to `to` base
    * @param pad
    * @param f
    */
  def convert[To <: Number[To]](
      data: Vector[UInt8],
      from: UInt32,
      to: UInt32,
      pad: Boolean,
      f: UInt8 => To): Try[Vector[To]] = {
    var acc: UInt32 = UInt32.zero
    var bits: UInt32 = UInt32.zero
    val ret = Vector.newBuilder[To]
    val maxv: UInt32 = (UInt32.one << to) - UInt32.one
    val eight = UInt32(8)
    val fromU8 = UInt8(from.toLong.toShort)
    if (from > eight || to > eight) {
      Failure(
        new IllegalArgumentException(
          "Can't have convert bits 'from' or 'to' parameter greater than 8"))
    } else {
      data.map { (h: UInt8) =>
        if ((h >> fromU8.toInt) != UInt8.zero) {
          Failure(
            new IllegalArgumentException("Invalid input for bech32: " + h))
        } else {
          acc = (acc << from) | UInt32(h.toLong)
          bits = bits + from
          while (bits >= to) {
            bits = bits - to
            val newBase = UInt8(((acc >> bits) & maxv).toInt.toShort)
            val r: To = f(newBase)
            ret.+=(r)
          }
        }
      }

      if (pad) {
        if (bits > UInt32.zero) {
          val r: Long = (acc << (to - bits) & maxv).toLong
          ret.+=(f(UInt8(r.toShort)))
        }
      } else if (bits >= from || ((acc << (to - bits)) & maxv) != UInt8.zero) {
        Failure(new IllegalArgumentException("Invalid padding in encoding"))
      }
      Success(ret.result())
    }
  }

  def convertBytes[T <: Number[T]](
      data: ByteVector,
      from: UInt32,
      to: UInt32,
      pad: Boolean,
      f: Byte => T): Try[Vector[T]] = {
    val wrapperF: UInt8 => T = { (u8: UInt8) =>
      f(UInt8.toByte(u8))
    }
    convert[T](UInt8.toUInt8s(data), from, to, pad, wrapperF)
  }

  def convertUInt8sToUInt5s(u8s: Vector[UInt8]): Vector[UInt5] = {
    val f = { (u8: UInt8) =>
      UInt5.fromByte(UInt8.toByte(u8))
    }
    val u8sTry =
      NumberUtil.convert[UInt5](u8s, UInt32(8), UInt32(5), pad = true, f)
    u8sTry.get

  }

  def convertUInt5sToUInt8(
      u5s: Vector[UInt5],
      pad: Boolean = false): Vector[UInt8] = {
    val u8s = u5s.map(_.toUInt8)
    val u8sTry =
      NumberUtil.convert[UInt8](u8s,
                                UInt32(5),
                                UInt32(8),
                                pad = pad,
                                { (u8: UInt8) =>
                                  u8
                                })
    //should always be able to convert from uint5 => uint8
    u8sTry.get
  }

  /** Expands the [[org.bitcoins.core.protocol.blockchain.BlockHeader.nBits nBits]]
    * field given in a block header to the _actual_ target difficulty.
    * @see  [[https://bitcoin.org/en/developer-reference#target-nbits developer reference]]
    * for more information
    *
    * Meant to replicate this function in bitcoin core
    * @see [[https://github.com/bitcoin/bitcoin/blob/2068f089c8b7b90eb4557d3f67ea0f0ed2059a23/src/arith_uint256.cpp#L206]]
    * @param nBits
    * @return
    */
  def targetExpansion(nBits: UInt32): BlockHeader.TargetDifficultyHelper = {
    //mantissa bytes without sign bit
    val noSignificand = nBits.bytes.takeRight(3)
    val mantissaBytes = {
      val withSignBit = noSignificand
      val noSignBit = false +: withSignBit.bits.tail
      noSignBit.toByteVector
    }

    val significand = nBits.bytes.head

    //if the most significant bit is set, we have a negative number
    val signum = if (noSignificand.bits.head) {
      -1
    } else {
      1
    }

    val mantissa =
      new BigInteger(signum, mantissaBytes.toArray)
    //guards against a negative exponent, in which case we just shift right
    //see bitcoin core implementation
    val result = {
      if (significand <= 3) {

        val exp = 8 * (3 - significand)
        //avoid shift right, because of weird behavior on the jvm
        //https://stackoverflow.com/questions/47519140/bitwise-shift-right-with-long-not-equaling-zero/47519728#47519728
        mantissa.divide(NumberUtil.pow2(exp).bigInteger)
      } else {
        val exponent = significand - 3

        val pow256 = BigInteger.valueOf(256).pow(exponent)
        mantissa.multiply(pow256)
      }
    }

    val nWordNotZero = mantissa != BigInteger.ZERO
    val nWord = BigInt(mantissa)
    val nSize = nBits.toBigInt / NumberUtil.pow2(24)
    val isNegative: Boolean = {
      //*pfNegative = nWord != 0 && (nCompact & 0x00800000) != 0;
      nWordNotZero &&
      (nBits & UInt32(0x00800000L)) != UInt32.zero
    }

    val isOverflow: Boolean = {
      //nWord != 0 && ((nSize > 34) ||
      //  (nWord > 0xff && nSize > 33) ||
      //  (nWord > 0xffff && nSize > 32));

      nWordNotZero && ((nSize > 34) ||
        (nWord > UInt8.max.toBigInt && nSize > 33) ||
        (nWord > UInt32(0xffffL).toBigInt && nSize > 32))
    }

    BlockHeader.TargetDifficultyHelper(result.abs(), isNegative, isOverflow)
  }

  /** Compressed the big integer to be used inside of [[org.bitcoins.core.protocol.blockchain.BlockHeader.nBits]]
    * @see [[https://github.com/bitcoin/bitcoin/blob/2068f089c8b7b90eb4557d3f67ea0f0ed2059a23/src/arith_uint256.cpp#L226 bitcoin core implementation]]
    * @param bigInteger
    * @return
    */
  def targetCompression(bigInteger: BigInteger, isNegative: Boolean): UInt32 = {
    val bytes = bigInteger.toByteArray
    val bitVec = BitVector(bytes)

    val negativeFlag = UInt32(0x00800000L)

    //emulates bits() in arith_uin256.h
    //Returns the position of the highest bit set plus one, or zero if the
    //value is zero.
    //https://github.com/bitcoin/bitcoin/blob/2068f089c8b7b90eb4557d3f67ea0f0ed2059a23/src/arith_uint256.h#L241
    var size: Int = if (bigInteger == BigInteger.ZERO) {
      0
    } else {
      ((bitVec.length + 7) / 8).toInt
    }

    var compact: UInt32 = {
      if (size <= 3) {
        // GetLow64() << 8 * (3 - nSize);
        //note: counter intuitively, the expression
        // 8 * (3 - nSize)
        //gets evaluated before we apply the shift left operator
        //verified this property with g++
        val shiftAmount = 8 * (3 - size)

        val u64 = toUnsignedInt(bitVec.takeRight(64).toByteArray).bigInteger
          .shiftLeft(shiftAmount)

        UInt32.fromBytes(ByteVector(u64.toByteArray))
      } else {
        //8 * (nSize - 3)
        val shiftAmount = 8 * (size - 3)
        val bn = bigInteger.shiftRight(shiftAmount)
        val bytes = bn.toByteArray.takeRight(4)
        UInt32.fromBytes(ByteVector(bytes))
      }
    }

    //The 0x00800000 bit denotes the sign.
    // Thus, if it is already set, divide the mantissa by 256 and increase the exponent.
    if ((compact & negativeFlag) != UInt32.zero) {
      compact = compact >> 8
      size = size + 1
    }

    //~0x007fffff = 0xff800000
    require((compact & UInt32(0xff800000L)) == UInt32.zero,
            s"Exponent/sign bit must not be set yet in compact encoding")
    require(size < 256, "Size of compact encoding can't be more than 2^256")

    compact = compact | UInt32(size << 24)

    compact = {
      if (isNegative && ((compact & UInt32(0x007fffffL)) != UInt32.zero)) {
        compact | negativeFlag
      } else {
        compact | UInt32.zero
      }
    }

    compact
  }

  def targetCompression(bigInt: BigInt, isNegative: Boolean): UInt32 = {
    targetCompression(bigInt.bigInteger, isNegative)
  }

  def targetCompression(difficultyHelper: TargetDifficultyHelper): UInt32 = {
    targetCompression(difficultyHelper.difficulty, difficultyHelper.isNegative)
  }

  /** Implements this check for overflowing for [[org.bitcoins.core.protocol.blockchain.BlockHeader.nBits]]
    * @see [[https://github.com/bitcoin/bitcoin/blob/2068f089c8b7b90eb4557d3f67ea0f0ed2059a23/src/arith_uint256.cpp#L220 bitcoin core check]]
    * @param nBits
    * @return
    */
  def isNBitsOverflow(nBits: UInt32): Boolean = {
    val noSignificand = nBits.bytes.takeRight(3)
    val mantissaBytes = {
      val withSignBit = noSignificand
      val noSignBit = false +: withSignBit.bits.tail
      noSignBit.toByteVector
    }

    val nSize: Long = nBits.toLong >>> 24L

    val nWord: UInt32 = UInt32.fromBytes(mantissaBytes)

    nWord != UInt32.zero && (
      nSize > 34 ||
        (nWord > UInt32(UInt8.max.toInt) && nSize > 33) ||
        (nWord > UInt32(0xffff) && nSize > 32)
    )
  }

  /** Generates a random positive integer */
  def posInt: Int = {
    Math.abs(scala.util.Random.nextInt())
  }

  /** Decomposes the input num into a list of numDigits digits in the given base.
    * The output Vector has the most significant digit first and the 1's place last.
    */
  def decompose(num: Long, base: Int, numDigits: Int): Vector[Int] = {
    var currentNum: Long = num

    val backwardsDigits = (0 until numDigits).toVector.map { _ =>
      val digit = currentNum % base
      currentNum = currentNum / base

      digit.toInt
    }

    backwardsDigits.reverse
  }

  /** Recomposes the input digits into the number they represent.
    * The input Vector has the most significant digit first and the 1's place last.
    */
  def fromDigits(digits: Vector[Int], base: Int, numDigits: Int): Long = {
    def pow(base: Long, exp: Long): Long = {
      if (math.pow(base.toDouble, numDigits.toDouble) <= Int.MaxValue) {
        math.pow(base.toDouble, exp.toDouble).toLong
      } else { // For large numbers, Double loss of precision becomes an issue
        def powRec(base: Long, currentExp: Long): Long = {
          if (currentExp == 0) 1
          else base * powRec(base, currentExp - 1)
        }

        powRec(base, exp)
      }
    }

    digits.indices.foldLeft(0L) { (numSoFar, index) =>
      numSoFar + digits(index) * pow(base, numDigits - 1 - index).toLong
    }
  }

  /** Returns a pseudorandom, uniformly distributed long value between 0
    *  (inclusive) and the specified value (exclusive), drawn from this
    *  random number generator's sequence.
    *
    *  Stolen from scala.util.Random.nextLong (in scala version 2.13)
    *  @see https://github.com/scala/scala/blob/4aae0b91cd266f02b9f3d911db49381a300b5103/src/library/scala/util/Random.scala#L131
    */
  def randomLong(bound: Long): Long = {
    require(bound > 0, "bound must be positive")

    /*
     * Divide bound by two until small enough for nextInt. On each
     * iteration (at most 31 of them but usually much less),
     * randomly choose both whether to include high bit in result
     * (offset) and whether to continue with the lower vs upper
     * half (which makes a difference only if odd).
     */

    var offset = 0L
    var _bound = bound

    while (_bound >= Integer.MAX_VALUE) {
      val bits = scala.util.Random.nextInt(2)
      val halfn = _bound >>> 1
      val nextn =
        if ((bits & 2) == 0) halfn
        else _bound - halfn
      if ((bits & 1) == 0)
        offset += _bound - nextn
      _bound = nextn
    }
    offset + scala.util.Random.nextInt(_bound.toInt)
  }

  def randomBytes(num: Int): ByteVector = {
    val bytes = new Array[Byte](0 max num)
    scala.util.Random.self.nextBytes(bytes)
    ByteVector(bytes)
  }

  def lexicographicalOrdering[T](implicit
      ord: Ordering[T]): Ordering[Vector[T]] = {
    new Ordering[Vector[T]] {
      override def compare(x: Vector[T], y: Vector[T]): Int = {
        val xe = x.iterator
        val ye = y.iterator

        while (xe.hasNext && ye.hasNext) {
          val res = ord.compare(xe.next(), ye.next())
          if (res != 0) return res
        }

        Ordering.Boolean.compare(xe.hasNext, ye.hasNext)
      }
    }
  }

  /** Stolen from Scala 2.13 IndexedSeq::binarySearch
    * @see https://github.com/scala/scala/blob/4aae0b91cd266f02b9f3d911db49381a300b5103/src/library/scala/collection/IndexedSeq.scala#L117
    */
  @tailrec
  final def search[A, B >: A, Wrapper](
      seq: IndexedSeq[Wrapper],
      elem: B,
      from: Int,
      to: Int,
      unwrap: Wrapper => A)(implicit ord: Ordering[B]): Int = {
    if (from < 0) search(seq, elem, from = 0, to, unwrap)
    else if (to > seq.length) search(seq, elem, from, seq.length, unwrap)
    else if (to <= from) from
    else {
      val idx = from + (to - from - 1) / 2
      math.signum(ord.compare(elem, unwrap(seq(idx)))) match {
        case -1 => search(seq, elem, from, idx, unwrap)(ord)
        case 1  => search(seq, elem, idx + 1, to, unwrap)(ord)
        case _  => idx
      }
    }
  }

  def search[A, B >: A, Wrapper](
      seq: IndexedSeq[Wrapper],
      elem: B,
      unwrap: Wrapper => A)(implicit ord: Ordering[B]): Int = {
    search(seq, elem, from = 0, to = seq.length, unwrap)
  }

  def search[A, B >: A](seq: IndexedSeq[A], elem: B, from: Int, to: Int)(
      implicit ord: Ordering[B]): Int = {
    search(seq, elem, from, to, identity[A])
  }

  def search[A, B >: A](seq: IndexedSeq[A], elem: B)(implicit
      ord: Ordering[B]): Int = {
    search(seq, elem, from = 0, to = seq.length)
  }
}

object NumberUtil extends NumberUtil
