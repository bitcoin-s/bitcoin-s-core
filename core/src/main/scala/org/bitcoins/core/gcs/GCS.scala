package org.bitcoins.core.gcs

import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.crypto.{CryptoUtil, SipHashKey}
import scodec.bits.{BitVector, ByteVector}
import scodec.bits.bin

import scala.annotation.tailrec

// TODO: Replace ByteVector with a type for keys
/** Defines all functionality dealing with Golomb-Coded Sets
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#GolombCoded_Sets]]
  */
object GCS {

  /** Given parameters and data, golomb-encodes the data
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#set-construction]]
    */
  def buildGCS(
      data: Vector[ByteVector],
      key: SipHashKey,
      p: UInt8,
      m: UInt64): BitVector = {
    val hashedValues = hashedSetConstruct(data, key, m)
    val sortedHashedValues = hashedValues.sortWith(_ < _)
    encodeSortedSet(sortedHashedValues, p)
  }

  /** Given parameters and data, constructs a GolombFilter for that data
    */
  def buildGolombFilter(
      data: Vector[ByteVector],
      key: SipHashKey,
      p: UInt8,
      m: UInt64): GolombFilter = {
    val encodedData = buildGCS(data, key, p, m)

    GolombFilter(key, m, p, CompactSizeUInt(UInt64(data.length)), encodedData)
  }

  /** Given data, constructs a GolombFilter for that data using Basic Block Filter parameters
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#block-filters]]
    */
  def buildBasicBlockFilter(
      data: Vector[ByteVector],
      key: SipHashKey): GolombFilter = {
    buildGolombFilter(data, key, FilterType.Basic.P, FilterType.Basic.M)
  }

  private def sipHash(item: ByteVector, key: SipHashKey): UInt64 = {
    val digest = CryptoUtil.sipHash(item, key)

    UInt64.fromHex(digest.toHexString)
  }

  /** Hashes the item to the range [0, f)
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#hashing-data-objects]]
    */
  def hashToRange(item: ByteVector, f: UInt64, key: SipHashKey): UInt64 = {
    val hash = sipHash(item, key)

    val bigInt = (hash.toBigInt * f.toBigInt) >> 64

    UInt64(bigInt)
  }

  /** Hashes the items of a set of items
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#hashing-data-objects]]
    */
  def hashedSetConstruct(
      rawItems: Vector[ByteVector],
      key: SipHashKey,
      m: UInt64): Vector[UInt64] = {
    val n = rawItems.length
    val f = m * n

    val hashedItemsBuilder = Vector.newBuilder[UInt64]

    rawItems.foreach { item =>
      val setValue = hashToRange(item, f, key)
      hashedItemsBuilder.+=(setValue)
    }

    hashedItemsBuilder.result()
  }

  /** Converts num to unary (6 = 1111110)
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-rice-coding]]
    *
    * TODO: protect against large inputs which cause OutOfMemoryErrors and even larger ones which fail on toInt
    */
  def toUnary(num: UInt64): BitVector = {
    if (num == UInt64.zero) {
      bin"0"
    } else {
      /*
       * We use the fact that 2^n - 1 = 111...1 (in binary) where there are n 1 digits
       */
      val binUnary = (BigInt(1) << num.toInt) - 1
      val leftPadded = BitVector(binUnary.toByteArray)
      val noPadding = dropLeftPadding(leftPadded)

      noPadding.:+(false)
    }
  }

  /** Encodes a hash into a unary prefix and binary suffix
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-rice-coding]]
    */
  def golombEncode(item: UInt64, p: UInt8): BitVector = {
    val q = item >> p.toInt

    val prefix = toUnary(q)

    val pBits = item.bytes.toBitVector.takeRight(p.toInt)

    prefix ++ pBits
  }

  /** Decodes an item off of the front of a BitVector by reversing [[GCS.golombEncode]]
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#golomb-rice-coding]]
    */
  def golombDecode(codedItem: BitVector, p: UInt8): UInt64 = {
    @tailrec
    def split(vec: BitVector, accum: UInt64): (UInt64, BitVector) = {
      if (vec.head) {
        split(vec.tail, accum + UInt64.one)
      } else {
        (accum, vec.tail)
      }
    }

    val (q, pBits) = split(codedItem, UInt64.zero)

    val sizeWithPadding = (8 - (p.toInt % 8)) + p.toInt

    val pBitsAsBytes = {
      val withoutRightPaddingOrData = pBits.take(p.toInt)
      val withLeftPadding = withoutRightPaddingOrData.padLeft(sizeWithPadding)
      withLeftPadding.toByteVector
    }

    (q << p.toInt) + UInt64.fromBytes(pBitsAsBytes)
  }

  @tailrec
  private def dropLeftPadding(padded: BitVector): BitVector = {
    if (padded.isEmpty || padded.head) {
      padded
    } else {
      dropLeftPadding(padded.tail)
    }
  }

  /** Returns the first hash gcs-encoded at the front of a BitVector, as well as the remaining BitVector */
  private def golombDecodeItemFromSet(
      encodedData: BitVector,
      p: UInt8): (UInt64, BitVector) = {
    val head = golombDecode(encodedData, p)

    val prefixSize = (head >> p.toInt).toInt + 1

    (head, encodedData.drop(prefixSize + p.toInt))
  }

  /** Decodes all hashes from golomb-encoded data, reversing [[GCS.encodeSortedSet]]
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#set-queryingdecompression]]
    */
  def golombDecodeSet(encodedData: BitVector, p: UInt8): Vector[UInt64] =
    golombDecodeSetsWithPredicate(encodedData, p) { _ =>
      true
    }

  /** Decodes all hashes while the given predicate returns true
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#set-queryingdecompression]]
    */
  def golombDecodeSetsWithPredicate(encodedData: BitVector, p: UInt8)(
      predicate: UInt64 => Boolean): Vector[UInt64] = {
    @tailrec
    def loop(
        encoded: BitVector,
        lastHash: UInt64,
        decoded: Vector[UInt64]): Vector[UInt64] = {
      if (encoded.length < p.toInt + 1) { // Only padding left
        decoded
      } else {
        val (delta, encodedLeft) = golombDecodeItemFromSet(encoded, p)
        val hash = lastHash + delta
        if (predicate(hash)) {
          loop(encodedLeft, hash, decoded :+ hash)
        } else {
          decoded
        }
      }
    }

    loop(encoded = encodedData, lastHash = UInt64.zero, Vector.empty)
  }

  /** Given a set of ascending hashes, golomb-encodes them
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#set-construction]]
    */
  def encodeSortedSet(hashes: Vector[UInt64], p: UInt8): BitVector = {
    val (golombStream, _) = hashes.foldLeft((BitVector.empty, UInt64.zero)) {
      case ((accum, lastHash), hash) =>
        val delta = hash - lastHash
        val encoded = golombEncode(delta, p)
        (accum ++ encoded, hash)
    }

    golombStream
  }
}
