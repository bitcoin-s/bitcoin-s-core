package org.bitcoins.core.hd

import org.bitcoins.core.crypto.ExtKey
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.SeqWrapper
import org.bitcoins.crypto.{Factory, StringFactory}
import scodec.bits.ByteVector

abstract class BIP32Path extends SeqWrapper[BIP32Node] {
  def path: Vector[BIP32Node]
  override protected lazy val wrapped: Vector[BIP32Node] = path

  /** BIP32 paths can be subsets/superset of each other.
    * If all elements in a path `p` is included in a path
    * `P`, (i.e. `p` is a subset of `P`), `p.diff(P)`
    * is the elements from `P` that is not in `p`.
    *
    * @example
    * {{{
    *  // equal paths
    * m/44'/1' diff m/44'/1' == Some(BIP32Path.empty)
    *
    * // diffable path
    * m/44'/0'/0' diff m/44'/0'/0'/0/2 = Some(m/0/2)
    * m/44'/0'/0'/1 diff m/44'/0'/0'/1/2 = Some(m/2)
    *
    * // this is longer than other
    * m/44'/1' diff m/44' == None
    *
    * // any fields are unequal along the way
    * m/44'/1' diff m/43'/2' == None
    * m/44'/1'/0 diff m/44'/2'/1 == None
    * }}}
    */
  def diff(otherPath: BIP32Path): Option[BIP32Path] = {

    if (path.length > otherPath.length) {
      None
    } else if (this.toVector == otherPath.toVector) {
      Some(BIP32Path.empty)
    } else {
      val lengthDiff = otherPath.length - path.length

      val extendedPath: Vector[Option[BIP32Node]] = path.map(Some(_)) ++
        Vector.fill[Option[BIP32Node]](lengthDiff)(None)

      val pathsWithIndices = extendedPath
        .zip(otherPath)
        .zipWithIndex

      val calculatedDiff: Option[BIP32Path] = pathsWithIndices
        .foldLeft(Option(BIP32Path.empty)) {
          // we encountered an error along the way, return
          // none
          case (None, _) => None

          // we've reached the end of our path, append
          // the element from their path but don't
          // include the previous one (as
          // that's shared)
          case (Some(_), ((None, their), index)) if index == path.length =>
            Some(BIP32Path(their))

          // append the next divergent element to
          // the acummed value
          case (Some(accum), ((None, their), _)) =>
            Some(BIP32Path((accum :+ their).toVector))

          // we've not yet reached the start of diverging
          // paths
          case (Some(_), ((Some(our), their), _)) if our == their =>
            Some(BIP32Path(our))

          // paths are divergent, fail the computation
          case (Some(_), ((Some(_), _), _)) =>
            None
        }

      calculatedDiff
    }
  }

  override def toString: String =
    path
      .map { case BIP32Node(index, hardened) =>
        val isHardened = if (hardened) "'" else ""
        index.toString + isHardened
      }
      .fold("m")((accum, curr) => accum + "/" + curr)

  def bytes: ByteVector = path.foldLeft(ByteVector.empty)(_ ++ _.toUInt32.bytes)

}

object BIP32Path extends Factory[BIP32Path] with StringFactory[BIP32Path] {
  private case class BIP32PathImpl(path: Vector[BIP32Node]) extends BIP32Path

  /** The empty BIP32 path "m", i.e. a path that does no
    * child key derivation
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#the-key-tree BIP44]]
    *     section on key trees
    */
  val empty: BIP32Path = BIP32PathImpl(Vector.empty)

  def apply(path: Vector[BIP32Node]): BIP32Path = BIP32PathImpl(path)

  def apply(path: BIP32Node*): BIP32Path = BIP32Path(Vector(path *))

  /** Parses a string representation of a BIP32 path. This is on the form
    * of
    *
    * {{{
    *   m/level/hardenedLevel'/...
    * }}}
    *
    * Where `level` is an integer index and hardenedLevel is an integer
    * index followed by a `'`. Different notation is used in BIP32, but this
    * is the most common way of writing down BIP32 paths.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki BIP43]]
    *     and [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
    *     for examples of this notation.
    */
  override def fromString(string: String): BIP32Path = {
    val parts = string
      .split("/")
      .toVector
      // BIP32 path segments are written both with whitespace between (https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#examples)
      // and without (https://wiki.trezor.io/Standard_derivation_paths)
      .map(_.trim)

    if (parts.isEmpty) {
      sys.error(s"Cannot have bip32 path empty bip32 path, got=$string")
    } else {
      val head = parts.head
      val rest = parts.tail
      require(head == "m",
              """The first element in a BIP32 path string must be "m"""")

      val path = rest.map { str =>
        val (index: String, hardened: Boolean) =
          if (str.endsWith("'") || str.endsWith("h")) {
            (str.dropRight(1), true)
          } else {
            (str, false)
          }
        BIP32Node(index.toInt, hardened)
      }

      BIP32PathImpl(path)
    }
  }

  /** Takes in a BIP32 Path and verifies all paths are hardened
    * @throws RuntimeException is a non hardened path is found
    */
  def fromHardenedString(string: String): BIP32Path = {
    val path = BIP32Path.fromString(string)
    require(path.forall(_.hardened),
            s"Found non hardened path in string=$string")
    path
  }

  private def fromBytes(bytes: ByteVector, littleEndian: Boolean): BIP32Path = {
    require(bytes.size % 4 == 0,
            s"ByteVector is not suited for KeyPath, got=${bytes.length}")

    val parts: Vector[ByteVector] = bytes.grouped(4).toVector

    val path = parts.map { part =>
      val uInt32: UInt32 =
        if (littleEndian) UInt32.fromBytesLE(part) else UInt32.fromBytes(part)
      val hardened = uInt32 >= ExtKey.hardenedIdx
      val index = if (hardened) uInt32 - ExtKey.hardenedIdx else uInt32
      BIP32Node(index.toInt, hardened)
    }

    BIP32Path(path)
  }

  override def fromBytes(bytes: ByteVector): BIP32Path =
    fromBytes(bytes, littleEndian = false)

  override def fromBytesLE(bytes: ByteVector): BIP32Path =
    fromBytes(bytes, littleEndian = true)

}

case class BIP32Node(index: Int, hardened: Boolean) {
  require(index >= 0, s"BIP32 node index must be positive! Got $index")

  /** Converts this node to a BIP32 notation
    * unsigned 32 bit integer
    */
  def toUInt32: UInt32 =
    if (hardened) ExtKey.hardenedIdx + UInt32(index.toLong)
    else UInt32(index)
}
