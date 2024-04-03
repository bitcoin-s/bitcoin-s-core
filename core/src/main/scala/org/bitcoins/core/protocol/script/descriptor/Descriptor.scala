package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.script.{
  P2PKHScriptPubKey,
  P2PKScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  RawScriptPubKey,
  ScriptPubKey
}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.StringFactory

import scala.util.Try

/** @see [[https://github.com/bitcoin/bitcoin/blob/master/doc/descriptors.md]]
  */
sealed abstract class Descriptor {

  def expression: DescriptorExpression
  def checksum: Option[String]

  override def toString: String = {
    val checksumStr = checksum match {
      case Some(c) => "#" + c
      case None    => ""
    }
    expression.toString + checksumStr
  }
}

sealed abstract class ScriptDescriptor extends Descriptor {
  override def expression: ScriptExpression

  def scriptPubKey: ScriptPubKey

}

case class RawDescriptor(
    expression: RawScriptExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: RawScriptPubKey = expression.scriptPubKey
}

case class P2WPKHDescriptor(
    expression: P2WPKHExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2WPKHWitnessSPKV0 = expression.scriptPubKey
}

case class P2WSHDescriptor(
    expression: P2WSHExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2WSHWitnessSPKV0 = expression.scriptPubKey
}

case class P2PKDescriptor(
    expression: P2PKScriptExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2PKScriptPubKey = expression.scriptPubKey
}

case class P2PKHDescriptor(
    expression: P2PKHScriptExpression,
    checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2PKHScriptPubKey = expression.scriptPubKey
}

case class P2SHDescriptor(expression: P2SHExpression, checksum: Option[String])
    extends ScriptDescriptor {
  override val scriptPubKey: P2SHScriptPubKey = expression.scriptPubKey
}

sealed abstract class DescriptorFactory[
    T <: Descriptor,
    E <: DescriptorExpression,
    U <: DescriptorType]
    extends StringFactory[T] {
  def descriptorType: U

  override def fromString(string: String): T = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    if (t != descriptorType) {
      sys.error(
        s"Incorrect type for descriptor, got=$t expected=$descriptorType")
    } else {
      val (payload, checksum) = iter.current.span(_ != '#')
      val expressionIter = DescriptorIterator(payload.dropRight(1)) //drop ')'
      val expression = parseValidExpression(expressionIter)
      //now check for a valid checksum
      val checksumOpt =
        if (checksum.nonEmpty) Some(checksum.tail) else None //drop '#'
      val isValidChecksum = Descriptor.isValidChecksum(expression, checksumOpt)
      if (isValidChecksum) {
        createDescriptor(expression, checksumOpt)
      } else {
        sys.error(s"Invalid descriptor checksum given for string=$string")
      }
    }
  }

  protected def parseValidExpression(iter: DescriptorIterator): E

  protected def createDescriptor(e: E, checksum: Option[String]): T
}

object RawDescriptor
    extends DescriptorFactory[
      RawDescriptor,
      RawScriptExpression,
      DescriptorType.Raw.type] {

  override val descriptorType: DescriptorType.Raw.type = DescriptorType.Raw

  override protected def parseValidExpression(
      iter: DescriptorIterator): RawScriptExpression = {
    val raw = RawScriptPubKey.fromAsmHex(iter.current)
    RawScriptExpression(raw)
  }

  override protected def createDescriptor(
      e: RawScriptExpression,
      checksum: Option[String]): RawDescriptor = {
    RawDescriptor(e, checksum)
  }
}

object P2WPKHDescriptor
    extends DescriptorFactory[
      P2WPKHDescriptor,
      P2WPKHExpression,
      DescriptorType.WPKH.type] {
  override val descriptorType: DescriptorType.WPKH.type = DescriptorType.WPKH

  override def parseValidExpression(
      iter: DescriptorIterator): P2WPKHExpression = {
    val keyExpression = iter.takeKeyExpression()
    P2WPKHExpression(keyExpression)
  }

  override protected def createDescriptor(
      e: P2WPKHExpression,
      checksum: Option[String]): P2WPKHDescriptor = {
    P2WPKHDescriptor(e, checksum)
  }
}

object P2WSHDescriptor
    extends DescriptorFactory[
      P2WSHDescriptor,
      P2WSHExpression,
      DescriptorType.WSH.type] {
  override val descriptorType: DescriptorType.WSH.type = DescriptorType.WSH

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2WSHExpression = {
    val scriptExpression = iter.takeScriptExpression()
    P2WSHExpression(scriptExpression)
  }

  override protected def createDescriptor(
      e: P2WSHExpression,
      checksum: Option[String]): P2WSHDescriptor = {
    P2WSHDescriptor(e, checksum)
  }
}

object P2PKDescriptor
    extends DescriptorFactory[
      P2PKDescriptor,
      P2PKScriptExpression,
      DescriptorType.PK.type] {
  override val descriptorType: DescriptorType.PK.type = DescriptorType.PK

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2PKScriptExpression = {
    val keyExpression = iter.takeKeyExpression()
    P2PKScriptExpression(keyExpression)
  }

  override protected def createDescriptor(
      e: P2PKScriptExpression,
      checksum: Option[String]): P2PKDescriptor = {
    P2PKDescriptor(e, checksum)
  }
}

object P2PKHDescriptor
    extends DescriptorFactory[
      P2PKHDescriptor,
      P2PKHScriptExpression,
      DescriptorType.PKH.type] {
  override val descriptorType: DescriptorType.PKH.type = DescriptorType.PKH

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2PKHScriptExpression = {
    val keyExpression = iter.takeKeyExpression()
    P2PKHScriptExpression(keyExpression)
  }

  override protected def createDescriptor(
      e: P2PKHScriptExpression,
      checksum: Option[String]): P2PKHDescriptor = {
    P2PKHDescriptor(e, checksum)
  }
}

object P2SHDescriptor
    extends DescriptorFactory[
      P2SHDescriptor,
      P2SHExpression,
      DescriptorType.SH.type] {
  override val descriptorType: DescriptorType.SH.type = DescriptorType.SH

  override protected def parseValidExpression(
      iter: DescriptorIterator): P2SHExpression = {
    val scriptExpression = iter.takeScriptExpression()
    P2SHExpression(scriptExpression)
  }

  override protected def createDescriptor(
      e: P2SHExpression,
      checksum: Option[String]): P2SHDescriptor = {
    P2SHDescriptor(e, checksum)
  }
}

object ScriptDescriptor extends StringFactory[ScriptDescriptor] {

  private val map: Map[
    DescriptorType,
    DescriptorFactory[
      _ <: ScriptDescriptor,
      _ <: ScriptExpression,
      _ <: DescriptorType]] = {
    Map(
      DescriptorType.Raw -> RawDescriptor,
      DescriptorType.WPKH -> P2WPKHDescriptor,
      DescriptorType.WSH -> P2WSHDescriptor,
      DescriptorType.PK -> P2PKDescriptor,
      DescriptorType.SH -> P2SHDescriptor,
      DescriptorType.PKH -> P2PKHDescriptor
    )
  }

  override def fromString(string: String): ScriptDescriptor = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    t match {
      case s: ScriptDescriptorType =>
        map
          .get(s)
          .map(_.fromString(string))
          .getOrElse(
            sys.error(s"Cannot find parse t=$t to ScriptDescriptor s=$s"))
    }
  }
}

object Descriptor extends StringFactory[Descriptor] {

  final val charset: Vector[Char] = {
    ("0123456789()[],'/*abcdefgh@:$%{}" +
      "IJKLMNOPQRSTUVWXYZ&+-.;<=>?!^_|~" +
      "ijklmnopqrstuvwxyzABCDEFGH`#\"\\ ").toVector
  }

  private val charsetWithIdx: Vector[(Char, Int)] = {
    charset.zipWithIndex
  }

  override def fromString(string: String): Descriptor = {
    val iter = DescriptorIterator(string)
    val t = iter.takeDescriptorType()
    t match {
      case _: ScriptDescriptorType =>
        ScriptDescriptor.fromString(string)
    }
  }

  /** Implements checksum algorithm specified by BIP380 for descriptors
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki#checksum]]
    */
  def createChecksum(string: String): String = {
    require(!string.exists(_ == '#'),
            s"String already contains checksum, got=$string")
    var c = UInt64.one
    var cls = 0
    var clsCount = 0

    string.foreach { case ch =>
      val pos = charsetWithIdx
        .find(_._1 == ch)
        .map(_._2)
        .getOrElse {
          sys.error(s"Invalid character=$ch in descriptor string=$string")
        }
      c = polyMod(c, pos & 31)
      cls = cls * 3 + (pos >> 5)
      clsCount += 1
      if (clsCount == 3) {
        // Emit an extra symbol representing the group numbers, for every 3 characters.
        c = polyMod(c, cls)
        cls = 0
        clsCount = 0
      }
    }

    if (clsCount > 0) {
      c = polyMod(c, cls)
    }

    0.until(8).foreach { _ =>
      c = polyMod(c, 0)
    }

    c = c ^ 1 // Prevent appending zeroes from not affecting the checksum.

    val builder = new StringBuilder(8)
    0.until(8).foreach { j =>
      //ret[j] = CHECKSUM_CHARSET[(c >> (5 * (7 - j))) & 31]
      val idx = (c.toLong >> (5 * (7 - j))) & 31
      val char = Bech32.charset(idx.toInt)
      builder.append(char)
    }

    builder.result()
  }

  def isValidChecksum(
      expression: DescriptorExpression,
      checksumOpt: Option[String]): Boolean = {
    checksumOpt match {
      case None => true //trivially true if we have no checksum
      case Some(checksum) =>
        val t = Try(createChecksum(expression.toString))
        if (t.isFailure) false
        else t.get == checksum
    }
  }

  /** Implement polynomial algorithm for descriptors
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0380.mediawiki#checksum]]
    * @see [[https://github.com/bitcoin/bitcoin/blob/d1e9a02126634f9e2ca0b916b69b173a8646524d/src/script/descriptor.cpp#L90]]
    */
  private def polyMod(c: UInt64, idx: Int): UInt64 = {
    var res = c
    val c0: UInt8 = UInt8((c >> 35).toInt)
    res = (c & UInt64(0x7ffffffffL)) << 5 ^ idx
    if ((c0 & UInt8.one) != UInt8.zero) {
      res = res ^ 0xf5dee51989L
    }

    if ((c0 & UInt8(2)) != UInt8.zero) {
      res = res ^ 0xa9fdca3312L
    }

    if ((c0 & UInt8(4)) != UInt8.zero) {
      res = res ^ 0x1bab10e32dL
    }

    if ((c0 & UInt8(8)) != UInt8.zero) {
      res = res ^ 0x3706b1677aL
    }

    if ((c0 & UInt8(16)) != UInt8.zero) {
      res = res ^ 0x644d626ffdL
    }

    res
  }
}
