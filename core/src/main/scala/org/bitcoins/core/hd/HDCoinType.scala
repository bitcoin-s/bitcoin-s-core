package org.bitcoins.core.hd

import org.bitcoins.core.config._

/** Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#Coin_type BIP44]],
  * [[https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki BIP84]]
  * and
  * [[https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki BIP49]]
  * coin type.
  */
sealed trait HDCoinType {
  def toInt: Int
}

/** @see [[https://github.com/satoshilabs/slips/blob/master/slip-0044.md SLIP-0044]]
  *     central registry of coin types
  */
object HDCoinType {

  case object Bitcoin extends HDCoinType {
    override val toInt: Int = 0
  }

  case object Testnet extends HDCoinType {
    override val toInt: Int = 1
  }

  case class UnknownCoinType(int: Int) extends HDCoinType {
    override val toInt: Int = int
  }

  lazy val all: Vector[HDCoinType] = Vector(Bitcoin, Testnet)

  def fromKnown(int: Int): Option[HDCoinType] = all.find(_.toInt == int)

  def fromInt(int: Int): HDCoinType =
    fromKnown(int).getOrElse(UnknownCoinType(int))

  def fromNetwork(np: NetworkParameters): HDCoinType = {
    np match {
      case MainNet                     => Bitcoin
      case TestNet3 | RegTest | SigNet => Testnet
    }
  }

  def fromNode(node: BIP32Node): HDCoinType = {
    require(node.hardened,
            s"Cannot construct HDCoinType from un-hardened node: $node")

    fromInt(node.index)
  }

}
