package org.bitcoins.core

import scala.math.Ordering
import scala.util.{Failure, Success, Try}

// We extend AnyVal to avoid runtime allocation of new
// objects. See the Scala documentation on value classes
// and universal traits for more:
// https://docs.scala-lang.org/overviews/core/value-classes.html
package object currency {

  /** Provides natural language syntax for bitcoins */
  implicit class BitcoinsInt(private val i: Int) extends AnyVal {
    def bitcoins: Bitcoins = Bitcoins(i)

    def bitcoin: Bitcoins = bitcoins

    def BTC: Bitcoins = bitcoins
  }

  /** Provides natural language syntax for bitcoins */
  implicit class BitcoinsLong(private val i: Long) extends AnyVal {
    def bitcoins: Bitcoins = Bitcoins(i)

    def bitcoin: Bitcoins = bitcoins

    def BTC: Bitcoins = bitcoins
  }

  /** Provides natural language syntax for satoshis */
  implicit class SatoshisInt(private val i: Int) extends AnyVal {
    def satoshis: Satoshis = Satoshis(i)

    def satoshi: Satoshis = satoshis

    def sats: Satoshis = satoshis

    def sat: Satoshis = satoshis
  }

  /** Provides natural language syntax for satoshis */
  implicit class SatoshisLong(private val i: Long) extends AnyVal {
    def satoshis: Satoshis = Satoshis(i)

    def satoshi: Satoshis = satoshis

    def sats: Satoshis = satoshis

    def sat: Satoshis = satoshis
  }

  implicit val currencyUnitOrdering: Ordering[CurrencyUnit] =
    new Ordering[CurrencyUnit] {
      override def compare(x: CurrencyUnit, y: CurrencyUnit): Int = x.compare(y)
    }

  implicit val currencyUnitNumeric: Numeric[CurrencyUnit] =
    new Numeric[CurrencyUnit] {
      override def plus(x: CurrencyUnit, y: CurrencyUnit): CurrencyUnit = x + y

      override def minus(x: CurrencyUnit, y: CurrencyUnit): CurrencyUnit = x - y

      override def times(x: CurrencyUnit, y: CurrencyUnit): CurrencyUnit = x * y

      override def negate(x: CurrencyUnit): CurrencyUnit = -x

      override def fromInt(x: Int): CurrencyUnit = Satoshis(x.toLong)

      override def toInt(x: CurrencyUnit): Int = x.satoshis.toLong.toInt

      override def toLong(x: CurrencyUnit): Long = x.satoshis.toLong

      override def toFloat(x: CurrencyUnit): Float = x.satoshis.toBigInt.toFloat

      override def toDouble(x: CurrencyUnit): Double =
        x.satoshis.toBigInt.toDouble

      override def compare(x: CurrencyUnit, y: CurrencyUnit): Int =
        x.satoshis `compare` y.satoshis

      // Cannot use the override modifier because this method was added in scala version 2.13
      def parseString(str: String): Option[CurrencyUnit] = {
        if (str.isEmpty) {
          None
        } else {
          Try(str.toLong) match {
            case Success(num) => Some(Satoshis(num))
            case Failure(_)   => None
          }
        }
      }
    }

  implicit val satoshisOrdering: Ordering[Satoshis] =
    new Ordering[Satoshis] {
      override def compare(x: Satoshis, y: Satoshis): Int = x.compare(y)
    }
}
