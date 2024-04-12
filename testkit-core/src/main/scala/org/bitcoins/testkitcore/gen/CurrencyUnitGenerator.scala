package org.bitcoins.testkitcore.gen

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.ln.currency._
import org.scalacheck.Gen

trait CurrencyUnitGenerator {

  def satoshis: Gen[Satoshis] =
    for {
      int64 <- NumberGenerator.int64s
    } yield Satoshis(int64)

  def bitcoins: Gen[Bitcoins] =
    for {
      sat <- satoshis
    } yield Bitcoins(sat)

  def currencyUnit: Gen[CurrencyUnit] = Gen.oneOf(satoshis, bitcoins)

  def positiveSatoshis: Gen[Satoshis] =
    Gen.choose(0L, Long.MaxValue).map(Satoshis.apply)

  /** Generates a postiive satoshi value that is 'realistic'. This current 'realistic' range
    * is from 0 to 1,000,000 bitcoin
    */
  def positiveRealistic: Gen[Satoshis] =
    Gen.choose(0L, Bitcoins(1000000).satoshis.toLong).map { case n =>
      Satoshis(n)
    }
}

object CurrencyUnitGenerator extends CurrencyUnitGenerator

trait LnCurrencyUnitGenerator {

  def milliBitcoin: Gen[MilliBitcoins] =
    for {
      amount <- Gen.choose(MilliBitcoins.min.toLong, MilliBitcoins.max.toLong)
    } yield MilliBitcoins(amount)

  def microBitcoin: Gen[MicroBitcoins] =
    for {
      amount <- Gen.choose(MicroBitcoins.min.toLong, MicroBitcoins.max.toLong)
    } yield MicroBitcoins(amount)

  def nanoBitcoin: Gen[NanoBitcoins] =
    for {
      amount <- Gen.choose(NanoBitcoins.min.toLong, NanoBitcoins.max.toLong)
    } yield NanoBitcoins(amount)

  def picoBitcoin: Gen[PicoBitcoins] =
    for {
      amount <- Gen.choose(PicoBitcoins.min.toLong, PicoBitcoins.max.toLong)
    } yield PicoBitcoins(amount)

  def lnCurrencyUnit: Gen[LnCurrencyUnit] =
    Gen.oneOf(milliBitcoin, microBitcoin, nanoBitcoin, picoBitcoin)

  def negativeLnCurrencyUnit: Gen[LnCurrencyUnit] =
    lnCurrencyUnit.suchThat(_ < LnCurrencyUnits.zero)
}

object LnCurrencyUnitGenerator extends LnCurrencyUnitGenerator
