package org.bitcoins.core.gen

import org.bitcoins.core.currency.{ Bitcoins, CurrencyUnit, CurrencyUnits, Satoshis }
import org.bitcoins.core.number.{ Int32, Int64 }
import org.bitcoins.core.protocol.ln._
import org.scalacheck.Gen

/**
 * Created by chris on 6/23/16.
 */
trait CurrencyUnitGenerator {

  def satoshis: Gen[Satoshis] = for {
    int64 <- NumberGenerator.int64s
  } yield Satoshis(int64)

  def bitcoins: Gen[Bitcoins] = for {
    sat <- satoshis
  } yield Bitcoins(sat)

  def currencyUnit: Gen[CurrencyUnit] = Gen.oneOf(satoshis, bitcoins)

  def positiveSatoshis: Gen[Satoshis] = satoshis.suchThat(_ >= CurrencyUnits.zero)

  /**
   * Generates a postiive satoshi value that is 'realistic'. This current 'realistic' range
   * is from 0 to 1,000,000 bitcoin
   */
  def positiveRealistic: Gen[Satoshis] = Gen.choose(0, Bitcoins(1000000).satoshis.toLong).map { n =>
    Satoshis(Int64(n))
  }
}

object CurrencyUnitGenerator extends CurrencyUnitGenerator

trait LnCurrencyUnitGenerator {

  def milliBitcoin: Gen[MilliBitcoins] = for {
    amount <- Gen.choose(MilliBitcoins.min.toLong, MilliBitcoins.max.toLong)
  } yield MilliBitcoins(amount)

  def microBitcoin: Gen[MicroBitcoins] = for {
    amount <- Gen.choose(MicroBitcoins.min.toLong, MicroBitcoins.max.toLong)
  } yield MicroBitcoins(amount)

  def nanoBitcoin: Gen[NanoBitcoins] = for {
    amount <- Gen.choose(NanoBitcoins.min.toLong, NanoBitcoins.max.toLong)
  } yield NanoBitcoins(amount)

  def picoBitcoin: Gen[PicoBitcoins] = for {
    amount <- Gen.choose(PicoBitcoins.min.toLong, PicoBitcoins.max.toLong)
  } yield PicoBitcoins(amount)

  def lnCurrencyUnit: Gen[LnCurrencyUnit] = Gen.oneOf(milliBitcoin, microBitcoin, nanoBitcoin, picoBitcoin)

  def negativeLnCurrencyUnit: Gen[LnCurrencyUnit] = lnCurrencyUnit.suchThat(_ < LnCurrencyUnits.zero)
}

object LnCurrencyUnitGenerator extends LnCurrencyUnitGenerator