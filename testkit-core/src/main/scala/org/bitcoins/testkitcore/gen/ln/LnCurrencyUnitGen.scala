package org.bitcoins.testkitcore.gen.ln

import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.currency._
import org.scalacheck.Gen

trait LnCurrencyUnitGen {

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

  def positivePicoBitcoin: Gen[PicoBitcoins] = {
    Gen.choose(0L, PicoBitcoins.max.toLong).map(PicoBitcoins(_))
  }

  def lnCurrencyUnit: Gen[LnCurrencyUnit] =
    Gen.oneOf(milliBitcoin, microBitcoin, nanoBitcoin, picoBitcoin)

  def lnCurrencyUnitOpt: Gen[Option[LnCurrencyUnit]] = {
    Gen.option(lnCurrencyUnit)
  }

  def realisticLnInvoice: Gen[LnCurrencyUnit] = {
    val gen = Gen.choose(0L, LnPolicy.maxAmountMSat.toLong)
    val msat = gen.map(MilliSatoshis(_))
    msat.map(LnCurrencyUnits.fromMSat(_))
  }

  def milliSatoshis: Gen[MilliSatoshis] =
    for {
      i64 <- NumberGenerator.uInt64
    } yield MilliSatoshis(i64.toBigInt)

  def milliSatoshisPair: Gen[(MilliSatoshis, MilliSatoshis)] =
    for {
      first <- milliSatoshis
      second <- milliSatoshis
    } yield (first, second)
}

object LnCurrencyUnitGen extends LnCurrencyUnitGen
