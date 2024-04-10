package org.bitcoins.testkitcore.dlc

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.transaction.Transaction
import org.scalatest.{Assertion, Assertions}
import org.bitcoins.core.currency.currencyUnitNumeric

object DLCFeeTestUtil extends Assertions {

  def validateFees(
      builder: DLCTxBuilder,
      fundingTx: Transaction,
      closingTx: Transaction,
      fundingTxSigs: Int = 2,
      closingTxSigs: Int = 2): Assertion = {
    val feeRate = builder.feeRate
    val finalizer = builder.fundingTxFinalizer
    val expectedFundingFee =
      finalizer.offerFundingFee + finalizer.acceptFundingFee
    val expectedClosingFee =
      finalizer.offerFutureFee + finalizer.acceptFutureFee

    val fundingInput =
      (builder.offerFundingInputs ++ builder.acceptFundingInputs)
        .map(_.output.value)
        .sum
    val fundingOutput = fundingTx.outputs.map(_.value).sum(CurrencyUnits)
    val actualFundingFee = fundingInput - fundingOutput

    val closingInput = fundingTx.outputs(builder.fundOutputIndex).value
    val closingOutput = closingTx.outputs.map(_.value).sum(CurrencyUnits)
    val actualClosingFee = closingInput - closingOutput

    /** Actual Fee Rate = Actual Fee / Ceil(Actual Weight / 4.0)
      * Expected Fee Rate = Expected Fee / Ceil(Expected Weight / 4.0)
      *
      * Expected Fee = Actual Fee (yay!)
      *
      * Expected Weight - #sigs - 4 <= Actual Weight <= Expected Weight
      * The right comparison is true because Expected Weight is designed to be an upper bound.
      * The left comparison is true because the possible savings are from one weight being saved per
      * signature, and 1 vbyte = 4 weight being saved if rounding works out well between both parites.
      *
      * Because of these comparisons, we can derive
      *
      * Lower Bound Fee Rate = Actual Fee / (Ceil((Actual Weight + #sigs)/4.0) + 1)
      * Upper Bound Fee Rate = Actual Fee / Ceil(Actual Weight/4.0)
      *
      * So that these two fee rates correspond to vbyte amounts 1 apart and represent the
      * actual fee rate but allowing for signature size variation after which we should match
      * the expected fee rate. This function asserts:
      * Lower Bound Fee Rate <= Expected Fee Rate <= Upper Bound Fee Rate
      */
    def feeRateBounds(
        tx: Transaction,
        actualFee: CurrencyUnit,
        numSignatures: Int,
        missingOutputBytes: Long = 0): (Double, Double) = {
      val vbytesLower =
        Math.ceil(tx.weight / 4.0) + missingOutputBytes
      val vbytesUpper =
        Math.ceil((tx.weight + numSignatures) / 4.0) + missingOutputBytes + 1

      // Upper VBytes => Lower fee rate
      val lowerBound = actualFee.satoshis.toLong / vbytesUpper
      // Lower VBytes => Upper fee rate
      val upperBound = actualFee.satoshis.toLong / vbytesLower
      (lowerBound, upperBound)
    }

    val (actualFundingFeeRateLower, actualFundingFeeRateUpper) =
      feeRateBounds(fundingTx, actualFundingFee, fundingTxSigs)

    val (actualClosingFeeRateLower, actualClosingFeeRateUpper) =
      feeRateBounds(closingTx, actualClosingFee, closingTxSigs)

    assert(actualFundingFee == expectedFundingFee)
    assert(actualClosingFee == expectedClosingFee)

    val offerOutputBytes =
      9 + builder.offerFinalAddress.scriptPubKey.asmBytes.length
    val acceptOutputBytes =
      9 + builder.acceptFinalAddress.scriptPubKey.asmBytes.length

    val (actualClosingFeeRateWithoutOfferLower,
         actualClosingFeeRateWithoutOfferUpper) =
      feeRateBounds(closingTx,
                    actualClosingFee,
                    closingTxSigs,
                    offerOutputBytes)

    val (actualClosingFeeRateWithoutAcceptLower,
         actualClosingFeeRateWithoutAcceptUpper) =
      feeRateBounds(closingTx,
                    actualClosingFee,
                    closingTxSigs,
                    acceptOutputBytes)

    def feeRateBetweenBounds(
        lowerBound: Double,
        upperBound: Double): Boolean = {
      feeRate.toLong >= lowerBound - 10 && feeRate.toLong <= upperBound + 10
    }

    assert(
      feeRateBetweenBounds(actualFundingFeeRateLower,
                           actualFundingFeeRateUpper))
    assert(
      feeRateBetweenBounds(actualClosingFeeRateLower,
                           actualClosingFeeRateUpper) ||
        feeRateBetweenBounds(actualClosingFeeRateWithoutOfferLower,
                             actualClosingFeeRateWithoutOfferUpper) ||
        feeRateBetweenBounds(actualClosingFeeRateWithoutAcceptLower,
                             actualClosingFeeRateWithoutAcceptUpper))
  }
}
