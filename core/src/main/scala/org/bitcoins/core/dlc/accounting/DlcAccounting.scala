package org.bitcoins.core.dlc.accounting

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.crypto.Sha256Digest

case class DlcAccounting(
    dlcId: Sha256Digest,
    myCollateral: CurrencyUnit,
    theirCollateral: CurrencyUnit,
    myPayoutAddress: BitcoinAddress,
    theirPayoutAddress: BitcoinAddress,
    myPayout: CurrencyUnit,
    theirPayout: CurrencyUnit) {

  /** Profit and loss for the DLC
    * @see https://www.investopedia.com/terms/p/plstatement.asp
    */
  val pnl: CurrencyUnit = myPayout - myCollateral

  /** Rate of return for the DLC
    * @see https://www.investopedia.com/terms/r/rateofreturn.asp
    */
  val rateOfReturn: BigDecimal = pnl.toBigDecimal / myCollateral.toBigDecimal

  val rorPrettyPrint: String = {
    RateOfReturnUtil.prettyPrint(rateOfReturn)
  }
}
