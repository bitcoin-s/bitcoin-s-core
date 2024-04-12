package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc.models.ContractOraclePair._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.OrderedAnnouncements

sealed trait DLCTemplate {
  def totalCollateral: CurrencyUnit

  def oracles: Vector[OracleAnnouncementTLV]

  def oracleThreshold: Int

  def contractDescriptor: NumericContractDescriptor

  def oracleInfo: OracleInfo

  def toContractInfo: ContractInfo

  require(oracleThreshold > 0,
          s"Oracle threshold must be greater than zero, got $oracleThreshold")
  require(oracles.nonEmpty, "Must provide at least one oracle")
  require(
    oracleThreshold <= oracles.size,
    s"Oracle threshold ($oracleThreshold) cannot be greater than number of oracles ${oracles.size}")
}

case class SingleOracleDLCTemplate(
    oracle: OracleAnnouncementTLV,
    totalCollateral: CurrencyUnit,
    contractDescriptor: NumericContractDescriptor
) extends DLCTemplate {
  override val oracles: Vector[OracleAnnouncementTLV] = Vector(oracle)

  override val oracleThreshold: Int = 1

  override val oracleInfo: NumericSingleOracleInfo = NumericSingleOracleInfo(
    oracle)

  override val toContractInfo: ContractInfo = {
    val pair: NumericPair =
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo)
    SingleContractInfo(totalCollateral.satoshis, pair)
  }
}

case class MultiOracleDLCTemplate(
    oracles: Vector[OracleAnnouncementTLV],
    oracleThreshold: Int,
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean,
    totalCollateral: CurrencyUnit,
    contractDescriptor: NumericContractDescriptor
) extends DLCTemplate {

  require(maxErrorExp > 0,
          s"maxErrorExp must be greater than 0, got $maxErrorExp")
  require(minFailExp > 0, s"minFailExp must be greater than 0, got $minFailExp")
  require(
    minFailExp < maxErrorExp,
    s"minFailExp ($minFailExp) must be less than maxErrorExp ($maxErrorExp)")

  override val oracleInfo: NumericMultiOracleInfo =
    NumericMultiOracleInfo(threshold = oracleThreshold,
                           announcements = OrderedAnnouncements(oracles),
                           maxErrorExp = maxErrorExp,
                           minFailExp = minFailExp,
                           maximizeCoverage = maximizeCoverage)

  override val toContractInfo: ContractInfo = {
    val pair: NumericPair =
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo)
    SingleContractInfo(totalCollateral.satoshis, pair)
  }
}

object DLCTemplate {

  /** Verifies that the oracles are using compatible event descriptors */
  private[dlc] def validateMatchingOracleDescriptors(
      oracles: Vector[OracleAnnouncementTLV]
  ): Boolean = {
    oracles.head.eventTLV.eventDescriptor match {
      case EnumEventDescriptorV0TLV(outcomes) =>
        oracles.forall {
          _.eventTLV.eventDescriptor match {
            case enumEvent: EnumEventDescriptorV0TLV =>
              enumEvent.outcomes.sortBy(_.normStr) == outcomes.sortBy(_.normStr)
            case _: DigitDecompositionEventDescriptorV0TLV => false
          }
        }
      case decomp: DigitDecompositionEventDescriptorV0TLV =>
        oracles.forall {
          _.eventTLV.eventDescriptor match {
            case _: EnumEventDescriptorV0TLV => false
            case d: DigitDecompositionEventDescriptorV0TLV =>
              decomp.numDigits == d.numDigits && decomp.base == d.base
          }
        }
    }
  }
}
