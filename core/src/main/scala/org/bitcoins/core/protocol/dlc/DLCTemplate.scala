package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc.ContractOraclePair._
import org.bitcoins.core.protocol.tlv._

sealed trait DLCTemplate {
  def totalCollateral: CurrencyUnit

  def oracles: Vector[OracleAnnouncementTLV]

  def oracleThreshold: Int

  def contractDescriptor: NumericContractDescriptor

  def oracleInfo: OracleInfo

  def toContractInfo: ContractInfo
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
    ContractInfo(totalCollateral.satoshis, pair)
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

  override val oracleInfo: NumericMultiOracleInfo =
    NumericMultiOracleInfo(threshold = oracleThreshold,
                           announcements = oracles,
                           maxErrorExp = maxErrorExp,
                           minFailExp = minFailExp,
                           maximizeCoverage = maximizeCoverage)

  override val toContractInfo: ContractInfo = {
    val pair: NumericPair =
      ContractOraclePair.NumericPair(contractDescriptor, oracleInfo)
    ContractInfo(totalCollateral.satoshis, pair)
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
            case enum: EnumEventDescriptorV0TLV =>
              enum.outcomes.sortBy(_.normStr) == outcomes.sortBy(_.normStr)
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
