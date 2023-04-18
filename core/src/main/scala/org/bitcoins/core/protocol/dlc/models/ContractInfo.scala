package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.compute.CETCalculator.{
  CETOutcome,
  MultiOracleOutcome
}
import org.bitcoins.core.protocol.dlc.compute.{
  CETCalculator,
  DLCAdaptorPointComputer
}
import org.bitcoins.core.protocol.dlc.models.ContractOraclePair.{
  EnumPair,
  NumericPair
}
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCAccept
import org.bitcoins.core.protocol.tlv.{
  ContractInfoTLV,
  ContractInfoV0TLV,
  ContractInfoV1TLV,
  DLCSerializationVersion,
  OracleAnnouncementTLV,
  TLVDeserializable,
  TLVSerializable,
  UnsignedNumericOutcome
}
import org.bitcoins.core.util.Indexed
import org.bitcoins.crypto.ECPublicKey

import scala.collection.immutable.HashMap

sealed trait ContractInfo extends TLVSerializable[ContractInfoTLV] {

  def contracts: Vector[SingleContractInfo]

  require(contracts.nonEmpty, s"Cannot have empty contract: $this")

  def totalCollateral: Satoshis

  def oracleInfos: Vector[OracleInfo]

  def contractDescriptors: Vector[ContractDescriptor]

  /** Returns the maximum payout the offerer could win from this contract */
  def maxOffererPayout: Satoshis

  /** Computes the CET set and their corresponding payouts using CETCalculator. */
  def allOutcomesAndPayouts: Vector[(OracleOutcome, Satoshis)]

  /** Corresponds with this DLC's CET set */
  lazy val allOutcomes: Vector[OracleOutcome] =
    allOutcomesAndPayouts.map(_._1)

  /** Maps adpator points to their corresponding OracleOutcomes (which correspond to CETs) */
  lazy val sigPointMap: Map[ECPublicKey, OracleOutcome] =
    adaptorPoints.zip(allOutcomes).toMap

  /** Map OracleOutcomes (which correspond to CETs) to their adpator point and payouts */
  lazy val outcomeMap: Map[OracleOutcome, (ECPublicKey, Satoshis, Satoshis)] = {
    val builder =
      HashMap.newBuilder[OracleOutcome, (ECPublicKey, Satoshis, Satoshis)]

    allOutcomesAndPayouts.zip(adaptorPoints).foreach {
      case ((outcome, offerPayout), adaptorPoint) =>
        val acceptPayout = (totalCollateral - offerPayout).satoshis

        builder.+=((outcome, (adaptorPoint, offerPayout, acceptPayout)))
    }

    builder.result()
  }

  lazy val adaptorPoints: Vector[ECPublicKey] = {
    contracts.flatMap(DLCAdaptorPointComputer.computeAdaptorPoints)
  }

  lazy val adaptorPointsIndexed: Vector[Indexed[ECPublicKey]] = Indexed(
    adaptorPoints)

  /** Checks if the given OracleSignatures exactly match the given OracleOutcome.
    *
    * Warning: This will return false if too many OracleSignatures are given.
    *
    * TODO: Needs a lot of optimization
    */
  def verifySigs(
      outcome: OracleOutcome,
      sigs: Vector[OracleSignatures]): Boolean = {
    if (!sigs.map(_.oracle).forall(outcome.oracles.contains)) {
      false
    } else {
      outcome match {
        case EnumOracleOutcome(oracles, enumOutcome) =>
          oracles.foldLeft(true) { case (boolSoFar, oracle) =>
            lazy val sig = sigs.find(_.oracle == oracle)
            boolSoFar && sig.exists(_.verifySignatures(enumOutcome))
          }
        case NumericOracleOutcome(oraclesAndOutcomes) =>
          oraclesAndOutcomes.foldLeft(true) {
            case (boolSoFar, (oracle, digits)) =>
              lazy val sig = sigs.find(_.oracle == oracle)
              boolSoFar && sig.exists(_.verifySignatures(digits))
          }
      }
    }
  }

  /** Searches all possible outcomes for one which corresponds to the given signatures.
    *
    * Warning: This will return false if too many OracleSignatures are given.
    */
  def findOutcome(sigs: Vector[OracleSignatures]): Option[OracleOutcome] = {
    // TODO: Optimize by looking at nonces
    // TODO: Optimize using NumericOracleSignatures.computeOutcome
    allOutcomes.find(verifySigs(_, sigs))
  }

  /** Returns the adaptor point and payouts for a given OracleOutcome */
  def resultOfOutcome(
      outcome: OracleOutcome): (ECPublicKey, Satoshis, Satoshis) = {
    outcomeMap(outcome)
  }

  /** Returns the payouts for the signature as (toOffer, toAccept) */
  def getPayouts(sigs: Vector[OracleSignatures]): (Satoshis, Satoshis) = {
    val outcome = findOutcome(sigs) match {
      case Some(outcome) => outcome
      case None =>
        throw new IllegalArgumentException(
          s"Signatures do not correspond to a possible outcome! $sigs")
    }
    getPayouts(outcome)
  }

  /** Returns the payouts for the outcome as (toOffer, toAccept) */
  def getPayouts(outcome: OracleOutcome): (Satoshis, Satoshis) = {
    val (_, offerOutcome, acceptOutcome) = resultOfOutcome(outcome)

    (offerOutcome, acceptOutcome)
  }

  /** A ContractInfo can be constructed by the offerer, but it will not contain new
    * information which alters the DLC's contract which is received in the accept message.
    *
    * Specifically if the total collateral changes or negotiation fields are relevant.
    *
    * In these cases, this function should be called to update the ContractInfo.
    */
  def updateOnAccept(
      newTotalCollateral: Satoshis,
      negotiationFields: DLCAccept.NegotiationFields): ContractInfo

  def serializationVersion: DLCSerializationVersion = {
    contractDescriptors.head match {
      case _: EnumContractDescriptor =>
        //enum contracts weren't broken by
        //https://github.com/bitcoin-s/bitcoin-s/pull/3854
        DLCSerializationVersion.Beta
      case n: NumericContractDescriptor =>
        n.outcomeValueFunc.serializationVersion
    }
  }
}

object ContractInfo
    extends TLVDeserializable[ContractInfoTLV, ContractInfo](ContractInfoTLV) {

  override def fromTLV(tlv: ContractInfoTLV): ContractInfo = {
    tlv match {
      case tlv: ContractInfoV0TLV => SingleContractInfo.fromTLV(tlv)
      case tlv: ContractInfoV1TLV => DisjointUnionContractInfo.fromTLV(tlv)
    }
  }
}

/** Fully determines a DLC up to public keys and funding UTXOs to be used.
  *
  * Contains all contract and oracle information and provides an external
  * facing interface which should be used in place of directly accessing
  * ContractDescriptors or OracleInfos whenever possible to make future
  * refactoring simpler and to make the code more modular.
  *
  * This class also contains lazy vals for all expensive computations
  * done regarding CETs during DLC setup and execution.
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/a8876ed28ed33d5f7d5104f01aa2a8d80d128460/Messaging.md#the-contract_info-type
  */
case class SingleContractInfo(
    override val totalCollateral: Satoshis,
    contractOraclePair: ContractOraclePair)
    extends ContractInfo
    with TLVSerializable[ContractInfoV0TLV] {

  override def contracts: Vector[SingleContractInfo] = {
    Vector(this)
  }

  def contractDescriptor: ContractDescriptor =
    contractOraclePair.contractDescriptor

  def oracleInfo: OracleInfo = contractOraclePair.oracleInfo

  def announcements: Vector[OracleAnnouncementTLV] = {
    contractOraclePair match {
      case EnumPair(_, oracleInfo) =>
        oracleInfo.singleOracleInfos.map(_.announcement)
      case NumericPair(_, oracleInfo) =>
        oracleInfo.singleOracleInfos.map(_.announcement)
    }
  }

  override def contractDescriptors: Vector[ContractDescriptor] = Vector(
    contractDescriptor)

  override def oracleInfos: Vector[OracleInfo] = Vector(oracleInfo)

  override def toTLV: ContractInfoV0TLV = {
    ContractInfoV0TLV(totalCollateral,
                      contractDescriptor.toTLV,
                      oracleInfo.toTLV)
  }

  /** @inheritdoc */
  override val maxOffererPayout: Satoshis = {
    contractDescriptor match {
      case descriptor: EnumContractDescriptor =>
        descriptor.values.maxBy(_.toLong)
      case _: NumericContractDescriptor =>
        totalCollateral
    }
  }

  /** @inheritdoc */
  override lazy val allOutcomesAndPayouts: Vector[(OracleOutcome, Satoshis)] = {
    contractOraclePair match {
      case ContractOraclePair.EnumPair(descriptor,
                                       single: EnumSingleOracleInfo) =>
        descriptor.keys.map { outcome =>
          // Safe .get because outcome is from descriptor.keys
          val payout = descriptor.find(_._1 == outcome).get._2
          (EnumOracleOutcome(Vector(single), outcome), payout)
        }
      case ContractOraclePair.EnumPair(descriptor: EnumContractDescriptor,
                                       oracleInfo: EnumMultiOracleInfo) =>
        descriptor.keys.flatMap { outcome =>
          // Safe .get because outcome is from descriptor.keys
          val payout = descriptor.find(_._1 == outcome).get._2
          CETCalculator
            .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
            .map { oracles =>
              (EnumOracleOutcome(oracles, outcome), payout)
            }
        }
      case ContractOraclePair.NumericPair(
            descriptor,
            oracleInfo: NumericSingleOracleInfo) =>
        val vec = CETCalculator.computeCETs(base = 2,
                                            descriptor.numDigits,
                                            descriptor.outcomeValueFunc,
                                            totalCollateral,
                                            descriptor.roundingIntervals)

        vec.map { case CETOutcome(digits, amt) =>
          (NumericOracleOutcome(oracleInfo, UnsignedNumericOutcome(digits)),
           amt)
        }
      case ContractOraclePair.NumericPair(
            descriptor,
            oracleInfo: NumericExactMultiOracleInfo) =>
        val vec = CETCalculator.computeCETs(base = 2,
                                            descriptor.numDigits,
                                            descriptor.outcomeValueFunc,
                                            totalCollateral,
                                            descriptor.roundingIntervals)

        CETCalculator
          .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
          .flatMap { oracles =>
            vec.map { case CETOutcome(digits, amt) =>
              val outcome = UnsignedNumericOutcome(digits)
              (NumericOracleOutcome(oracles.map((_, outcome))), amt)
            }
          }
      case ContractOraclePair.NumericPair(descriptor: NumericContractDescriptor,
                                          oracleInfo: NumericMultiOracleInfo) =>
        val vec: Vector[MultiOracleOutcome] =
          CETCalculator.computeMultiOracleCETsBinary(
            descriptor.numDigits,
            descriptor.outcomeValueFunc,
            totalCollateral,
            descriptor.roundingIntervals,
            oracleInfo.maxErrorExp,
            oracleInfo.minFailExp,
            oracleInfo.maximizeCoverage,
            oracleInfo.threshold
          )

        CETCalculator
          .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
          .flatMap { oracles =>
            vec.map { case MultiOracleOutcome(digitsVec, amt) =>
              val outcomesVec =
                digitsVec.map(UnsignedNumericOutcome.apply)
              (NumericOracleOutcome(oracles.zip(outcomesVec)), amt)
            }
          }
    }
  }

  /** @inheritdoc */
  override def updateOnAccept(
      newTotalCollateral: Satoshis,
      negotiationFields: DLCAccept.NegotiationFields): SingleContractInfo = {
    contractOraclePair match {
      case ContractOraclePair.EnumPair(_, _) =>
        if (negotiationFields != DLCAccept.NoNegotiationFields) {
          throw new IllegalArgumentException(
            s"Cannot have rounding intervals for single nonce contract: $negotiationFields")
        }

        if (newTotalCollateral == totalCollateral) {
          this
        } else {
          this.copy(totalCollateral = newTotalCollateral)
        }

      case ContractOraclePair.NumericPair(descriptor, oracleInfo) =>
        val newRoundingIntervals = negotiationFields match {
          case DLCAccept.NegotiationFieldsV1(acceptRoundingIntervals) =>
            descriptor.roundingIntervals.minRoundingWith(
              acceptRoundingIntervals)
          case DLCAccept.NoNegotiationFields => descriptor.roundingIntervals
          case _: DLCAccept.NegotiationFieldsV2 =>
            throw new IllegalArgumentException(
              s"Cannot use disjoint union negotiation fields for a SingleContractInfo, $negotiationFields")
        }

        if (
          newTotalCollateral == totalCollateral && newRoundingIntervals == descriptor.roundingIntervals
        ) {
          this
        } else {
          val newDescriptor =
            descriptor.copy(roundingIntervals = newRoundingIntervals)
          val contractOraclePair =
            ContractOraclePair.NumericPair(newDescriptor, oracleInfo)
          SingleContractInfo(totalCollateral = newTotalCollateral,
                             contractOraclePair = contractOraclePair)
        }
    }
  }
}

object SingleContractInfo
    extends TLVDeserializable[ContractInfoV0TLV, SingleContractInfo](
      ContractInfoV0TLV) {

  lazy val dummy: ContractInfo = fromTLV(ContractInfoV0TLV.dummy)

  override def fromTLV(tlv: ContractInfoV0TLV): SingleContractInfo = {
    val contract = ContractDescriptor.fromTLV(tlv.contractDescriptor)
    val oracleInfo = OracleInfo.fromTLV(tlv.oracleInfo)
    val contractOraclePair =
      ContractOraclePair.fromDescriptorOracle(contract, oracleInfo)
    SingleContractInfo(tlv.totalCollateral, contractOraclePair)
  }

  def apply(
      enumDescriptor: EnumContractDescriptor,
      enumOracleInfo: EnumOracleInfo): SingleContractInfo = {
    val enumPair = ContractOraclePair.EnumPair(enumDescriptor, enumOracleInfo)
    SingleContractInfo(totalCollateral = enumDescriptor.values.maxBy(_.toLong),
                       enumPair)
  }

  def apply(
      totalCollateral: Satoshis,
      contractDescriptor: ContractDescriptor,
      oracleInfo: OracleInfo): SingleContractInfo = {
    SingleContractInfo(
      totalCollateral,
      ContractOraclePair.fromDescriptorOracle(contractDescriptor, oracleInfo))
  }
}

case class DisjointUnionContractInfo(contracts: Vector[SingleContractInfo])
    extends ContractInfo
    with TLVSerializable[ContractInfoV1TLV] {
  require(contracts.nonEmpty,
          s"Cannot have empty contract oracle pairs for ContractInfoV1TLV")

  override val totalCollateral: Satoshis = contracts.head.totalCollateral

  require(contracts.forall(_.totalCollateral == totalCollateral),
          "All contract total collaterals must be equal.")

  override def oracleInfos: Vector[OracleInfo] = contracts.map(_.oracleInfo)

  override def contractDescriptors: Vector[ContractDescriptor] =
    contracts.map(_.contractDescriptor)

  override def toTLV: ContractInfoV1TLV = {
    ContractInfoV1TLV(
      totalCollateral,
      contracts.map(contract =>
        (contract.contractDescriptor.toTLV, contract.oracleInfo.toTLV)))
  }

  /** @inheritdoc */
  override val maxOffererPayout: Satoshis =
    contracts
      .map(_.maxOffererPayout)
      .max(org.bitcoins.core.currency.currencyUnitOrdering)

  /** @inheritdoc */
  override lazy val allOutcomesAndPayouts: Vector[(OracleOutcome, Satoshis)] = {
    contracts.flatMap(_.allOutcomesAndPayouts)
  }

  /** @inheritdoc */
  override def updateOnAccept(
      newTotalCollateral: Satoshis,
      negotiationFields: DLCAccept.NegotiationFields): DisjointUnionContractInfo = {
    negotiationFields match {
      case DLCAccept.NegotiationFieldsV2(nestedNegotiationFields) =>
        require(
          nestedNegotiationFields.length == contracts.length,
          s"Expected ${contracts.length} negotiation fields, got $negotiationFields.")

        val newContracts = contracts.zip(nestedNegotiationFields).map {
          case (contract, negotiationFields) =>
            contract.updateOnAccept(newTotalCollateral, negotiationFields)
        }

        DisjointUnionContractInfo(newContracts)
      case _: DLCAccept.NegotiationFields =>
        throw new IllegalArgumentException(
          s"Required disjoint union negotiation fields for disjoint union contract info, got $negotiationFields")
    }
  }
}

object DisjointUnionContractInfo
    extends TLVDeserializable[ContractInfoV1TLV, DisjointUnionContractInfo](
      ContractInfoV1TLV) {

  override def fromTLV(tlv: ContractInfoV1TLV): DisjointUnionContractInfo = {
    val contracts = tlv.contractOraclePairs.map {
      case (descriptorTLV, oracleTLV) =>
        val contract = ContractDescriptor.fromTLV(descriptorTLV)
        val oracleInfo = OracleInfo.fromTLV(oracleTLV)
        val contractOraclePair =
          ContractOraclePair.fromDescriptorOracle(contract, oracleInfo)

        SingleContractInfo(tlv.totalCollateral, contractOraclePair)
    }

    DisjointUnionContractInfo(contracts)
  }
}
