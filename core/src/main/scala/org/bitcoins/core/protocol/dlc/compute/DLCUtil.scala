package org.bitcoins.core.protocol.dlc.compute

import org.bitcoins.core.config.{BitcoinNetwork, NetworkParameters}
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd.{BIP32Path, HDChainType}
import org.bitcoins.core.number.UInt16
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs,
  DLCOffer
}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementTLV,
  OracleAttestmentTLV,
  OracleAttestmentV0TLV,
  OracleEventV0TLV
}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.util.sorted.{OrderedAnnouncements}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.util.Try
import org.bitcoins.core.currency.currencyUnitOrdering

object DLCUtil {

  /** @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Protocol.md#definition-of-contract_id
    * @param fundingTxId the id of the transaction that contains the DLC funding output
    * @param outputIdx the index of the output
    * @param tempContractId the temporary contractId in the offer message
    * @return
    */
  def computeContractId(
      fundingTxId: DoubleSha256DigestBE,
      outputIdx: Int,
      tempContractId: Sha256Digest): ByteVector = {
    val u16 = UInt16(outputIdx)
    //we need to pad the u16 due to how xor works in scodec so we don't lose precision
    val padded = ByteVector.fill(30)(0.toByte) ++ u16.bytes
    fundingTxId.bytes
      .xor(tempContractId.bytes)
      .xor(padded)
  }

  /** @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Protocol.md#definition-of-contract_id
    * @param fundingTx the transaction that contains the DLC funding output
    * @param outputIdx the index of the output
    * @param tempContractId the temporary contractId in the offer message
    * @return
    */
  def computeContractId(
      fundingTx: Transaction,
      outputIdx: Int,
      tempContractId: Sha256Digest): ByteVector = {
    computeContractId(fundingTx.txIdBE, outputIdx, tempContractId)
  }

  /** Extracts an adaptor secret from cetSig assuming it is the completion
    * adaptorSig (which it may not be) and returns the oracle signature if
    * and only if adaptorSig does correspond to cetSig.
    *
    * This method is used to search through possible cetSigs until the correct
    * one is found by validating the returned signature.
    *
    * @param adaptorPoint A potential adaptor point that could have been executed for
    * @param adaptorSig The adaptor signature corresponding to outcome
    * @param cetSig The actual signature for local's key found on-chain on a CET
    */
  private def sigFromOutcomeAndSigs(
      adaptorPoint: ECPublicKey,
      adaptorSig: ECAdaptorSignature,
      cetSig: ECDigitalSignature): Try[FieldElement] = {
    // This value is either the oracle signature S value or it is
    // useless garbage, but we don't know in this scope, the caller
    // must do further work to check this.
    Try {
      adaptorPoint
        .extractAdaptorSecret(adaptorSig, ECDigitalSignature(cetSig.bytes.init))
        .fieldElement
    }
  }

  /** Given a [[ECDigitalSignature]] we found on the blockchain, and the set
    * of possible adaptor signatures we have stored locally in our wallet
    * we reverse engineer the actual outcome our counterparty broadcast
    */
  def computeOutcome(
      completedSig: ECDigitalSignature,
      possibleAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)]): Option[
    (FieldElement, ECPublicKey)] = {
    val sigOpt: Option[(ECPublicKey, ECAdaptorSignature)] = {
      possibleAdaptorSigs.find { case (adaptorPoint, adaptorSig) =>
        val possibleOracleSigT =
          sigFromOutcomeAndSigs(adaptorPoint, adaptorSig, completedSig)

        possibleOracleSigT.isSuccess && possibleOracleSigT.get.getPublicKey == adaptorPoint
      }
    }

    sigOpt.map { case (adaptorPoint, adaptorSig) =>
      (sigFromOutcomeAndSigs(adaptorPoint, adaptorSig, completedSig).get,
       adaptorPoint)
    }
  }

  def computeOutcome(
      isInitiator: Boolean,
      offerFundingKey: ECPublicKey,
      acceptFundingKey: ECPublicKey,
      contractInfo: ContractInfo,
      localAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)],
      cet: WitnessTransaction): Option[
    (SchnorrDigitalSignature, OracleOutcome)] = {
    val allAdaptorPoints = contractInfo.adaptorPoints

    val cetSigs = cet.witness.head
      .asInstanceOf[P2WSHWitnessV0]
      .signatures

    require(cetSigs.size == 2,
            s"There must be only 2 signatures, got ${cetSigs.size}")

    val outcomeValues = cet.outputs.map(_.value).sorted
    val totalCollateral = contractInfo.totalCollateral

    val possibleOutcomes: Vector[ECPublicKey] = {
      contractInfo.allOutcomesAndPayouts.zipWithIndex
        .filter { case ((_, amt), _) =>
          val amts = Vector(amt, totalCollateral - amt)
            .filter(_ >= Policy.dustThreshold)
            .sorted

          // Only messages within 1 satoshi of the on-chain CET's value
          // should be considered.
          // Off-by-one is okay because both parties round to the nearest
          // Satoshi for fees and if both round up they could be off-by-one.
          Math.abs(
            (amts.head - outcomeValues.head).satoshis.toLong) <= 1 && Math
            .abs((amts.last - outcomeValues.last).satoshis.toLong) <= 1
        }
        .map { case (_, index) => allAdaptorPoints(index) }
    }

    val (offerCETSig, acceptCETSig) =
      if (offerFundingKey.hex.compareTo(acceptFundingKey.hex) > 0) {
        (cetSigs.last, cetSigs.head)
      } else {
        (cetSigs.head, cetSigs.last)
      }

    val outcomeSigs = localAdaptorSigs.filter { case (outcome, _) =>
      possibleOutcomes.contains(outcome)
    }

    val cetSig = if (isInitiator) {
      acceptCETSig
    } else {
      offerCETSig
    }

    computeOutcome(cetSig, outcomeSigs).map { case (s, adaptorPoint) =>
      val index = allAdaptorPoints.indexOf(adaptorPoint)
      val outcome: OracleOutcome = contractInfo.allOutcomes(index)

      (SchnorrDigitalSignature(outcome.aggregateNonce, s), outcome)
    }
  }

  def calcContractId(offer: DLCOffer, accept: DLCAccept): ByteVector = {
    calcContractId(offer, accept.withoutSigs)
  }

  def calcContractId(
      offer: DLCOffer,
      acceptWithoutSigs: DLCAcceptWithoutSigs): ByteVector = {
    val fundingKeys =
      Vector(offer.pubKeys.fundingKey, acceptWithoutSigs.pubKeys.fundingKey)
    val fundOutputSerialId = offer.fundOutputSerialId
    val offerFundingInputs = offer.fundingInputs
    val acceptFundingInputs = acceptWithoutSigs.fundingInputs
    val offerChangeSPK = offer.changeAddress.scriptPubKey
    val acceptChangeSPK = acceptWithoutSigs.changeAddress.scriptPubKey
    val offerFinalAddressSPK = offer.pubKeys.payoutAddress.scriptPubKey
    val acceptFinalAddressSPK =
      acceptWithoutSigs.pubKeys.payoutAddress.scriptPubKey
    val feeRate = offer.feeRate
    val (_, fundingSPK) = DLCTxBuilder.buildFundingSPKs(fundingKeys)

    val fundingTxFinalizer = DLCTxBuilder.buildFundingTxFinalizer(
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeSPK,
      acceptChangeSPK = acceptChangeSPK,
      offerPayoutSPK = offerFinalAddressSPK,
      acceptPayoutSPK = acceptFinalAddressSPK,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )

    val (fundingTx, fundingOutputIdx) = DLCTxBuilder.buildFundingTransaction(
      offerInput = offer.collateral,
      acceptInput = acceptWithoutSigs.totalCollateral,
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeSPK,
      offerChangeSerialId = offer.changeSerialId,
      acceptChangeSPK = acceptChangeSPK,
      acceptChangeSerialId = acceptWithoutSigs.changeSerialId,
      fundingSPK = fundingSPK,
      fundOutputSerialId = fundOutputSerialId,
      finalizer = fundingTxFinalizer
    )

    computeContractId(fundingTx = fundingTx,
                      outputIdx = fundingOutputIdx,
                      tempContractId = offer.tempContractId)
  }

  /** Checks that the oracles signatures given to us are correct
    * Things we need to check
    * 1. We have all the oracle signatures
    * 2. The oracle signatures are for one of the contracts in the [[ContractInfo]]
    *  @see https://github.com/bitcoin-s/bitcoin-s/issues/4032
    */
  def checkOracleSignaturesAgainstContract(
      contractInfo: ContractInfo,
      oracleSigs: Vector[OracleSignatures]): Boolean = {
    contractInfo match {
      case single: SingleContractInfo =>
        checkSingleContractInfoOracleSigs(single, oracleSigs)
      case disjoint: DisjointUnionContractInfo =>
        //at least one disjoint union contract
        //has to have matching signatures
        disjoint.contracts.exists { case single: SingleContractInfo =>
          checkSingleContractInfoOracleSigs(single, oracleSigs)
        }
    }
  }

  /** Check if the given [[SingleContractInfo]] has one [[OracleSignatures]]
    * matches it inside of oracleSignatures.
    */
  private def checkSingleContractInfoOracleSigs(
      contractInfo: SingleContractInfo,
      oracleSignatures: Vector[OracleSignatures]): Boolean = {
    require(oracleSignatures.nonEmpty, s"Signatures cannot be empty")
    matchOracleSignatures(contractInfo, oracleSignatures).isDefined
  }

  /** Matches a [[SingleContractInfo]] to its oracle's signatures */
  def matchOracleSignatures(
      contractInfo: SingleContractInfo,
      oracleSignatures: Vector[OracleSignatures]): Option[OracleSignatures] = {
    matchOracleSignatures(contractInfo.announcements, oracleSignatures)
  }

  def matchOracleSignatures(
      announcements: Vector[OracleAnnouncementTLV],
      oracleSignatures: Vector[OracleSignatures]): Option[OracleSignatures] = {
    val announcementNonces: Vector[Vector[SchnorrNonce]] = {
      announcements
        .map { ann =>
          ann.eventTLV match {
            case v0: OracleEventV0TLV =>
              v0.nonces
          }
        }
        .map(_.toVector)
    }
    val resultOpt = oracleSignatures.find { case oracleSignature =>
      val oracleSigNonces: Vector[SchnorrNonce] =
        oracleSignature.sigs.map(_.rx)
      announcementNonces.exists(_ == oracleSigNonces)
    }
    resultOpt
  }

  /** Checks to see if the given oracle signatures and announcement have the same nonces */
  private def matchOracleSignaturesForAnnouncements(
      announcement: OracleAnnouncementTLV,
      signature: OracleSignatures): Option[OracleSignatures] = {
    matchOracleSignatures(
      Vector(announcement),
      Vector(signature)
    )
  }

  /** Builds a set of oracle signatures from given announcements
    * and attestations. This method discards attestments
    * that do not have a matching announcement. Those attestments
    * are not included in the returned set of [[OracleSignatures]]
    */
  def buildOracleSignatures(
      announcements: OrderedAnnouncements,
      attestments: Vector[OracleAttestmentTLV]): Vector[OracleSignatures] = {
    val result: Vector[OracleSignatures] = {
      val init = Vector.empty[OracleSignatures]
      attestments
        .foldLeft(init) { (acc, attestment) =>
          val r: Vector[OracleSignatures] = announcements.flatMap { ann =>
            val oracleSig = attestment match {
              case v0: OracleAttestmentV0TLV =>
                OracleSignatures(SingleOracleInfo(ann), v0)
            }
            val isMatch = matchOracleSignaturesForAnnouncements(ann, oracleSig)
            isMatch match {
              case Some(matchedSig) =>
                acc.:+(matchedSig)
              case None =>
                //don't add it, skip it
                acc
            }
          }.toVector
          r
        }
    }

    result
  }

  def buildOracleSignaturesNaive(
      announcements: OrderedAnnouncements,
      attestments: Vector[OracleAttestmentTLV]): Vector[OracleSignatures] = {

    attestments.foldLeft(Vector.empty[OracleSignatures]) { (acc, sig) =>
      // Nonces should be unique so searching for the first nonce should be safe
      val firstNonce = sig match {
        case v0: OracleAttestmentV0TLV =>
          v0.unsortedSignatures.head.rx
      }
      announcements
        .find { ann =>
          ann.eventTLV match {
            case v0: OracleEventV0TLV =>
              v0.nonces.headOption
                .contains(firstNonce)
          }
        } match {
        case Some(announcement) =>
          acc :+ OracleSignatures(SingleOracleInfo(announcement), sig)
        case None =>
          throw new RuntimeException(
            s"Cannot find announcement for associated public key, ${sig.publicKey.hex}")
      }
    }
  }

  /** Calculates a [[DLCPublicKeys]] from the wallet's [[BIP39KeyManager]] */
  def calcDLCPubKeys(
      xpub: ExtPublicKey,
      chainType: HDChainType,
      keyIndex: Int,
      networkParameters: NetworkParameters,
      externalPayoutAddressOpt: Option[BitcoinAddress]): DLCPublicKeys = {
    val chainIndex = chainType.index
    val fundingKey =
      xpub
        .deriveChildPubKey(BIP32Path.fromString(s"m/$chainIndex/$keyIndex"))
        .get
        .key

    networkParameters match {
      case bitcoinNetwork: BitcoinNetwork =>
        externalPayoutAddressOpt match {
          case None =>
            val payoutKey =
              xpub
                .deriveChildPubKey(
                  BIP32Path.fromString(s"m/$chainIndex/${keyIndex + 1}"))
                .get
                .key
            DLCPublicKeys.fromPubKeys(fundingKey, payoutKey, bitcoinNetwork)
          case Some(externalPayoutAddress) =>
            DLCPublicKeys(fundingKey, externalPayoutAddress)
        }
    }
  }
}
