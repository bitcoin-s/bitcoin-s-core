package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.{DLCPublicKeys, DLCTimeouts}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.P2PKWithTimeoutScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStampWithFuture}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

import scala.concurrent.{ExecutionContext, Future}

case class DLCTxBuilder(
    offer: DLCOffer,
    accept: DLCAcceptWithoutSigs
)(implicit ec: ExecutionContext) {

  val DLCOffer(
    offerOutcomes: ContractInfo,
    OracleInfo(oraclePubKey: SchnorrPublicKey, preCommittedR: SchnorrNonce),
    DLCPublicKeys(offerFundingKey: ECPublicKey,
                  offerToLocalCETKey: ECPublicKey,
                  offerFinalAddress: BitcoinAddress),
    offerTotalCollateral: Satoshis,
    offerFundingInputs: Vector[OutputReference],
    offerChangeAddress: BitcoinAddress,
    feeRate: SatoshisPerVirtualByte,
    DLCTimeouts(penaltyTimeout: UInt32,
                contractMaturity: BlockStampWithFuture,
                contractTimeout: BlockStampWithFuture)) = offer

  val network: BitcoinNetwork = offerFinalAddress.networkParameters match {
    case network: BitcoinNetwork => network
  }

  val DLCAcceptWithoutSigs(acceptTotalCollateral: Satoshis,
                           DLCPublicKeys(acceptFundingKey: ECPublicKey,
                                         acceptToLocalCETKey: ECPublicKey,
                                         acceptFinalAddress: BitcoinAddress),
                           acceptFundingInputs: Vector[OutputReference],
                           acceptChangeAddress: BitcoinAddress,
                           eventId: Sha256DigestBE) = accept

  val totalInput: CurrencyUnit = offerTotalCollateral + acceptTotalCollateral

  val offerTotalFunding: CurrencyUnit =
    offerFundingInputs.map(_.output.value).sum

  val acceptTotalFunding: CurrencyUnit =
    acceptFundingInputs.map(_.output.value).sum

  require(offer.eventId == eventId,
          "Offer and accept (without sigs) must refer to same event")
  require(acceptFinalAddress.networkParameters == network,
          "Offer and accept (without sigs) must be on the same network")
  require(offerChangeAddress.networkParameters == network,
          "Offer change address must have same network as final address")
  require(acceptChangeAddress.networkParameters == network,
          "Accept change address must have same network as final address")
  require(totalInput >= offerOutcomes.values.max,
          "Total collateral must add up to max winnings")
  require(
    offerTotalFunding >= offerTotalCollateral,
    "Offer funding inputs must add up to at least offer's total collateral")
  require(
    acceptTotalFunding >= acceptTotalCollateral,
    "Accept funding inputs must add up to at least accept's total collateral")

  val acceptOutcomes: ContractInfo = ContractInfo(offerOutcomes.map {
    case (hash, amt) => (hash, (totalInput - amt).satoshis)
  })

  lazy val buildFundingTx: Future[Transaction] = {
    DLCFundingTxBuilder(
      offerFundingKey = offerFundingKey,
      acceptFundingKey = acceptFundingKey,
      feeRate = feeRate,
      offerInput = offerTotalCollateral,
      acceptInput = acceptTotalCollateral,
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeAddress.scriptPubKey,
      acceptChangeSPK = acceptChangeAddress.scriptPubKey
    ).buildFundingTx()
  }

  private lazy val cetBuilderF = {
    for {
      fundingTx <- buildFundingTx
    } yield {
      val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
      val fundingOutputRef =
        OutputReference(fundingOutPoint, fundingTx.outputs.head)

      DLCCETBuilder(
        offerOutcomes = offerOutcomes,
        acceptOutcomes = acceptOutcomes,
        offerFundingKey = offerFundingKey,
        offerToLocalKey = offerToLocalCETKey,
        offerFinalSPK = offerFinalAddress.scriptPubKey,
        acceptFundingKey = acceptFundingKey,
        acceptToLocalKey = acceptToLocalCETKey,
        acceptFinalSPK = acceptFinalAddress.scriptPubKey,
        timeouts = offer.timeouts,
        feeRate = feeRate,
        oracleInfo = offer.oracleInfo,
        fundingOutputRef = fundingOutputRef
      )
    }
  }

  def buildOfferCET(msg: Sha256DigestBE): Future[WitnessTransaction] = {
    for {
      cetBuilder <- cetBuilderF
      cet <- cetBuilder.buildOfferCET(msg)
    } yield cet
  }

  def buildAcceptCET(msg: Sha256DigestBE): Future[WitnessTransaction] = {
    for {
      cetBuilder <- cetBuilderF
      cet <- cetBuilder.buildAcceptCET(msg)
    } yield cet
  }

  def getOfferCETWitness(
      msg: Sha256DigestBE): Future[P2PKWithTimeoutScriptPubKey] = {
    cetBuilderF.map(_.buildOfferToLocalP2PK(msg))
  }

  def getAcceptCETWitness(
      msg: Sha256DigestBE): Future[P2PKWithTimeoutScriptPubKey] = {
    cetBuilderF.map(_.buildAcceptToLocalP2PK(msg))
  }

  lazy val buildRefundTx: Future[WitnessTransaction] = {
    val builderF = for {
      fundingTx <- buildFundingTx
    } yield {
      val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
      val fundingOutputRef =
        OutputReference(fundingOutPoint, fundingTx.outputs.head)

      DLCRefundTxBuilder(
        offerTotalCollateral,
        offerFundingKey,
        offerFinalAddress.scriptPubKey,
        acceptTotalCollateral,
        acceptFundingKey,
        acceptFinalAddress.scriptPubKey,
        fundingOutputRef,
        offer.timeouts,
        feeRate
      )
    }

    builderF.flatMap(_.buildRefundTx())
  }

  def buildMutualCloseTx(sig: SchnorrDigitalSignature): Future[Transaction] = {
    val builderF = for {
      fundingTx <- buildFundingTx
    } yield {
      val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)

      DLCMutualCloseTxBuilder(offer.oracleInfo,
                              offerFinalAddress.scriptPubKey,
                              offerOutcomes,
                              acceptFinalAddress.scriptPubKey,
                              acceptOutcomes,
                              fundingOutPoint)
    }

    builderF.flatMap(_.buildMutualCloseTx(sig))
  }
}

object DLCTxBuilder {

  /** Experimental approx. vbytes for a CET */
  val approxCETVBytes = 190

  /** Experimental approx. vbytes for a closing tx spending ToLocalOutput */
  val approxToLocalClosingVBytes = 122
}
