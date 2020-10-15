package org.bitcoins.dlc.execution

import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, FundingSignatures}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.crypto.SchnorrDigitalSignature
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.sign.DLCTxSigner

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing SetupDLCs and DLCOutcomes */
case class DLCExecutor[Outcome <: DLCOutcomeType](signer: DLCTxSigner[Outcome])(
    implicit ec: ExecutionContext) {
  val builder: DLCTxBuilder[Outcome] = signer.builder
  val isInitiator: Boolean = signer.isInitiator

  /** Constructs the initiator's SetupDLC given the non-initiator's
    * CETSignatures which should arrive in a DLC accept message
    */
  def setupDLCOffer(
      cetSigs: CETSignatures[Outcome]): Future[SetupDLC[Outcome]] = {
    require(isInitiator, "You should call setupDLCAccept")

    setupDLC(cetSigs, None)
  }

  /** Constructs the non-initiator's SetupDLC given the initiator's
    * CETSignatures and FundingSignatures which should arrive in
    * a DLC sign message
    */
  def setupDLCAccept(
      cetSigs: CETSignatures[Outcome],
      fundingSigs: FundingSignatures): Future[SetupDLC[Outcome]] = {
    require(!isInitiator, "You should call setupDLCOffer")

    setupDLC(cetSigs, Some(fundingSigs))
  }

  /** Constructs a SetupDLC given the necessary signature information
    * from the counter-party.
    */
  def setupDLC(
      cetSigs: CETSignatures[Outcome],
      fundingSigsOpt: Option[FundingSignatures]): Future[SetupDLC[Outcome]] = {
    if (!isInitiator) {
      require(fundingSigsOpt.isDefined,
              "Accepting party must provide remote funding signatures")
    }

    val CETSignatures(outcomeSigs, refundSig) = cetSigs
    val cetInfoFs = outcomeSigs.map {
      case (msg, remoteAdaptorSig) =>
        builder.buildCET(msg).map { cet =>
          msg -> CETInfo(cet, remoteAdaptorSig)
        }
    }

    for {
      fundingTx <- {
        fundingSigsOpt match {
          case Some(fundingSigs) => signer.signFundingTx(fundingSigs)
          case None              => builder.buildFundingTx
        }
      }
      cetInfos <- Future.sequence(cetInfoFs)
      refundTx <- signer.signRefundTx(refundSig)
    } yield {
      SetupDLC(fundingTx, cetInfos.toMap, refundTx)
    }
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayouts(sigs: Vector[SchnorrDigitalSignature]): CurrencyUnit = {
    signer.getPayouts(sigs)
  }

  def executeDLC(
      dlcSetup: SetupDLC[Outcome],
      oracleSigs: Vector[SchnorrDigitalSignature]): Future[
    ExecutedDLCOutcome] = {
    val SetupDLC(fundingTx, cetInfos, _) = dlcSetup

    val msgOpt =
      builder.oracleAndContractInfo.allOutcomes.find(msg =>
        builder.oracleAndContractInfo.verifySigs(msg, oracleSigs))
    val (msg, remoteAdaptorSig) = msgOpt match {
      case Some(msg) =>
        val cetInfo = cetInfos(msg)
        (msg, cetInfo.remoteSignature)
      case None =>
        throw new IllegalArgumentException(
          s"Signatures do not correspond to any possible outcome! $oracleSigs")
    }

    signer.signCET(msg, remoteAdaptorSig, oracleSigs).map { cet =>
      ExecutedDLCOutcome(fundingTx, cet)
    }
  }

  def executeRefundDLC(dlcSetup: SetupDLC[Outcome]): RefundDLCOutcome = {
    val SetupDLC(fundingTx, _, refundTx) = dlcSetup
    RefundDLCOutcome(fundingTx, refundTx)
  }
}
