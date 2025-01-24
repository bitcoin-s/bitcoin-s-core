package org.bitcoins.testkitcore.dlc

import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.execution.{
  DLCExecutor,
  ExecutedDLCOutcome,
  RefundDLCOutcome,
  SetupDLC
}
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCAccept
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.dlc.sign.DLCTxSigner
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.DLCOfferTLV
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._

import scala.concurrent.{ExecutionContext, Future}

/** This case class allows for the construction and execution of Discreet Log
  * Contracts between two parties running on this machine (for tests).
  *
  * @param offer
  *   The DLCOffer associated with this DLC
  * @param accept
  *   The DLCAccept (without sigs) associated with this DLC
  * @param isInitiator
  *   True if this client sends the offer message
  * @param fundingPrivKey
  *   This client's funding private key for this event
  * @param payoutPrivKey
  *   This client's payout private key for this event
  * @param fundingUtxos
  *   This client's funding BitcoinUTXOSpendingInfo collection
  */
case class TestDLCClient(
    offer: DLCMessage.DLCOffer,
    accept: DLCMessage.DLCAcceptWithoutSigs,
    isInitiator: Boolean,
    fundingPrivKey: ECPrivateKey,
    payoutPrivKey: ECPrivateKey,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]
)(implicit ec: ExecutionContext) {
  val dlcTxBuilder: DLCTxBuilder = DLCTxBuilder(offer, accept)

  val dlcTxSigner: DLCTxSigner = DLCTxSigner(
    dlcTxBuilder,
    isInitiator,
    fundingPrivKey,
    payoutPrivKey,
    RegTest,
    fundingUtxos
  )

  private val dlcExecutor = DLCExecutor(dlcTxSigner)

  val messages: Vector[OracleOutcome] = offer.contractInfo.allOutcomes

  val timeouts: DLCTimeouts = offer.timeouts

  def fundingTx: Transaction = dlcTxBuilder.buildFundingTx

  lazy val fundingTxIdBE: DoubleSha256DigestBE = fundingTx.txIdBE

  /** Sets up the non-initiator's DLC given functions for sending CETSignatures
    * to the initiator as well as receiving CETSignatures and FundingSignatures
    * from them
    */
  def setupDLCAccept(
      sendSigs: (CETSignatures, PartialSignature[ECDigitalSignature]) => Future[
        Unit],
      getSigs: Future[(CETSignatures,
                       PartialSignature[ECDigitalSignature],
                       FundingSignatures)]
  ): Future[SetupDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    for {
      (remoteCetSigs, cets) <- dlcTxSigner.createCETsAndCETSigsAsync()
      refundSig = dlcTxSigner.signRefundTx
      _ <- sendSigs(remoteCetSigs, refundSig)
      (cetSigs, refundSig, fundingSigs) <- getSigs
      setupDLC <- Future.fromTry {
        dlcExecutor.setupDLCAccept(cetSigs, refundSig, fundingSigs, Some(cets))
      }
    } yield {
      setupDLC
    }
  }

  /** Sets up the initiator's DLC given functions for getting CETSignatures from
    * the non-initiator as well as sending signatures to them, and lastly a
    * Future which will be populated with the broadcasted (or relayed) fully
    * signed funding transaction
    */
  def setupDLCOffer(
      getSigs: Future[(CETSignatures, PartialSignature[ECDigitalSignature])],
      sendSigs: (
          CETSignatures,
          PartialSignature[ECDigitalSignature],
          FundingSignatures
      ) => Future[Unit],
      getFundingTx: Future[Transaction]
  ): Future[SetupDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    for {
      (cetSigs, refundSig) <- getSigs
      setupDLCWithoutFundingTxSigs <- Future.fromTry {
        dlcExecutor.setupDLCOffer(cetSigs, refundSig)
      }
      cetSigs =
        dlcTxSigner.createCETSigs(setupDLCWithoutFundingTxSigs.cets.map {
          case (msg, info) => AdaptorPointCETPair(msg, info.tx)
        })
      localFundingSigs <- Future.fromTry {
        dlcTxSigner.signFundingTx()
      }
      _ <- sendSigs(cetSigs, dlcTxSigner.signRefundTx, localFundingSigs)
      fundingTx <- getFundingTx
    } yield {
      setupDLCWithoutFundingTxSigs.copy(fundingTx = fundingTx)
    }
  }

  def executeDLC(
      dlcSetup: SetupDLC,
      oracleSigsF: Future[Vector[OracleSignatures]]
  ): Future[ExecutedDLCOutcome] = {
    oracleSigsF.map { oracleSigs =>
      dlcExecutor.executeDLC(dlcSetup, oracleSigs)
    }
  }

  def executeRefundDLC(dlcSetup: SetupDLC): RefundDLCOutcome = {
    dlcExecutor.executeRefundDLC(dlcSetup)
  }
}

object TestDLCClient {

  def apply(
      outcomes: ContractInfo,
      isInitiator: Boolean,
      fundingPrivKey: ECPrivateKey,
      payoutPrivKey: ECPrivateKey,
      payoutSerialId: UInt64,
      remotePubKeys: DLCPublicKeys,
      remotePayoutSerialId: UInt64,
      input: CurrencyUnit,
      remoteInput: CurrencyUnit,
      fundingUtxos: Vector[SpendingInfoWithSerialId],
      fundingInputs: Vector[DLCFundingInput],
      remoteFundingInputs: Vector[DLCFundingInput],
      timeouts: DLCTimeouts,
      feeRate: SatoshisPerVirtualByte,
      changeSPK: ScriptPubKey,
      changeSerialId: UInt64,
      remoteChangeSPK: ScriptPubKey,
      remoteChangeSerialId: UInt64,
      fundOutputSerialId: UInt64,
      network: BitcoinNetwork
  )(implicit ec: ExecutionContext): TestDLCClient = {
    val pubKeys = DLCPublicKeys.fromPrivKeys(
      fundingPrivKey,
      payoutPrivKey,
      network
    )

    val remoteOutcomes: ContractInfo = {
      val descriptors =
        outcomes.contractDescriptors.map(_.flip((input + remoteInput).satoshis))

      val contracts = descriptors.zip(outcomes.oracleInfos).map {
        case (descriptor, oracleInfo) =>
          val pair =
            ContractOraclePair.fromDescriptorOracle(descriptor, oracleInfo)
          SingleContractInfo(outcomes.totalCollateral, pair)
      }

      outcomes match {
        case _: SingleContractInfo => contracts.head
        case _: DisjointUnionContractInfo =>
          DisjointUnionContractInfo(contracts)
      }
    }

    val changeAddress = BitcoinAddress.fromScriptPubKey(changeSPK, network)
    val remoteChangeAddress =
      BitcoinAddress.fromScriptPubKey(remoteChangeSPK, network)

    val (
      offerOutcomes,
      offerPubKeys,
      offerPayoutSerialId,
      offerInput,
      offerFundingInputs,
      offerChangeAddress,
      offerChangeSerialId,
      acceptPubKeys,
      acceptPayoutSerialId,
      acceptInput,
      acceptFundingInputs,
      acceptChangeAddress,
      acceptChangeSerialId
    ) = if (isInitiator) {
      (
        outcomes,
        pubKeys,
        payoutSerialId,
        input,
        fundingInputs,
        changeAddress,
        changeSerialId,
        remotePubKeys,
        remotePayoutSerialId,
        remoteInput,
        remoteFundingInputs,
        remoteChangeAddress,
        remoteChangeSerialId
      )
    } else {
      (
        remoteOutcomes,
        remotePubKeys,
        remotePayoutSerialId,
        remoteInput,
        remoteFundingInputs,
        remoteChangeAddress,
        remoteChangeSerialId,
        pubKeys,
        payoutSerialId,
        input,
        fundingInputs,
        changeAddress,
        changeSerialId
      )
    }

    val offer = DLCMessage.DLCOffer(
      protocolVersionOpt = DLCOfferTLV.currentVersionOpt,
      contractInfo = offerOutcomes,
      pubKeys = offerPubKeys,
      collateral = offerInput.satoshis,
      fundingInputs = offerFundingInputs,
      changeAddress = offerChangeAddress,
      payoutSerialId = offerPayoutSerialId,
      changeSerialId = offerChangeSerialId,
      fundOutputSerialId = fundOutputSerialId,
      feeRate = feeRate,
      timeouts = timeouts
    )

    val negotiationFields = offerOutcomes match {
      case _: SingleContractInfo => DLCAccept.NoNegotiationFields
      case DisjointUnionContractInfo(contracts) =>
        DLCAccept.NegotiationFieldsV2(
          contracts.map(_ => DLCAccept.NoNegotiationFields)
        )
    }

    val accept = DLCMessage.DLCAcceptWithoutSigs(
      totalCollateral = acceptInput.satoshis,
      pubKeys = acceptPubKeys,
      fundingInputs = acceptFundingInputs,
      changeAddress = acceptChangeAddress,
      payoutSerialId = acceptPayoutSerialId,
      changeSerialId = acceptChangeSerialId,
      negotiationFields = negotiationFields,
      tempContractId = offer.tempContractId
    )

    TestDLCClient(
      offer,
      accept,
      isInitiator,
      fundingPrivKey,
      payoutPrivKey,
      fundingUtxos.map(_.spendingInfo)
    )
  }
}
