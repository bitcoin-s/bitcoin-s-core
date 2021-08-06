package org.bitcoins.core.protocol.dlc.sign

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  TaprootSerializationOptions,
  TransactionSignatureCreator,
  TransactionSignatureSerializer
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.{FutureUtil, Indexed}
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.{HashType, _}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Responsible for constructing all DLC signatures
  * and signed transactions
  */
case class DLCTxSigner(
    builder: DLCTxBuilder,
    isInitiator: Boolean,
    fundingKey: AdaptorSign,
    finalAddress: BitcoinAddress,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]) {

  private val offer = builder.offer
  private val accept = builder.accept

  private val remoteFundingPubKey = if (isInitiator) {
    accept.pubKeys.fundingKey
  } else {
    offer.pubKeys.fundingKey
  }

  if (isInitiator) {
    require(
      fundingKey.publicKey == offer.pubKeys.fundingKey,
      s"Given keys do not match public key and address in offer, funding.publicKey=${fundingKey.publicKey} offer.pubKeys.fundingKey=${offer.pubKeys.fundingKey}"
    )
    require(
      finalAddress == offer.pubKeys.payoutAddress,
      s"Final address and offerer payout address must be identical finalAddress=$finalAddress offer.pubKeys.payoutAddress=${offer.pubKeys.payoutAddress}"
    )
    val fundingUtxosAsInputs =
      fundingUtxos
        .sortBy(_.outPoint.bytes)
        .zip(offer.fundingInputs.sortBy(_.outPoint.bytes))
        .map { case (utxo, fund) =>
          DLCFundingInput.fromInputSigningInfo(utxo,
                                               fund.inputSerialId,
                                               fund.sequence)
        }
        .sortBy(_.inputSerialId)
    val sortedOfferInputs = offer.fundingInputs.sortBy(_.inputSerialId)

    require(
      fundingUtxosAsInputs == sortedOfferInputs,
      s"Funding ScriptSignatureParams did not match offer funding inputs, fundingUtxosAsInputs=${fundingUtxosAsInputs} sortedOffererInputs=$sortedOfferInputs"
    )
  } else {
    require(
      fundingKey.publicKey == accept.pubKeys.fundingKey,
      "Given keys do not match public key and address in accept"
    )
    require(
      finalAddress == accept.pubKeys.payoutAddress,
      s"Final address and acceptor payout address don't match, finalAddress=$finalAddress accept.pubKeys.payoutAddress=${accept.pubKeys.payoutAddress}"
    )
    val fundingUtxosAsInputs =
      fundingUtxos
        .sortBy(_.outPoint.bytes)
        .zip(accept.fundingInputs.sortBy(_.outPoint.bytes))
        .map { case (utxo, fund) =>
          DLCFundingInput.fromInputSigningInfo(utxo,
                                               fund.inputSerialId,
                                               fund.sequence)
        }
        .sortBy(_.inputSerialId)
    val sortedAcceptInputs = accept.fundingInputs.sortBy(_.inputSerialId)
    require(
      fundingUtxosAsInputs == sortedAcceptInputs,
      s"Funding ScriptSignatureParams did not match offer funding inputs, fundingUtxosAsInputs=${fundingUtxosAsInputs} sortedAcceptInputs=$sortedAcceptInputs"
    )
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sigs: Vector[OracleSignatures]): CurrencyUnit = {
    val (offerPayout, acceptPayout) = builder.getPayouts(sigs)
    if (isInitiator) {
      offerPayout
    } else {
      acceptPayout
    }
  }

  /** Creates this party's FundingSignatures */
  def signFundingTx(): Try[FundingSignatures] = {
    val fundingInputs =
      if (isInitiator) builder.offerFundingInputs
      else builder.acceptFundingInputs

    val utxos =
      fundingUtxos
        .zip(fundingInputs)
        .map { case (utxo, fundingInput) =>
          SpendingInfoWithSerialId(utxo, fundingInput.inputSerialId)
        }
        .sortBy(_.serialId)

    DLCTxSigner.signFundingTx(builder.buildFundingTx, utxos)
  }

  /** Constructs the signed DLC funding transaction given remote FundingSignatures */
  def completeFundingTx(remoteSigs: FundingSignatures): Try[Transaction] = {
    signFundingTx().flatMap { localSigs =>
      DLCTxSigner.completeFundingTx(localSigs,
                                    remoteSigs,
                                    offer.fundingInputs,
                                    accept.fundingInputs,
                                    builder.buildFundingTx)
    }
  }

  private var _cetSigningInfo: Option[ECSignatureParams[P2WSHV0InputInfo]] =
    None

  private def cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo] = {
    _cetSigningInfo match {
      case Some(info) => info
      case None =>
        val signingInfo =
          DLCTxSigner.buildCETSigningInfo(builder.fundOutputIndex,
                                          builder.buildFundingTx,
                                          builder.fundingMultiSig,
                                          fundingKey)

        _cetSigningInfo = Some(signingInfo)

        signingInfo
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcome */
  def signCET(adaptorPoint: ECPublicKey, index: Int): ECAdaptorSignature = {
    signCETs(Vector(Indexed(adaptorPoint, index))).head._2
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes */
  def buildAndSignCETs(adaptorPoints: Vector[Indexed[ECPublicKey]]): Vector[
    (ECPublicKey, WitnessTransaction, ECAdaptorSignature)] = {
    val outcomesAndCETs = builder.buildCETsMap(adaptorPoints)
    DLCTxSigner.buildAndSignCETs(outcomesAndCETs, cetSigningInfo, fundingKey)
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes */
  def signCETs(adaptorPoints: Vector[Indexed[ECPublicKey]]): Vector[
    (ECPublicKey, ECAdaptorSignature)] = {
    buildAndSignCETs(adaptorPoints).map { case (outcome, _, sig) =>
      outcome -> sig
    }
  }

  /** Signs remote's Contract Execution Transaction (CET) for a given outcomes and their corresponding CETs */
  def signGivenCETs(outcomesAndCETs: Vector[AdaptorPointCETPair]): Vector[
    (ECPublicKey, ECAdaptorSignature)] = {
    DLCTxSigner.signCETs(outcomesAndCETs, cetSigningInfo, fundingKey)
  }

  def completeCET(
      outcome: OracleOutcome,
      remoteAdaptorSig: ECAdaptorSignature,
      oracleSigs: Vector[OracleSignatures]): WitnessTransaction = {
    val index = builder.contractInfo.allOutcomes.indexOf(outcome)

    DLCTxSigner.completeCET(
      outcome,
      cetSigningInfo,
      builder.fundingMultiSig,
      builder.buildFundingTx,
      builder.buildCET(outcome.sigPoint, index),
      remoteAdaptorSig,
      remoteFundingPubKey,
      oracleSigs
    )
  }

  /** Creates this party's signature of the refund transaction */
  lazy val signRefundTx: PartialSignature = {
    DLCTxSigner.signRefundTx(cetSigningInfo, builder.buildRefundTx)
  }

  /** Constructs the signed refund transaction given remote's signature */
  def completeRefundTx(remoteSig: PartialSignature): WitnessTransaction = {
    val localSig = signRefundTx

    DLCTxSigner.completeRefundTx(localSig,
                                 remoteSig,
                                 builder.fundingMultiSig,
                                 builder.buildFundingTx,
                                 builder.buildRefundTx)
  }

  /** Creates all of this party's CETSignatures */
  def createCETSigs(): CETSignatures = {
    val adaptorPoints = builder.contractInfo.adaptorPointsIndexed
    val cetSigs = signCETs(adaptorPoints)

    CETSignatures(cetSigs)
  }

  /** Creates CET signatures async */
  def createCETSigsAsync()(implicit
      ec: ExecutionContext): Future[CETSignatures] = {
    val adaptorPoints = builder.contractInfo.adaptorPointsIndexed
    //divide and conquer

    //we want a batch size of at least 1
    val size =
      Math.max(adaptorPoints.length / Runtime.getRuntime.availableProcessors(),
               1)

    val computeBatchFn: Vector[Indexed[ECPublicKey]] => Future[
      Vector[(ECPublicKey, ECAdaptorSignature)]] = {
      (adaptorPoints: Vector[Indexed[ECPublicKey]]) =>
        FutureUtil.makeAsync(() => signCETs(adaptorPoints))
    }

    val cetSigsF: Future[Vector[(ECPublicKey, ECAdaptorSignature)]] = {
      FutureUtil.batchAndParallelExecute(elements = adaptorPoints,
                                         f = computeBatchFn,
                                         batchSize = size)
    }.map(_.flatten)

    for {
      cetSigs <- cetSigsF
    } yield CETSignatures(cetSigs)
  }

  /** Creates all of this party's CETSignatures */
  def createCETsAndCETSigs(): (CETSignatures, Vector[WitnessTransaction]) = {
    val adaptorPoints = builder.contractInfo.adaptorPointsIndexed
    val cetsAndSigs = buildAndSignCETs(adaptorPoints)
    val (msgs, cets, sigs) = cetsAndSigs.unzip3

    (CETSignatures(msgs.zip(sigs)), cets)
  }

  /** The equivalent of [[createCETsAndCETSigs()]] but async */
  def createCETsAndCETSigsAsync()(implicit
  ec: ExecutionContext): Future[(CETSignatures, Vector[WitnessTransaction])] = {
    val adaptorPoints = builder.contractInfo.adaptorPointsIndexed
    val fn = { (adaptorPoints: Vector[Indexed[ECPublicKey]]) =>
      FutureUtil.makeAsync(() => buildAndSignCETs(adaptorPoints))
    }
    val cetsAndSigsF: Future[
      Vector[Vector[(ECPublicKey, WitnessTransaction, ECAdaptorSignature)]]] = {
      FutureUtil.batchAndParallelExecute[Indexed[ECPublicKey],
                                         Vector[(
                                             ECPublicKey,
                                             WitnessTransaction,
                                             ECAdaptorSignature)]](
        elements = adaptorPoints,
        f = fn)
    }

    for {
      cetsAndSigsNested <- cetsAndSigsF
      cetsAndSigs = cetsAndSigsNested.flatten
      (msgs, cets, sigs) = cetsAndSigs.unzip3
    } yield (CETSignatures(msgs.zip(sigs)), cets)
  }

  /** Creates this party's CETSignatures given the outcomes and their unsigned CETs */
  def createCETSigs(
      outcomesAndCETs: Vector[AdaptorPointCETPair]): CETSignatures = {
    val cetSigs = signGivenCETs(outcomesAndCETs)

    CETSignatures(cetSigs)
  }
}

object DLCTxSigner {

  def apply(
      builder: DLCTxBuilder,
      isInitiator: Boolean,
      fundingKey: AdaptorSign,
      payoutPrivKey: AdaptorSign,
      network: BitcoinNetwork,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]): DLCTxSigner = {
    val payoutAddr =
      Bech32Address(P2WPKHWitnessSPKV0(payoutPrivKey.publicKey), network)
    DLCTxSigner(builder, isInitiator, fundingKey, payoutAddr, fundingUtxos)
  }

  def buildCETSigningInfo(
      fundOutputIndex: Int,
      fundingTx: Transaction,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingKey: Sign): ECSignatureParams[P2WSHV0InputInfo] = {
    val fundingOutPoint =
      TransactionOutPoint(fundingTx.txId, UInt32(fundOutputIndex))

    ECSignatureParams(
      P2WSHV0InputInfo(
        outPoint = fundingOutPoint,
        amount = fundingTx.outputs(fundOutputIndex).value,
        scriptWitness = P2WSHWitnessV0(fundingMultiSig),
        conditionalPath = ConditionalPath.NoCondition
      ),
      fundingTx,
      fundingKey,
      HashType.sigHashAll
    )
  }

  def signCET(
      sigPoint: ECPublicKey,
      cet: WitnessTransaction,
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: AdaptorSign): ECAdaptorSignature = {
    signCETs(Vector(AdaptorPointCETPair(sigPoint, cet)),
             cetSigningInfo,
             fundingKey).head._2
  }

  def signCETs(
      outcomesAndCETs: Vector[AdaptorPointCETPair],
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: AdaptorSign): Vector[(ECPublicKey, ECAdaptorSignature)] = {
    buildAndSignCETs(outcomesAndCETs, cetSigningInfo, fundingKey).map {
      case (outcome, _, sig) => outcome -> sig
    }
  }

  def buildAndSignCETs(
      outcomesAndCETs: Vector[AdaptorPointCETPair],
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingKey: AdaptorSign): Vector[
    (ECPublicKey, WitnessTransaction, ECAdaptorSignature)] = {
    outcomesAndCETs.map { case AdaptorPointCETPair(sigPoint, cet) =>
      val hashToSign =
        TransactionSignatureSerializer.hashForSignature(
          cet,
          cetSigningInfo,
          HashType.sigHashAll,
          taprootOptions = TaprootSerializationOptions.empty)

      val adaptorSig = fundingKey.adaptorSign(sigPoint, hashToSign.bytes)
      (sigPoint, cet, adaptorSig)
    }
  }

  // TODO: Without PSBTs
  def completeCET(
      outcome: OracleOutcome,
      cetSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingTx: Transaction,
      ucet: WitnessTransaction,
      remoteAdaptorSig: ECAdaptorSignature,
      remoteFundingPubKey: ECPublicKey,
      oracleSigs: Vector[OracleSignatures]): WitnessTransaction = {
    val signLowR: ByteVector => ECDigitalSignature =
      cetSigningInfo.signer.signLowR(_: ByteVector)
    val localSig = TransactionSignatureCreator.createSig(ucet,
                                                         cetSigningInfo,
                                                         signLowR,
                                                         HashType.sigHashAll)
    val oracleSigSum =
      OracleSignatures.computeAggregateSignature(outcome, oracleSigs)

    val remoteSig =
      oracleSigSum
        .completeAdaptorSignature(remoteAdaptorSig, HashType.sigHashAll.byte)

    val localParitalSig =
      PartialSignature(cetSigningInfo.signer.publicKey, localSig)
    val remotePartialSig = PartialSignature(remoteFundingPubKey, remoteSig)

    val psbt =
      PSBT
        .fromUnsignedTx(ucet)
        .addUTXOToInput(fundingTx, index = 0)
        .addScriptWitnessToInput(P2WSHWitnessV0(fundingMultiSig), index = 0)
        .addSignature(localParitalSig, inputIndex = 0)
        .addSignature(remotePartialSig, inputIndex = 0)

    val cetT = psbt.finalizePSBT
      .flatMap(_.extractTransactionAndValidate)
      .map(_.asInstanceOf[WitnessTransaction])

    cetT match {
      case Success(cet) => cet
      case Failure(err) => throw err
    }
  }

  def signRefundTx(
      refundSigningInfo: ECSignatureParams[P2WSHV0InputInfo],
      refundTx: WitnessTransaction
  ): PartialSignature = {
    val fundingPubKey = refundSigningInfo.signer.publicKey

    val signLowR: ByteVector => ECDigitalSignature =
      refundSigningInfo.signer.signLowR(_: ByteVector)
    val sig = TransactionSignatureCreator.createSig(refundTx,
                                                    refundSigningInfo,
                                                    signLowR,
                                                    HashType.sigHashAll)

    PartialSignature(fundingPubKey, sig)
  }

  // TODO: Without PSBTs
  def completeRefundTx(
      localSig: PartialSignature,
      remoteSig: PartialSignature,
      fundingMultiSig: MultiSignatureScriptPubKey,
      fundingTx: Transaction,
      uRefundTx: WitnessTransaction): WitnessTransaction = {
    val psbt = PSBT
      .fromUnsignedTx(uRefundTx)
      .addUTXOToInput(fundingTx, index = 0)
      .addScriptWitnessToInput(P2WSHWitnessV0(fundingMultiSig), index = 0)
      .addSignature(localSig, inputIndex = 0)
      .addSignature(remoteSig, inputIndex = 0)

    val refundTxT = psbt.finalizePSBT
      .flatMap(_.extractTransactionAndValidate)
      .map(_.asInstanceOf[WitnessTransaction])

    refundTxT match {
      case Success(refundTx) => refundTx
      case Failure(err)      => throw err
    }
  }

  def signFundingTx(
      fundingTx: Transaction,
      fundingUtxos: Vector[SpendingInfoWithSerialId]
  ): Try[FundingSignatures] = {
    val sigsT = fundingUtxos
      .foldLeft[Try[Vector[(TransactionOutPoint, ScriptWitnessV0)]]](
        Success(Vector.empty)) {
        case (sigsT, SpendingInfoWithSerialId(utxo, _)) =>
          sigsT.flatMap { sigs =>
            val sigComponent =
              BitcoinSigner.sign(utxo, fundingTx, isDummySignature = false)
            val witnessT =
              sigComponent.transaction match {
                case wtx: WitnessTransaction =>
                  val witness = wtx.witness(sigComponent.inputIndex.toInt)
                  if (witness == EmptyScriptWitness) {
                    Failure(
                      new RuntimeException(
                        s"Funding Inputs must be SegWit: $utxo"))
                  } else {
                    Success(witness)
                  }
                case _: NonWitnessTransaction =>
                  Failure(
                    new RuntimeException(
                      s"Funding Inputs must be SegWit: $utxo"))
              }

            witnessT.flatMap {
              case witness: ScriptWitnessV0 =>
                Success(sigs.:+((utxo.outPoint, witness)))
              case witness: ScriptWitness =>
                Failure(
                  new RuntimeException(
                    s"Unrecognized script witness: $witness"))
            }
          }
      }

    sigsT.map { sigs =>
      val sigsMap = sigs.toMap

      val sigsVec = fundingUtxos.map {
        case SpendingInfoWithSerialId(input, _) =>
          input.outPoint -> sigsMap(input.outPoint)
      }

      FundingSignatures(sigsVec)
    }
  }

  def completeFundingTx(
      localSigs: FundingSignatures,
      remoteSigs: FundingSignatures,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      fundingTx: Transaction): Try[Transaction] = {
    val fundingInputs =
      (offerFundingInputs ++ acceptFundingInputs).sortBy(_.inputSerialId)
    val allSigs = localSigs.merge(remoteSigs)

    val psbt = fundingInputs.zipWithIndex.foldLeft(
      PSBT.fromUnsignedTxWithP2SHScript(fundingTx)) {
      case (psbt, (fundingInput, index)) =>
        val witness = allSigs(fundingInput.outPoint)

        psbt
          .addUTXOToInput(fundingInput.prevTx, index)
          .addFinalizedScriptWitnessToInput(fundingInput.scriptSignature,
                                            witness,
                                            index)
    }

    val finalizedT = if (psbt.isFinalized) {
      Success(psbt)
    } else {
      psbt.finalizePSBT
    }

    finalizedT.flatMap(_.extractTransactionAndValidate)
  }
}
