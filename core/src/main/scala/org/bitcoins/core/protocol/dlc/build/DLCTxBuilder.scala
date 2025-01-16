package org.bitcoins.core.protocol.dlc.build

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number._
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs,
  DLCOffer
}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.util.Indexed
import org.bitcoins.core.wallet.builder.DualFundingTxFinalizer
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  P2WSHV0InputInfo
}
import org.bitcoins.crypto._
import org.bitcoins.core.currency.currencyUnitNumeric
import scodec.bits.ByteVector

case class DLCTxBuilder(offer: DLCOffer, accept: DLCAcceptWithoutSigs) {

  val DLCOffer(
    _,
    _,
    DLCPublicKeys(offerFundingKey: ECPublicKey,
                  offerFinalAddress: BitcoinAddress),
    offerTotalCollateral: Satoshis,
    offerFundingInputs: Vector[DLCFundingInput],
    offerChangeAddress: BitcoinAddress,
    offerPayoutSerialId: UInt64,
    offerChangeSerialId: UInt64,
    fundOutputSerialId: UInt64,
    feeRate: SatoshisPerVirtualByte,
    DLCTimeouts(contractMaturity: BlockTimeStamp,
                contractTimeout: BlockTimeStamp),
    _
  ) = offer

  val network: BitcoinNetwork = offerFinalAddress.networkParameters match {
    case network: BitcoinNetwork => network
  }

  val DLCAcceptWithoutSigs(
    acceptTotalCollateral: Satoshis,
    DLCPublicKeys(acceptFundingKey: ECPublicKey,
                  acceptFinalAddress: BitcoinAddress),
    acceptFundingInputs: Vector[DLCFundingInput],
    acceptChangeAddress: BitcoinAddress,
    acceptPayoutSerialId: UInt64,
    acceptChangeSerialId: UInt64,
    acceptNegotiationFields: DLCAccept.NegotiationFields,
    tempContractId: Sha256Digest
  ) = accept

  val totalInput: CurrencyUnit = offerTotalCollateral + acceptTotalCollateral

  val fundingInputs: Vector[DLCFundingInput] =
    (offerFundingInputs ++ acceptFundingInputs).sortBy(_.inputSerialId)

  require(
    fundingInputs.map(_.inputSerialId).distinct.size == fundingInputs.size,
    s"Must have all unique input serial ids, got ${fundingInputs.map(_.inputSerialId)}"
  )

  require(
    offerPayoutSerialId != acceptPayoutSerialId,
    s"offerPayoutSerialId ($offerPayoutSerialId) cannot equal acceptPayoutSerialId ($acceptPayoutSerialId)")

  require(
    Vector(offerChangeSerialId,
           acceptChangeSerialId,
           fundOutputSerialId).distinct.size == 3,
    s"offerChangeSerialId, acceptChangeSerialId, and fundOutputSerialId must be unique got ${Vector(offerChangeSerialId, acceptChangeSerialId, fundOutputSerialId)}, respectively"
  )

  // builder.offer.oracleAndContractInfo should not be used,
  // builder.oracleAndContractInfo should be used instead in case a party
  // is over-collateralized in which case payouts will be incorrect here.
  private val contractInfoBeforeAccept: ContractInfo =
    offer.contractInfo

  val contractInfo: ContractInfo =
    contractInfoBeforeAccept.updateOnAccept(totalInput.satoshis,
                                            acceptNegotiationFields)

  val offerTotalFunding: CurrencyUnit =
    offerFundingInputs.map(_.output.value).sum

  val acceptTotalFunding: CurrencyUnit =
    acceptFundingInputs.map(_.output.value).sum

  require(
    offer.tempContractId == tempContractId,
    s"Offer and accept (without sigs) must refer to same event, offer.tempContractId=${offer.tempContractId.hex} tempContractId=${tempContractId.hex}"
  )
  require(acceptFinalAddress.networkParameters == network,
          "Offer and accept (without sigs) must be on the same network")
  require(offerChangeAddress.networkParameters == network,
          "Offer change address must have same network as final address")
  require(acceptChangeAddress.networkParameters == network,
          "Accept change address must have same network as final address")
  require(
    totalInput >= contractInfo.totalCollateral,
    s"Total collateral must add up to max winnings, totalInput=${totalInput} contractInfo.totalCollateral=${contractInfo.totalCollateral}"
  )
  require(
    offerTotalFunding >= offerTotalCollateral,
    "Offer funding inputs must add up to at least offer's total collateral")
  require(
    acceptTotalFunding >= acceptTotalCollateral,
    "Accept funding inputs must add up to at least accept's total collateral")

  /** Returns the payouts for the signature as (toOffer, toAccept) */
  def getPayouts(
      oracleSigs: Vector[OracleSignatures]): (CurrencyUnit, CurrencyUnit) = {
    contractInfo.getPayouts(oracleSigs)
  }

  val fundingKeys: Vector[ECPublicKey] =
    Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)

  /** The 2-of-2 MultiSignatureScriptPubKey to be wrapped in P2WSH and used as
    * the funding output, and the funding output's P2WSH(MultiSig) ScriptPubKey
    */
  val (fundingMultiSig: MultiSignatureScriptPubKey,
       fundingSPK: P2WSHWitnessSPKV0) =
    DLCTxBuilder.buildFundingSPKs(fundingKeys)

  lazy val fundingTxFinalizer: DualFundingTxFinalizer = {
    DLCTxBuilder.buildFundingTxFinalizer(
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeAddress.scriptPubKey,
      acceptChangeSPK = acceptChangeAddress.scriptPubKey,
      offerPayoutSPK = offerFinalAddress.scriptPubKey,
      acceptPayoutSPK = acceptFinalAddress.scriptPubKey,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )
  }

  /** Constructs the unsigned funding transaction */
  lazy val buildFundingTx: Transaction = {
    DLCTxBuilder
      .buildFundingTransaction(
        offerInput = offerTotalCollateral,
        acceptInput = acceptTotalCollateral,
        offerFundingInputs = offerFundingInputs,
        acceptFundingInputs = acceptFundingInputs,
        offerChangeSPK = offerChangeAddress.scriptPubKey,
        offerChangeSerialId = offerChangeSerialId,
        acceptChangeSPK = acceptChangeAddress.scriptPubKey,
        acceptChangeSerialId = acceptChangeSerialId,
        fundingSPK = fundingSPK,
        fundOutputSerialId = fundOutputSerialId,
        finalizer = fundingTxFinalizer
      )
      ._1
  }

  // Need to build funding tx so it takes into account the dust threshold
  lazy val fundOutputIndex: Int = {
    buildFundingTx.outputs.zipWithIndex
      .find(_._1.scriptPubKey == fundingSPK)
      .get
      ._2
  }

  private def fundingTx: Transaction = {
    buildFundingTx
  }

  lazy val calcContractId: ByteVector = {
    DLCUtil.computeContractId(fundingTx = fundingTx,
                              outputIdx = fundOutputIndex,
                              tempContractId = accept.tempContractId)
  }

  /** Constructs the unsigned Contract Execution Transaction (CET) for a given
    * outcome hash
    */
  def buildCET(adaptorPoint: Indexed[ECPublicKey]): WitnessTransaction = {
    buildCETs(Vector(adaptorPoint)).head
  }

  def buildCET(adaptorPoint: ECPublicKey, index: Int): WitnessTransaction = {
    buildCET(Indexed(adaptorPoint, index))
  }

  def buildCETsMap(adaptorPoints: Vector[Indexed[ECPublicKey]])
      : Vector[AdaptorPointCETPair] = {
    DLCTxBuilder
      .buildCETs(
        adaptorPoints,
        contractInfo,
        offerFundingKey,
        offerFinalAddress.scriptPubKey,
        offerPayoutSerialId,
        acceptFundingKey,
        acceptFinalAddress.scriptPubKey,
        acceptPayoutSerialId,
        offer.timeouts,
        fundingTx,
        fundOutputIndex
      )
  }

  def buildCETs(adaptorPoints: Vector[Indexed[ECPublicKey]])
      : Vector[WitnessTransaction] = {
    buildCETsMap(adaptorPoints).map(_.wtx)
  }

  /** Constructs the unsigned refund transaction */
  lazy val buildRefundTx: WitnessTransaction = {
    DLCTxBuilder.buildRefundTx(
      offerTotalCollateral,
      offerFundingKey,
      offerFinalAddress.scriptPubKey,
      offerPayoutSerialId,
      acceptTotalCollateral,
      acceptFundingKey,
      acceptFinalAddress.scriptPubKey,
      acceptPayoutSerialId,
      fundingTx,
      fundOutputIndex,
      offer.timeouts
    )
  }
}

object DLCTxBuilder {

  def buildFundingSPKs(fundingPubKeys: Vector[ECPublicKey])
      : (MultiSignatureScriptPubKey, P2WSHWitnessSPKV0) = {
    require(fundingPubKeys.length == 2,
            s"There must be exactly 2 funding keys, got $fundingPubKeys")
    val multiSigSPK =
      MultiSignatureScriptPubKey(2, fundingPubKeys.sortBy(_.hex))
    val p2wshSPK = P2WSHWitnessSPKV0(multiSigSPK)

    (multiSigSPK, p2wshSPK)
  }

  def buildFundingTxFinalizer(
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      offerChangeSPK: ScriptPubKey,
      acceptChangeSPK: ScriptPubKey,
      offerPayoutSPK: ScriptPubKey,
      acceptPayoutSPK: ScriptPubKey,
      feeRate: SatoshisPerVirtualByte,
      fundingSPK: P2WSHWitnessSPKV0): DualFundingTxFinalizer = {
    DualFundingTxFinalizer(
      offerInputs = offerFundingInputs.map(_.toDualFundingInput),
      offerPayoutSPK = offerPayoutSPK,
      offerChangeSPK = offerChangeSPK,
      acceptInputs = acceptFundingInputs.map(_.toDualFundingInput),
      acceptPayoutSPK = acceptPayoutSPK,
      acceptChangeSPK = acceptChangeSPK,
      feeRate = feeRate,
      fundingSPK = fundingSPK
    )
  }

  /** Builds the funding transaction for a DLC
    * @return
    *   the transaction and the output index of the funding output
    */
  def buildFundingTransaction(
      offerInput: CurrencyUnit,
      acceptInput: CurrencyUnit,
      offerFundingInputs: Vector[DLCFundingInput],
      acceptFundingInputs: Vector[DLCFundingInput],
      offerChangeSPK: ScriptPubKey,
      offerChangeSerialId: UInt64,
      acceptChangeSPK: ScriptPubKey,
      acceptChangeSerialId: UInt64,
      fundingSPK: P2WSHWitnessSPKV0,
      fundOutputSerialId: UInt64,
      finalizer: DualFundingTxFinalizer): (Transaction, Int) = {
    // The total collateral of both parties combined
    val totalInput: CurrencyUnit = offerInput + acceptInput

    // The sum of all funding input amounts from the initiator
    val offerTotalFunding: CurrencyUnit =
      offerFundingInputs.map(_.output.value).sum

    // The sum of all funding input amounts from the non-initiator
    val acceptTotalFunding: CurrencyUnit =
      acceptFundingInputs.map(_.output.value).sum

    require(
      offerTotalFunding >= offerInput,
      s"Offer funding inputs must add up to at least offer's total collateral, offerTotalFunding=$offerTotalFunding offerInput=$offerInput"
    )
    require(
      acceptTotalFunding >= acceptInput,
      s"Accept funding inputs must add up to at least accept's total collateral, acceptTotalFunding=$acceptTotalFunding acceptInput=$acceptInput"
    )

    val fundingInputs: Vector[DLCFundingInput] =
      (offerFundingInputs ++ acceptFundingInputs).sortBy(_.inputSerialId)

    val inputs = fundingInputs.map { ref =>
      val scriptSig = ref.redeemScriptOpt match {
        case Some(redeemScript) => P2SHScriptSignature(redeemScript)
        case None               => EmptyScriptSignature
      }

      TransactionInput(ref.outPoint, scriptSig, ref.sequence)
    }

    val fundingValue =
      totalInput + finalizer.offerFutureFee + finalizer.acceptFutureFee
    val offerChangeValue =
      offerTotalFunding - offerInput - finalizer.offerFees
    val acceptChangeValue =
      acceptTotalFunding - acceptInput - finalizer.acceptFees

    val fundingOutput = TransactionOutput(fundingValue, fundingSPK)
    val offererChangeOutput =
      TransactionOutput(offerChangeValue, offerChangeSPK)
    val acceptorChangeOutput =
      TransactionOutput(acceptChangeValue, acceptChangeSPK)
    val outputsWithSerialId = Vector(
      (fundingOutput, fundOutputSerialId),
      (offererChangeOutput, offerChangeSerialId),
      (acceptorChangeOutput, acceptChangeSerialId)
    )

    val outputs = sortAndFilterOutputs(outputsWithSerialId)

    val btx = BaseTransaction(TransactionConstants.validLockVersion,
                              inputs,
                              outputs,
                              UInt32.zero)
    val changeSPKs = Vector(offererChangeOutput.scriptPubKey,
                            acceptorChangeOutput.scriptPubKey)
    val outputIdx = btx.outputs.zipWithIndex
      .filterNot { case (output, _) =>
        changeSPKs.contains(output.scriptPubKey)
      }
      .map(_._2)

    require(outputIdx.length == 1,
            s"Can only have one funding output idx, got=$outputIdx")
    (btx, outputIdx.head)
  }

  def buildCET(
      adaptorPoint: Indexed[ECPublicKey],
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      offerSerialId: UInt64,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      acceptSerialId: UInt64,
      timeouts: DLCTimeouts,
      fundingOutputRef: OutputReference): WitnessTransaction = {
    val cets = buildCETs(
      Vector(adaptorPoint),
      contractInfo,
      offerFundingKey,
      offerFinalSPK,
      offerSerialId,
      acceptFundingKey,
      acceptFinalSPK,
      acceptSerialId,
      timeouts,
      fundingOutputRef
    )
    require(cets.length == 1,
            s"Cannot have more than 1 CET for buildCET, got=${cets.length}")
    cets.head.wtx
  }

  def buildCETs(
      adaptorPoints: Vector[Indexed[ECPublicKey]],
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      offerSerialId: UInt64,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      acceptSerialId: UInt64,
      timeouts: DLCTimeouts,
      fundingOutputRef: OutputReference): Vector[AdaptorPointCETPair] = {
    val builder =
      DLCCETBuilder(contractInfo,
                    offerFundingKey,
                    offerFinalSPK,
                    offerSerialId,
                    acceptFundingKey,
                    acceptFinalSPK,
                    acceptSerialId,
                    timeouts,
                    fundingOutputRef)

    val outcomes = adaptorPoints.map { case Indexed(_, index) =>
      contractInfo.allOutcomes(index)
    }

    adaptorPoints.zip(outcomes).map { case (Indexed(sigPoint, _), outcome) =>
      AdaptorPointCETPair(sigPoint, builder.buildCET(outcome))
    }
  }

  def buildCETs(
      adaptorPoints: Vector[Indexed[ECPublicKey]],
      contractInfo: ContractInfo,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      offerSerialId: UInt64,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      acceptSerialId: UInt64,
      timeouts: DLCTimeouts,
      fundingTx: Transaction,
      fundOutputIndex: Int): Vector[AdaptorPointCETPair] = {
    val fundingOutPoint =
      TransactionOutPoint(fundingTx.txId, UInt32(fundOutputIndex))
    val fundingOutputRef =
      OutputReference(fundingOutPoint, fundingTx.outputs(fundOutputIndex))

    buildCETs(adaptorPoints,
              contractInfo,
              offerFundingKey,
              offerFinalSPK,
              offerSerialId,
              acceptFundingKey,
              acceptFinalSPK,
              acceptSerialId,
              timeouts,
              fundingOutputRef)
  }

  def buildRefundTx(
      offerInput: CurrencyUnit,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      offerSerialId: UInt64,
      acceptInput: CurrencyUnit,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      acceptSerialId: UInt64,
      fundingOutputRef: OutputReference,
      timeouts: DLCTimeouts): WitnessTransaction = {
    val OutputReference(fundingOutPoint, fundingOutput) = fundingOutputRef
    val fundingKeys = Vector(offerFundingKey, acceptFundingKey).sortBy(_.hex)
    val fundingInfo = P2WSHV0InputInfo(
      outPoint = fundingOutPoint,
      amount = fundingOutput.value,
      scriptWitness =
        P2WSHWitnessV0(MultiSignatureScriptPubKey(2, fundingKeys)),
      conditionalPath = ConditionalPath.NoCondition
    )

    val fundingInput = TransactionInput(fundingOutPoint,
                                        EmptyScriptSignature,
                                        TransactionConstants.disableRBFSequence)

    val outputsWithSerialId =
      Vector((TransactionOutput(offerInput, offerFinalSPK), offerSerialId),
             (TransactionOutput(acceptInput, acceptFinalSPK), acceptSerialId))

    val outputs = sortAndFilterOutputs(outputsWithSerialId)

    val witness = TransactionWitness.fromWitOpt(
      Vector(InputInfo.getScriptWitness(fundingInfo))
    )

    WitnessTransaction(
      TransactionConstants.validLockVersion,
      Vector(fundingInput),
      outputs,
      timeouts.contractTimeout.toUInt32,
      witness
    )
  }

  def buildRefundTx(
      offerInput: CurrencyUnit,
      offerFundingKey: ECPublicKey,
      offerFinalSPK: ScriptPubKey,
      offerSerialId: UInt64,
      acceptInput: CurrencyUnit,
      acceptFundingKey: ECPublicKey,
      acceptFinalSPK: ScriptPubKey,
      acceptSerialId: UInt64,
      fundingTx: Transaction,
      fundOutputIndex: Int,
      timeouts: DLCTimeouts): WitnessTransaction = {
    val fundingOutPoint =
      TransactionOutPoint(fundingTx.txId, UInt32(fundOutputIndex))
    val fundingOutputRef =
      OutputReference(fundingOutPoint, fundingTx.outputs(fundOutputIndex))

    buildRefundTx(offerInput,
                  offerFundingKey,
                  offerFinalSPK,
                  offerSerialId,
                  acceptInput,
                  acceptFundingKey,
                  acceptFinalSPK,
                  acceptSerialId,
                  fundingOutputRef,
                  timeouts)
  }

  def sortAndFilterOutputs(
      outputsWithSerialId: Vector[(TransactionOutput, UInt64)])
      : Vector[TransactionOutput] = {
    outputsWithSerialId
      .sortBy(_._2)
      .map(_._1)
      .filter(_.value >= Policy.dustThreshold)
  }
}
