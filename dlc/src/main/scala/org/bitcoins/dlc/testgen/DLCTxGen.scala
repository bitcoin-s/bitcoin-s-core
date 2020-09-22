package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCAcceptWithoutSigs,
  DLCSign,
  OracleInfo
}
import org.bitcoins.commons.jsonmodels.dlc.DLCPublicKeys
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  EmptyScriptSignature,
  P2WPKHWitnessSPKV0
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey}
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.sign.DLCTxSigner
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

object DLCTxGen {
  import DLCTLVGen._

  def dlcParams(
      contractInfo: ContractInfo = genContractInfo(),
      contractMaturityBound: BlockTimeStamp = BlockTimeStamp(100),
      contractTimeout: BlockTimeStamp = BlockTimeStamp(200),
      feeRate: SatoshisPerVirtualByte =
        SatoshisPerVirtualByte(Satoshis(5))): DLCParams = {
    val privKey = ECPrivateKey.freshPrivateKey
    val kVal = ECPrivateKey.freshPrivateKey
    val oracleInfo = OracleInfo(privKey.schnorrPublicKey, kVal.schnorrNonce)
    val realOutcome = contractInfo.keys.toVector(contractInfo.size / 2)
    val sig = privKey.schnorrSignWithNonce(realOutcome.bytes, kVal)
    DLCParams(oracleInfo,
              contractInfo,
              contractMaturityBound,
              contractTimeout,
              feeRate,
              realOutcome,
              sig)
  }

  private val dummyTransactionInput = TransactionInput(
    TransactionOutPoint(CryptoUtil.doubleSHA256(ByteVector("DLC".getBytes)),
                        UInt32.zero),
    EmptyScriptSignature,
    UInt32.zero)

  def fundingInputTx(
      inputs: Vector[TransactionInput] = Vector(dummyTransactionInput),
      idx: Int = 0,
      privKey: ECPrivateKey = ECPrivateKey.freshPrivateKey,
      amt: CurrencyUnit = defaultAmt * 2,
      lockTime: UInt32 = UInt32.zero): FundingInputTx = {
    val pubKey = privKey.publicKey
    val outputs =
      Vector.fill(idx)(
        TransactionOutput(defaultAmt, EmptyScriptPubKey)) :+ TransactionOutput(
        amt,
        P2WPKHWitnessSPKV0(pubKey))
    val tx = BaseTransaction(TransactionConstants.validLockVersion,
                             inputs,
                             outputs,
                             lockTime)

    FundingInputTx(tx, idx, privKey)
  }

  def dlcPartyParams(
      collateral: CurrencyUnit = defaultAmt,
      fundingInputTxs: Vector[FundingInputTx] = Vector(fundingInputTx()),
      changeAddress: BitcoinAddress = address(),
      fundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey,
      payoutAddress: BitcoinAddress = address()): DLCPartyParams = {
    DLCPartyParams(collateral,
                   fundingInputTxs,
                   changeAddress,
                   fundingPrivKey,
                   payoutAddress)
  }

  def validTestInputs(
      params: DLCParams = dlcParams(),
      offerParams: DLCPartyParams = dlcPartyParams(),
      acceptParams: DLCPartyParams = dlcPartyParams()): ValidTestInputs = {
    ValidTestInputs(params, offerParams, acceptParams)
  }

  def successTestVector(inputs: ValidTestInputs = validTestInputs())(implicit
      ec: ExecutionContext): Future[SuccessTestVector] = {
    val offer = inputs.calcOffer
    val acceptWithoutSigs = DLCAcceptWithoutSigs(
      inputs.acceptParams.collateral.satoshis,
      DLCPublicKeys(inputs.acceptParams.fundingPrivKey.publicKey,
                    inputs.acceptParams.payoutAddress),
      inputs.acceptParams.fundingInputs,
      inputs.acceptParams.changeAddress,
      inputs.calcOffer.tempContractId
    )

    val builder = DLCTxBuilder(offer, acceptWithoutSigs)
    val offerSigner = DLCTxSigner(builder,
                                  isInitiator = true,
                                  inputs.offerParams.fundingPrivKey,
                                  inputs.offerParams.payoutAddress,
                                  inputs.offerParams.fundingScriptSigParams)
    val acceptSigner = DLCTxSigner(builder,
                                   isInitiator = false,
                                   inputs.acceptParams.fundingPrivKey,
                                   inputs.acceptParams.payoutAddress,
                                   inputs.acceptParams.fundingScriptSigParams)

    val outcome = inputs.params.realOutcome

    for {
      accpetCETSigs <- acceptSigner.createCETSigs()
      offerCETSigs <- offerSigner.createCETSigs()
      offerFundingSigs <- offerSigner.createFundingTxSigs()

      fundingTx <- builder.buildFundingTx
      cetFs = inputs.params.contractInfo.keys.toVector.map(builder.buildCET)
      cets <- Future.sequence(cetFs)
      refundTx <- builder.buildRefundTx

      signedFundingTx <- acceptSigner.signFundingTx(offerFundingSigs)
      signedRefundTx <- offerSigner.signRefundTx(accpetCETSigs.refundSig)
      offerSignedCET <- offerSigner.signCET(outcome,
                                            accpetCETSigs.outcomeSigs(outcome),
                                            inputs.params.oracleSignature)
      acceptSignedCET <- acceptSigner.signCET(outcome,
                                              offerCETSigs.outcomeSigs(outcome),
                                              inputs.params.oracleSignature)
    } yield {
      val accept = acceptWithoutSigs.withSigs(accpetCETSigs)

      val contractId = fundingTx.txIdBE.bytes.xor(accept.tempContractId.bytes)
      val sign = DLCSign(offerCETSigs, offerFundingSigs, contractId)

      SuccessTestVector(
        inputs,
        offer.toTLV,
        accept.toTLV,
        sign.toTLV,
        DLCTransactions(fundingTx, cets, refundTx),
        DLCTransactions(signedFundingTx,
                        Vector(offerSignedCET, acceptSignedCET),
                        signedRefundTx)
      )
    }
  }

  def randomSuccessTestVector(numOutcomes: Int)(implicit
      ec: ExecutionContext): Future[SuccessTestVector] = {
    val outcomes = DLCTestUtil.genOutcomes(numOutcomes)
    val contractInfo = genContractInfo(outcomes)

    successTestVector(validTestInputs(dlcParams(contractInfo = contractInfo)))
  }
}
