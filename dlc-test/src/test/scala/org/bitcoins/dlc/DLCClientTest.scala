package org.bitcoins.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCSign
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  IfConditionalScriptPubKey,
  MultiSignatureScriptPubKey,
  NonStandardIfConditionalScriptPubKey,
  P2PKScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, FutureUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  P2SHNestedSegwitV0InputInfo,
  P2WPKHV0InputInfo,
  P2WSHV0InputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution._
import org.bitcoins.dlc.testgen.{DLCTestUtil, TestDLCClient}
import org.bitcoins.dlc.verify.DLCSignatureVerifier
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.Assertion

import scala.concurrent.{Future, Promise}

class DLCClientTest extends BitcoinSAsyncTest {
  behavior of "AdaptorDLCClient"

  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: SchnorrPublicKey = oraclePrivKey.schnorrPublicKey
  val preCommittedK: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val preCommittedR: SchnorrNonce = preCommittedK.schnorrNonce

  val localInput: CurrencyUnit = CurrencyUnits.oneBTC
  val remoteInput: CurrencyUnit = CurrencyUnits.oneBTC
  val totalInput: CurrencyUnit = localInput + remoteInput

  val inputPrivKeyLocal: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal: ECPublicKey = inputPrivKeyLocal.publicKey
  val inputPrivKeyLocal2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyLocal2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal2A: ECPublicKey = inputPrivKeyLocal2A.publicKey
  val inputPubKeyLocal2B: ECPublicKey = inputPrivKeyLocal2B.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey
  val inputPrivKeyRemote2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyRemote2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote2A: ECPublicKey = inputPrivKeyRemote2A.publicKey
  val inputPubKeyRemote2B: ECPublicKey = inputPrivKeyRemote2B.publicKey

  val blockTimeToday: BlockTime = BlockTime(
    UInt32(System.currentTimeMillis() / 1000))

  val localFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(localInput,
                        P2WPKHWitnessSPKV0(inputPrivKeyLocal.publicKey))),
    UInt32.zero
  )

  val localNestedSPK: IfConditionalScriptPubKey =
    NonStandardIfConditionalScriptPubKey(P2PKScriptPubKey(inputPubKeyLocal2A),
                                         P2PKScriptPubKey(inputPubKeyLocal2B))

  val localFundingTx2: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(TransactionOutput(localInput, P2WSHWitnessSPKV0(localNestedSPK))),
    UInt32.zero
  )

  val localFundingUtxos = Vector(
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint =
                          TransactionOutPoint(localFundingTx.txId, UInt32.zero),
                        amount = localInput,
                        pubKey = inputPubKeyLocal),
      prevTransaction = localFundingTx,
      signer = inputPrivKeyLocal,
      hashType = HashType.sigHashAll
    ),
    ScriptSignatureParams(
      P2WSHV0InputInfo(
        outPoint = TransactionOutPoint(localFundingTx2.txId, UInt32.zero),
        amount = localInput,
        scriptWitness = P2WSHWitnessV0(localNestedSPK),
        ConditionalPath.nonNestedTrue
      ),
      prevTransaction = localFundingTx2,
      signer = inputPrivKeyLocal2A,
      hashType = HashType.sigHashAll
    )
  )

  val localFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(localFundingTx,
                              UInt32.zero,
                              TransactionConstants.sequence),
      DLCFundingInputP2WSHV0(localFundingTx2,
                             UInt32.zero,
                             TransactionConstants.sequence,
                             maxWitnessLen =
                               UInt16(localFundingUtxos.last.maxWitnessLen))
    )

  val remoteFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(remoteInput,
                        P2WPKHWitnessSPKV0(inputPrivKeyRemote.publicKey))),
    UInt32.zero
  )

  val remoteNestedSPK: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(2,
                               Vector(inputPubKeyRemote2A, inputPubKeyRemote2B))

  val remoteFundingTx2: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(remoteInput,
                        P2SHScriptPubKey(P2WSHWitnessSPKV0(remoteNestedSPK)))),
    UInt32.zero
  )

  val remoteFundingUtxos = Vector(
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint = TransactionOutPoint(remoteFundingTx.txId,
                                                       UInt32.zero),
                        amount = remoteInput,
                        pubKey = inputPubKeyRemote),
      prevTransaction = remoteFundingTx,
      signer = inputPrivKeyRemote,
      hashType = HashType.sigHashAll
    ),
    ScriptSignatureParams(
      P2SHNestedSegwitV0InputInfo(
        outPoint = TransactionOutPoint(remoteFundingTx2.txId, UInt32.zero),
        amount = remoteInput,
        scriptWitness = P2WSHWitnessV0(remoteNestedSPK),
        ConditionalPath.NoCondition
      ),
      prevTransaction = remoteFundingTx2,
      signers = Vector(inputPrivKeyRemote2A, inputPrivKeyRemote2B),
      hashType = HashType.sigHashAll
    )
  )

  val remoteFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(remoteFundingTx,
                              UInt32.zero,
                              TransactionConstants.sequence),
      DLCFundingInputP2SHSegwit(
        prevTx = remoteFundingTx2,
        prevTxVout = UInt32.zero,
        sequence = TransactionConstants.sequence,
        maxWitnessLen = UInt16(remoteFundingUtxos.last.maxWitnessLen),
        redeemScript = P2WSHWitnessSPKV0(remoteNestedSPK)
      )
    )

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val offerFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val offerPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val timeouts: DLCTimeouts =
    DLCTimeouts(blockTimeToday,
                BlockTime(UInt32(blockTimeToday.time.toLong + 1)))

  val feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte(Satoshis.one)

  def constructDLCClients(numOutcomes: Int): (
      TestDLCClient,
      TestDLCClient,
      Vector[Sha256Digest]) = {
    val outcomeHashes = DLCTestUtil.genOutcomes(numOutcomes).map(_._2)

    val (outcomes, remoteOutcomes) =
      DLCTestUtil.genContractInfos(outcomeHashes, totalInput)

    // Offer is local
    val dlcOffer: TestDLCClient = TestDLCClient(
      outcomes = outcomes,
      oraclePubKey = oraclePubKey,
      preCommittedR = preCommittedR,
      isInitiator = true,
      fundingPrivKey = offerFundingPrivKey,
      payoutPrivKey = offerPayoutPrivKey,
      remotePubKeys = DLCPublicKeys.fromPrivKeys(acceptFundingPrivKey,
                                                 acceptPayoutPrivKey,
                                                 RegTest),
      input = localInput,
      remoteInput = remoteInput,
      fundingUtxos = localFundingUtxos,
      remoteFundingInputs = remoteFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = localChangeSPK,
      remoteChangeSPK = remoteChangeSPK,
      network = RegTest
    )

    // Accept is remote
    val dlcAccept: TestDLCClient = TestDLCClient(
      outcomes = remoteOutcomes,
      oraclePubKey = oraclePubKey,
      preCommittedR = preCommittedR,
      isInitiator = false,
      fundingPrivKey = acceptFundingPrivKey,
      payoutPrivKey = acceptPayoutPrivKey,
      remotePubKeys = DLCPublicKeys.fromPrivKeys(offerFundingPrivKey,
                                                 offerPayoutPrivKey,
                                                 RegTest),
      input = remoteInput,
      remoteInput = localInput,
      fundingUtxos = remoteFundingUtxos,
      remoteFundingInputs = localFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = remoteChangeSPK,
      remoteChangeSPK = localChangeSPK,
      network = RegTest
    )

    (dlcOffer, dlcAccept, outcomeHashes)
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(
      outcome: DLCOutcome,
      dlcOffer: TestDLCClient,
      dlcAccept: TestDLCClient): Assertion = {
    val fundingTx = outcome.fundingTx
    assert(noEmptySPKOutputs(fundingTx))

    val signers = Vector(dlcOffer.fundingPrivKey, dlcAccept.fundingPrivKey)
    val closingSpendingInfo = ScriptSignatureParams(
      P2WSHV0InputInfo(
        TransactionOutPoint(fundingTx.txId, UInt32.zero),
        fundingTx.outputs.head.value,
        P2WSHWitnessV0(
          MultiSignatureScriptPubKey(2,
                                     signers.map(_.publicKey).sortBy(_.hex))),
        ConditionalPath.NoCondition
      ),
      fundingTx,
      signers,
      HashType.sigHashAll
    )

    outcome match {
      case ExecutedDLCOutcome(fundingTx, cet) =>
        DLCFeeTestUtil.validateFees(dlcOffer.dlcTxBuilder,
                                    fundingTx,
                                    cet,
                                    fundingTxSigs = 5)
        assert(noEmptySPKOutputs(cet))
        assert(BitcoinScriptUtil.verifyScript(cet, Vector(closingSpendingInfo)))
      case RefundDLCOutcome(fundingTx, refundTx) =>
        DLCFeeTestUtil.validateFees(dlcOffer.dlcTxBuilder,
                                    fundingTx,
                                    refundTx,
                                    fundingTxSigs = 5)
        assert(noEmptySPKOutputs(refundTx))
        assert(
          BitcoinScriptUtil.verifyScript(refundTx, Vector(closingSpendingInfo)))
    }
  }

  def setupDLC(numOutcomes: Int): Future[
    (
        SetupDLC,
        TestDLCClient,
        SetupDLC,
        TestDLCClient,
        Vector[Sha256Digest])] = {
    val (dlcOffer, dlcAccept, outcomeHashes) = constructDLCClients(numOutcomes)

    val offerSigReceiveP =
      Promise[CETSignatures]()
    val sendAcceptSigs = { sigs: CETSignatures =>
      val _ = offerSigReceiveP.success(sigs)
      FutureUtil.unit
    }

    val acceptSigReceiveP =
      Promise[(CETSignatures, FundingSignatures)]()
    val sendOfferSigs = {
      (cetSigs: CETSignatures, fundingSigs: FundingSignatures) =>
        val _ = acceptSigReceiveP.success(cetSigs, fundingSigs)
        FutureUtil.unit
    }

    val acceptSetupF = dlcAccept.setupDLCAccept(sendSigs = sendAcceptSigs,
                                                getSigs =
                                                  acceptSigReceiveP.future)
    val offerSetupF = dlcOffer.setupDLCOffer(getSigs = offerSigReceiveP.future,
                                             sendSigs = sendOfferSigs,
                                             getFundingTx =
                                               acceptSetupF.map(_.fundingTx))

    for {
      acceptSetup <- acceptSetupF
      offerSetup <- offerSetupF
    } yield {
      assert(acceptSetup.fundingTx == offerSetup.fundingTx)
      acceptSetup.cets.foreach {
        case (outcome, CETInfo(cet, _)) =>
          assert(cet == offerSetup.cets(outcome).tx)
      }
      assert(acceptSetup.refundTx == offerSetup.refundTx)

      (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeHashes)
    }
  }

  def executeForCase(outcomeIndex: Int, numOutcomes: Int): Future[Assertion] = {
    setupDLC(numOutcomes).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeHashes) =>
        val oracleSig =
          oraclePrivKey.schnorrSignWithNonce(outcomeHashes(outcomeIndex).bytes,
                                             preCommittedK)

        for {
          offerOutcome <-
            dlcOffer.executeDLC(offerSetup, Future.successful(oracleSig))
          acceptOutcome <-
            dlcAccept.executeDLC(acceptSetup, Future.successful(oracleSig))
        } yield {
          assert(offerOutcome.fundingTx == acceptOutcome.fundingTx)

          validateOutcome(offerOutcome, dlcOffer, dlcAccept)
          validateOutcome(acceptOutcome, dlcOffer, dlcAccept)
        }
    }
  }

  def executeRefundCase(numOutcomes: Int): Future[Assertion] = {
    setupDLC(numOutcomes).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, _) =>
        val offerOutcome = dlcOffer.executeRefundDLC(offerSetup)
        val acceptOutcome = dlcAccept.executeRefundDLC(acceptSetup)

        validateOutcome(offerOutcome, dlcOffer, dlcAccept)
        validateOutcome(acceptOutcome, dlcOffer, dlcAccept)

        assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
        assert(acceptOutcome.refundTx == offerOutcome.refundTx)
    }
  }

  val numOutcomesToTest: Vector[Int] = Vector(2, 3, 5, 8)

  def runTests(exec: (Int, Int) => Future[Assertion]): Future[Assertion] = {
    val testFs = numOutcomesToTest.map { numOutcomes =>
      (0 until numOutcomes).foldLeft(Future.successful(succeed)) {
        case (lastTestF, outcomeIndex) =>
          lastTestF.flatMap { _ =>
            exec(outcomeIndex, numOutcomes)
          }
      }
    }

    Future.sequence(testFs).map(_ => succeed)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal case" in {
    runTests(executeForCase)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    val testFs = numOutcomesToTest.map { numOutcomes =>
      executeRefundCase(numOutcomes)
    }

    Future.sequence(testFs).map(_ => succeed)
  }

  it should "all work for a 100 outcome DLC" in {
    val numOutcomes = 100
    val testFs = (0 until 10).map(_ * 10).map { outcomeIndex =>
      for {
        _ <- executeForCase(outcomeIndex, numOutcomes)
      } yield succeed
    }

    Future.sequence(testFs).flatMap(_ => executeRefundCase(numOutcomes))
  }

  it should "fail on invalid funding signatures" in {
    val (offerClient, acceptClient, _) =
      constructDLCClients(numOutcomes = 3)
    val builder = offerClient.dlcTxBuilder
    val offerVerifier = DLCSignatureVerifier(builder, isInitiator = true)
    val acceptVerifier = DLCSignatureVerifier(builder, isInitiator = false)

    for {
      offerFundingSigs <- offerClient.dlcTxSigner.createFundingTxSigs()
      acceptFundingSigs <- acceptClient.dlcTxSigner.createFundingTxSigs()

      badOfferFundingSigs = DLCTestUtil.flipBit(offerFundingSigs)
      badAcceptFundingSigs = DLCTestUtil.flipBit(acceptFundingSigs)

      _ <- recoverToSucceededIf[RuntimeException] {
        offerClient.dlcTxSigner.signFundingTx(badAcceptFundingSigs)
      }
      _ <- recoverToSucceededIf[RuntimeException] {
        acceptClient.dlcTxSigner.signFundingTx(badOfferFundingSigs)
      }
    } yield {
      assert(offerVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
      assert(acceptVerifier.verifyRemoteFundingSigs(offerFundingSigs))

      assert(!offerVerifier.verifyRemoteFundingSigs(badAcceptFundingSigs))
      assert(!acceptVerifier.verifyRemoteFundingSigs(badOfferFundingSigs))
      assert(!offerVerifier.verifyRemoteFundingSigs(offerFundingSigs))
      assert(!acceptVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
    }
  }

  it should "fail on invalid CET signatures" in {
    val (offerClient, acceptClient, outcomes) =
      constructDLCClients(numOutcomes = 3)
    val builder = offerClient.dlcTxBuilder
    val offerVerifier = DLCSignatureVerifier(builder, isInitiator = true)
    val acceptVerifier = DLCSignatureVerifier(builder, isInitiator = false)

    for {
      offerCETSigs <- offerClient.dlcTxSigner.createCETSigs()
      acceptCETSigs <- acceptClient.dlcTxSigner.createCETSigs()

      badOfferCETSigs = DLCTestUtil.flipBit(offerCETSigs)
      badAcceptCETSigs = DLCTestUtil.flipBit(acceptCETSigs)

      cetFailures = outcomes.map { outcome =>
        val oracleSig =
          oraclePrivKey.schnorrSignWithNonce(outcome.bytes, preCommittedK)

        for {
          _ <- recoverToSucceededIf[RuntimeException] {
            offerClient.dlcTxSigner.signCET(
              outcome,
              badAcceptCETSigs.outcomeSigs(outcome),
              oracleSig)
          }
          _ <- recoverToSucceededIf[RuntimeException] {
            acceptClient.dlcTxSigner
              .signCET(outcome, badOfferCETSigs.outcomeSigs(outcome), oracleSig)
          }
        } yield succeed
      }

      _ <- Future.sequence(cetFailures)

      _ <- recoverToExceptionIf[RuntimeException] {
        offerClient.dlcTxSigner.signRefundTx(badAcceptCETSigs.refundSig)
      }
      _ <- recoverToExceptionIf[RuntimeException] {
        acceptClient.dlcTxSigner.signRefundTx(badOfferCETSigs.refundSig)
      }
    } yield {
      outcomes.foreach { outcome =>
        assert(
          offerVerifier.verifyCETSig(outcome,
                                     acceptCETSigs.outcomeSigs(outcome)))
        assert(
          acceptVerifier.verifyCETSig(outcome,
                                      offerCETSigs.outcomeSigs(outcome)))
      }
      assert(offerVerifier.verifyRefundSig(acceptCETSigs.refundSig))
      assert(offerVerifier.verifyRefundSig(offerCETSigs.refundSig))
      assert(acceptVerifier.verifyRefundSig(offerCETSigs.refundSig))
      assert(acceptVerifier.verifyRefundSig(acceptCETSigs.refundSig))

      outcomes.foreach { outcome =>
        assert(
          !offerVerifier.verifyCETSig(outcome,
                                      badAcceptCETSigs.outcomeSigs(outcome)))
        assert(
          !acceptVerifier.verifyCETSig(outcome,
                                       badOfferCETSigs.outcomeSigs(outcome)))

        assert(
          !offerVerifier.verifyCETSig(outcome,
                                      offerCETSigs.outcomeSigs(outcome)))
        assert(
          !acceptVerifier.verifyCETSig(outcome,
                                       acceptCETSigs.outcomeSigs(outcome)))
      }
      assert(!offerVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
      assert(!offerVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
      assert(!acceptVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
      assert(!acceptVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
    }
  }

  it should "be able to derive oracle signature from remote CET signature" in {
    val outcomeIndex = 1

    setupDLC(numOutcomes = 3).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeHashes) =>
        val oracleSig =
          oraclePrivKey.schnorrSignWithNonce(outcomeHashes(outcomeIndex).bytes,
                                             preCommittedK)

        for {
          acceptCETSigs <- dlcAccept.dlcTxSigner.createCETSigs()
          offerCETSigs <- dlcOffer.dlcTxSigner.createCETSigs()
          offerFundingSigs <- dlcOffer.dlcTxSigner.createFundingTxSigs()
          offerOutcome <-
            dlcOffer.executeDLC(offerSetup, Future.successful(oracleSig))
          acceptOutcome <-
            dlcAccept.executeDLC(acceptSetup, Future.successful(oracleSig))

          builder = DLCTxBuilder(dlcOffer.offer, dlcAccept.accept)
          contractId <- builder.buildFundingTx.map(
            _.txIdBE.bytes.xor(dlcAccept.accept.tempContractId.bytes))
        } yield {
          val offer = dlcOffer.offer
          val paramHash = offer.paramHash
          val accept = dlcOffer.accept.withSigs(acceptCETSigs)
          val sign = DLCSign(offerCETSigs, offerFundingSigs, contractId)

          val offerRemoteClaimed =
            DLCStatus.RemoteClaimed(paramHash.flip,
                                    isInitiator = true,
                                    offer,
                                    accept,
                                    sign,
                                    offerOutcome.fundingTx,
                                    acceptOutcome.cet)
          val acceptRemoteClaimed =
            DLCStatus.RemoteClaimed(paramHash.flip,
                                    isInitiator = false,
                                    offer,
                                    accept,
                                    sign,
                                    acceptOutcome.fundingTx,
                                    offerOutcome.cet)

          assert(offerRemoteClaimed.oracleSig == oracleSig)
          assert(acceptRemoteClaimed.oracleSig == oracleSig)
        }
    }
  }
}
