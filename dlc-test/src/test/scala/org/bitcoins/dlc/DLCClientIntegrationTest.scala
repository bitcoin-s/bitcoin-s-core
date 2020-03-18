package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrDigitalSignature,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{
  Bitcoins,
  CurrencyUnit,
  CurrencyUnits,
  Satoshis
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0
}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{CryptoUtil, FutureUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2WPKHV0SpendingInfo
import org.bitcoins.dlc.DLCMessage.ContractInfo
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class DLCClientIntegrationTest extends BitcoindRpcTest {
  private val clientsF = BitcoindRpcTestUtil.createNodePairV18(clientAccum)
  private val clientF = clientsF.map(_._1)
  private val addressForMiningF = clientF.flatMap(_.getNewAddress)

  def publishTransaction(tx: Transaction): Future[DoubleSha256DigestBE] = {
    for {
      client <- clientF
      txid <- client.sendRawTransaction(tx)
      addressForMining <- addressForMiningF
      _ <- client.generateToAddress(blocks = 6, addressForMining)
    } yield txid
  }

  def waitUntilBlock(blockHeight: Int): Future[Unit] = {
    for {
      client <- clientF
      addressForMining <- addressForMiningF
      _ <- BitcoindRpcTestUtil.waitUntilBlock(blockHeight,
                                              client,
                                              addressForMining)
    } yield ()
  }

  behavior of "DLCClient"

  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: ECPublicKey = oraclePrivKey.publicKey
  val preCommittedK: SchnorrNonce = SchnorrNonce.freshNonce
  val preCommittedR: ECPublicKey = preCommittedK.publicKey
  val localInput: CurrencyUnit = CurrencyUnits.oneBTC
  val remoteInput: CurrencyUnit = CurrencyUnits.oneBTC
  val totalInput: CurrencyUnit = localInput + remoteInput

  val inputPrivKeyLocal: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal: ECPublicKey = inputPrivKeyLocal.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey

  val localAddress: Try[BitcoinAddress] =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyLocal),
                                    RegTest)

  val remoteAddress: Try[BitcoinAddress] =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyRemote),
                                    RegTest)

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val csvTimeout: UInt32 = UInt32(30)

  def constructDLC(numOutcomes: Int): Future[
    (DLCClient, DLCClient, Vector[Sha256DigestBE])] = {
    def fundingInput(input: CurrencyUnit): Bitcoins = {
      Bitcoins((input + Satoshis(200)).satoshis)
    }

    val fundedInputsTxidF = for {
      client <- clientF
      transactionWithoutFunds <- client
        .createRawTransaction(
          Vector.empty,
          Map(localAddress.get -> fundingInput(localInput * 2),
              remoteAddress.get -> fundingInput(remoteInput * 2)))
      transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
      transaction = transactionResult.hex
      signedTxResult <- client.signRawTransactionWithWallet(transaction)
      localOutputIndex = signedTxResult.hex.outputs.zipWithIndex
        .find {
          case (output, _) =>
            output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(inputPubKeyLocal).pubKeyHash
              case _ => false
            }
        }
        .map(_._2)
      remoteOutputIndex = signedTxResult.hex.outputs.zipWithIndex
        .find {
          case (output, _) =>
            output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(inputPubKeyRemote).pubKeyHash
              case _ => false
            }
        }
        .map(_._2)
      txid <- publishTransaction(signedTxResult.hex)
    } yield {
      assert(localOutputIndex.isDefined)
      assert(remoteOutputIndex.isDefined)

      (txid, localOutputIndex.get, remoteOutputIndex.get, signedTxResult.hex)
    }

    val fundingTxF = fundedInputsTxidF.map(_._4)

    val localFundingUtxosF = fundedInputsTxidF.map {
      case (txid, localOutputIndex, _, tx) =>
        Vector(
          P2WPKHV0SpendingInfo(
            outPoint = TransactionOutPoint(txid, UInt32(localOutputIndex)),
            amount = tx.outputs(localOutputIndex).value,
            scriptPubKey = P2WPKHWitnessSPKV0(inputPubKeyLocal),
            signer = inputPrivKeyLocal,
            hashType = HashType.sigHashAll,
            scriptWitness = P2WPKHWitnessV0(inputPubKeyLocal)
          )
        )
    }

    val remoteFundingUtxosF = fundedInputsTxidF.map {
      case (txid, _, remoteOutputIndex, tx) =>
        Vector(
          P2WPKHV0SpendingInfo(
            outPoint = TransactionOutPoint(txid, UInt32(remoteOutputIndex)),
            amount = tx.outputs(remoteOutputIndex).value,
            scriptPubKey = P2WPKHWitnessSPKV0(inputPubKeyRemote),
            signer = inputPrivKeyRemote,
            hashType = HashType.sigHashAll,
            scriptWitness = P2WPKHWitnessV0(inputPubKeyRemote)
          )
        )
    }

    val feeRateF = clientF
      .flatMap(_.getNetworkInfo.map(_.relayfee))
      .map(btc => SatoshisPerByte(btc.satoshis))

    for {
      fundingTx <- fundingTxF
      localFundingUtxos <- localFundingUtxosF
      remoteFundingUtxos <- remoteFundingUtxosF
      feeRate <- feeRateF
      client <- clientF
      currentHeight <- client.getBlockCount
    } yield {
      val tomorrowInBlocks = BlockHeight(currentHeight + 144)
      val twoDaysInBlocks = BlockHeight(currentHeight + 288)

      val localExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)
      val remoteExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)

      val localVout = localFundingUtxos.head.outPoint.vout
      val remoteVout = remoteFundingUtxos.head.outPoint.vout

      val outcomes = DLCTestUtil.genOutcomes(numOutcomes)
      val outcomeHashes =
        outcomes.map(msg => CryptoUtil.sha256(ByteVector(msg.getBytes)).flip)

      val outcomeMap =
        outcomeHashes.zip(DLCTestUtil.genValues(numOutcomes, totalInput)).toMap
      val otherOutcomeMap = outcomeMap.map {
        case (hash, amt) => (hash, (totalInput - amt).satoshis)
      }

      val acceptDLC = DLCClient(
        outcomes = ContractInfo(outcomeMap),
        oraclePubKey = oraclePubKey,
        preCommittedR = preCommittedR,
        isInitiator = false,
        extPrivKey = localExtPrivKey,
        nextAddressIndex = 0,
        remotePubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(remoteExtPrivKey,
                                                             nextAddressIndex =
                                                               0,
                                                             RegTest),
        input = localInput,
        remoteInput = remoteInput,
        fundingUtxos = localFundingUtxos,
        remoteFundingInputs = Vector(
          OutputReference(TransactionOutPoint(fundingTx.txIdBE, remoteVout),
                          fundingTx.outputs(remoteVout.toInt))),
        timeouts = DLCTimeouts(penaltyTimeout = csvTimeout,
                               tomorrowInBlocks,
                               twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = localChangeSPK,
        remoteChangeSPK = remoteChangeSPK,
        network = RegTest
      )

      val offerDLC = DLCClient(
        outcomes = ContractInfo(otherOutcomeMap),
        oraclePubKey = oraclePubKey,
        preCommittedR = preCommittedR,
        isInitiator = true,
        extPrivKey = remoteExtPrivKey,
        nextAddressIndex = 0,
        remotePubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(localExtPrivKey,
                                                             nextAddressIndex =
                                                               0,
                                                             RegTest),
        input = remoteInput,
        remoteInput = localInput,
        fundingUtxos = remoteFundingUtxos,
        remoteFundingInputs = Vector(
          OutputReference(TransactionOutPoint(fundingTx.txIdBE, localVout),
                          fundingTx.outputs(localVout.toInt))),
        timeouts = DLCTimeouts(penaltyTimeout = csvTimeout,
                               tomorrowInBlocks,
                               twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = remoteChangeSPK,
        remoteChangeSPK = localChangeSPK,
        network = RegTest
      )

      (acceptDLC, offerDLC, outcomeHashes)
    }
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(outcome: DLCOutcome): Future[Assertion] = {
    clientF.flatMap { client =>
      val closingTxOpt = outcome match {
        case _: CooperativeDLCOutcome | _: UnilateralDLCOutcomeWithDustClosing |
            _: RefundDLCOutcomeWithDustClosing =>
          None
        case UnilateralDLCOutcomeWithClosing(_, _, closingTx, _) =>
          Some(closingTx)
        case RefundDLCOutcomeWithClosing(_, _, closingTx, _) => Some(closingTx)
      }

      val cetOpt = outcome match {
        case unilateral: UnilateralDLCOutcome => Some(unilateral.cet)
        case refund: RefundDLCOutcome         => Some(refund.refundTx)
        case _: CooperativeDLCOutcome         => None
      }

      closingTxOpt match {
        case None =>
          Future {
            cetOpt.foreach(cet => assert(noEmptySPKOutputs(cet)))
            assert(noEmptySPKOutputs(outcome.fundingTx))
          }
        case Some(closingTx) =>
          val txResultF = client.getRawTransaction(closingTx.txIdBE)

          txResultF.map { regtestLocalClosingTx =>
            assert(regtestLocalClosingTx.hex == closingTx)
            assert(regtestLocalClosingTx.confirmations.isDefined)
            assert(regtestLocalClosingTx.confirmations.get >= 6)

            assert(noEmptySPKOutputs(outcome.fundingTx))
            cetOpt.foreach(cet => assert(noEmptySPKOutputs(cet)))
            assert(noEmptySPKOutputs(closingTx))
          }
      }
    }
  }

  def setupDLC(
      dlcAccept: DLCClient,
      dlcOffer: DLCClient): Future[(SetupDLC, SetupDLC)] = {
    val offerSigReceiveP =
      Promise[CETSignatures]()
    val sendAcceptSigs = { sigs: CETSignatures =>
      val _ = offerSigReceiveP.success(sigs)
      FutureUtil.unit
    }

    val acceptSigReceiveP = Promise[(CETSignatures, FundingSignatures)]()
    val sendOfferSigs = {
      (cetSigs: CETSignatures, fundingSigs: FundingSignatures) =>
        val _ = acceptSigReceiveP.success(cetSigs, fundingSigs)
        FutureUtil.unit
    }

    val acceptSetupF = dlcAccept.setupDLCAccept(sendSigs = sendAcceptSigs,
                                                getSigs =
                                                  acceptSigReceiveP.future)

    val fundingTxP = Promise[Transaction]()

    val watchForFundingTx = new Runnable {
      override def run(): Unit = {
        if (!fundingTxP.isCompleted) {
          clientF.foreach { client =>
            val fundingTxResultF = client.getRawTransaction(
              dlcOffer.createUnsignedFundingTransaction.txIdBE)

            fundingTxResultF.onComplete {
              case Success(fundingTxResult) =>
                if (fundingTxResult.confirmations.isEmpty || fundingTxResult.confirmations.get < 3) {
                  ()
                } else {
                  fundingTxP.trySuccess(fundingTxResult.hex)
                }
              case Failure(_) => ()
            }
          }
        }
      }
    }

    val cancelOnFundingFound =
      system.scheduler.schedule(100.milliseconds, 1.second, watchForFundingTx)

    fundingTxP.future.foreach(_ => cancelOnFundingFound.cancel())

    val offerSetupF = dlcOffer.setupDLCOffer(getSigs = offerSigReceiveP.future,
                                             sendSigs = sendOfferSigs,
                                             getFundingTx = fundingTxP.future)

    for {
      acceptSetup <- acceptSetupF
      _ <- publishTransaction(acceptSetup.fundingTx)
      offerSetup <- offerSetupF
    } yield {
      assert(acceptSetup.fundingTx == offerSetup.fundingTx)
      assert(acceptSetup.refundTx == offerSetup.refundTx)
      assert(
        acceptSetup.cets.values.head.tx.txIdBE == offerSetup.cets.values.head.remoteTxid)
      assert(
        acceptSetup.cets.values.last.tx.txIdBE == offerSetup.cets.values.last.remoteTxid)
      assert(
        acceptSetup.cets.values.head.remoteTxid == offerSetup.cets.values.head.tx.txIdBE)
      assert(
        acceptSetup.cets.values.last.remoteTxid == offerSetup.cets.values.last.tx.txIdBE)

      (acceptSetup, offerSetup)
    }
  }

  def constructAndSetupDLC(numOutcomes: Int): Future[
    (DLCClient, SetupDLC, DLCClient, SetupDLC, Vector[Sha256DigestBE])] = {
    for {
      (acceptDLC, offerDLC, outcomeHashes) <- constructDLC(numOutcomes)
      (acceptSetup, offerSetup) <- setupDLC(acceptDLC, offerDLC)
    } yield (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomeHashes)
  }

  def executeForMutualCase(
      outcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {

    val setupsAndDLCs = for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomeHashes) <- constructAndSetupDLC(
        numOutcomes)
    } yield {
      val oracleSig =
        Schnorr.signWithNonce(outcomeHashes(outcomeIndex).bytes,
                              oraclePrivKey,
                              preCommittedK)

      if (local) {
        (offerDLC, offerSetup, acceptDLC, acceptSetup, oracleSig)
      } else {
        (acceptDLC, acceptSetup, offerDLC, offerSetup, oracleSig)
      }
    }

    val outcomeFs = setupsAndDLCs.map {
      case (initDLC, initSetup, otherDLC, otherSetup, oracleSig) =>
        val closeSigsP = Promise[(SchnorrDigitalSignature, PartialSignature)]()
        val initSendSigs = {
          (sig: SchnorrDigitalSignature, fundingSig: PartialSignature) =>
            closeSigsP.success(sig, fundingSig)
            FutureUtil.unit
        }

        val mutualCloseTxP = Promise[Transaction]()

        val watchForMutualCloseTx = new Runnable {
          override def run(): Unit = {
            if (!mutualCloseTxP.isCompleted) {
              val fundingTxId = initDLC
                .createUnsignedMutualClosePSBT(oracleSig)
                .transaction
                .txIdBE

              clientF.foreach { client =>
                val fundingTxResultF = client.getRawTransaction(fundingTxId)

                fundingTxResultF.onComplete {
                  case Success(fundingTxResult) =>
                    if (fundingTxResult.confirmations.isEmpty) {
                      ()
                    } else {
                      logger.info(
                        s"Found funding tx on chain! $fundingTxResult")
                      mutualCloseTxP.trySuccess(fundingTxResult.hex)
                    }
                  case Failure(_) => ()
                }
              }
            }
          }
        }

        val cancelOnMutualCloseFound =
          system.scheduler.schedule(100.milliseconds,
                                    1.second,
                                    watchForMutualCloseTx)

        mutualCloseTxP.future.foreach(_ => cancelOnMutualCloseFound.cancel())

        val initOutcomeF =
          initDLC.initiateMutualClose(initSetup,
                                      oracleSig,
                                      initSendSigs,
                                      mutualCloseTxP.future)

        val otherOutcomeF =
          otherDLC.executeMutualClose(otherSetup, closeSigsP.future)

        (initOutcomeF, otherOutcomeF)
    }

    for {
      (initOutcomeF, otherOutcomeF) <- outcomeFs
      otherOutcome <- otherOutcomeF
      _ <- publishTransaction(otherOutcome.closingTx)
      initOutcome <- initOutcomeF
      client <- clientF
      regtestClosingTx <- client.getRawTransaction(
        otherOutcome.closingTx.txIdBE)
    } yield {
      assert(initOutcome.fundingTx == otherOutcome.fundingTx)
      assert(initOutcome.closingTx == otherOutcome.closingTx)

      assert(noEmptySPKOutputs(initOutcome.fundingTx))
      assert(noEmptySPKOutputs(initOutcome.closingTx))

      assert(regtestClosingTx.hex == initOutcome.closingTx)
      assert(regtestClosingTx.confirmations.isDefined)
      assert(regtestClosingTx.confirmations.get >= 6)
    }
  }

  def executeForUnilateralCase(
      outcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {
    for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomeHashes) <- constructAndSetupDLC(
        numOutcomes)
      oracleSig = Schnorr.signWithNonce(outcomeHashes(outcomeIndex).bytes,
                                        oraclePrivKey,
                                        preCommittedK)
      (unilateralDLC, unilateralSetup, otherDLC, otherSetup) = {
        if (local) {
          (offerDLC, offerSetup, acceptDLC, acceptSetup)
        } else {
          (acceptDLC, acceptSetup, offerDLC, offerSetup)
        }
      }
      unilateralOutcome <- unilateralDLC.executeUnilateralDLC(
        unilateralSetup,
        Future.successful(oracleSig))
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt - 1)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt)
      _ <- publishTransaction(unilateralOutcome.cet)
      _ <- {
        unilateralOutcome match {
          case UnilateralDLCOutcomeWithClosing(_, _, closingTx, _) =>
            publishTransaction(closingTx)
          case _: UnilateralDLCOutcomeWithDustClosing => FutureUtil.unit
        }
      }
      otherOutcome <- otherDLC.executeRemoteUnilateralDLC(
        otherSetup,
        unilateralOutcome.cet,
        P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey))
      _ <- {
        otherOutcome match {
          case UnilateralDLCOutcomeWithClosing(_, _, closingTx, _) =>
            publishTransaction(closingTx)
          case _: UnilateralDLCOutcomeWithDustClosing => FutureUtil.unit
        }
      }
      _ <- validateOutcome(unilateralOutcome)
      _ <- validateOutcome(otherOutcome)
    } yield {
      assert(unilateralOutcome.fundingTx == otherOutcome.fundingTx)
      assert(unilateralOutcome.cet == otherOutcome.cet)
    }
  }

  def executeForRefundCase(
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {
    for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup, _) <- constructAndSetupDLC(
        numOutcomes)
      acceptOutcome <- acceptDLC.executeRefundDLC(acceptSetup)
      offerOutcome <- offerDLC.executeRefundDLC(offerSetup)
      offerClosingTxOpt = {
        offerOutcome match {
          case RefundDLCOutcomeWithClosing(_, _, closingTx, _) =>
            Some(closingTx)
          case _: RefundDLCOutcomeWithDustClosing => None
        }
      }
      acceptClosingTxOpt = {
        acceptOutcome match {
          case RefundDLCOutcomeWithClosing(_, _, closingTx, _) =>
            Some(closingTx)
          case _: RefundDLCOutcomeWithDustClosing => None
        }
      }
      _ = assert(offerOutcome.refundTx == acceptOutcome.refundTx)
      refundTx = offerOutcome.refundTx
      _ = assert(acceptDLC.timeouts == offerDLC.timeouts)
      timeout = offerDLC.timeouts.contractTimeout.toUInt32.toInt
      _ <- recoverToSucceededIf[BitcoindException](publishTransaction(refundTx))
      _ <- waitUntilBlock(timeout - 1)
      _ <- recoverToSucceededIf[BitcoindException](publishTransaction(refundTx))
      _ <- waitUntilBlock(timeout)
      _ <- publishTransaction(refundTx)
      _ <- {
        offerClosingTxOpt match {
          case Some(closingTx) => publishTransaction(closingTx)
          case None            => FutureUtil.unit
        }
      }
      _ <- {
        acceptClosingTxOpt match {
          case Some(closingTx) => publishTransaction(closingTx)
          case None            => FutureUtil.unit
        }
      }
      _ <- validateOutcome(offerOutcome)
      _ <- validateOutcome(acceptOutcome)
    } yield {
      assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
    }
  }

  def executeForJusticeCase(
      fakeOutcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {
    def chooseCET(
        localSetup: SetupDLC,
        remoteSetup: SetupDLC,
        outcomeHash: Sha256DigestBE): Transaction = {
      if (local) {
        remoteSetup.cets(outcomeHash).tx
      } else {
        localSetup.cets(outcomeHash).tx
      }
    }

    for {
      client <- clientF
      (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomeHashes) <- constructAndSetupDLC(
        numOutcomes)
      (punisherDLC, punisherSetup) = {
        if (local) {
          (offerDLC, offerSetup)
        } else {
          (acceptDLC, acceptSetup)
        }
      }
      cetWronglyPublished = chooseCET(offerSetup,
                                      acceptSetup,
                                      outcomeHashes(fakeOutcomeIndex))
      _ = assert(offerDLC.timeouts == acceptDLC.timeouts)
      timeout = offerDLC.timeouts.contractMaturity.toUInt32.toInt
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(cetWronglyPublished))
      _ <- waitUntilBlock(timeout - 1)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(cetWronglyPublished))
      heightBeforePublish <- client.getBlockCount
      _ <- waitUntilBlock(timeout)
      _ <- publishTransaction(cetWronglyPublished)
      justiceOutcome <- punisherDLC.executeJusticeDLC(punisherSetup,
                                                      cetWronglyPublished)
      toRemoteOutcome <- punisherDLC.executeRemoteUnilateralDLC(
        punisherSetup,
        cetWronglyPublished,
        P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey))
      _ = assert(toRemoteOutcome.cet == cetWronglyPublished)
      _ = assert(justiceOutcome.cet == cetWronglyPublished)
      _ <- {
        toRemoteOutcome match {
          case UnilateralDLCOutcomeWithClosing(_, _, closingTx, _) =>
            publishTransaction(closingTx)
          case _: UnilateralDLCOutcomeWithDustClosing => FutureUtil.unit
        }
      }
      justiceClosingTxOpt = {
        justiceOutcome match {
          case UnilateralDLCOutcomeWithClosing(_, _, closingTx, _) =>
            Some(closingTx)
          case _: UnilateralDLCOutcomeWithDustClosing => None
        }
      }
      _ <- justiceClosingTxOpt
        .map { tx =>
          recoverToSucceededIf[BitcoindException](
            publishTransaction(tx)
          )
        }
        .getOrElse(FutureUtil.unit)
      penaltyHeight = heightBeforePublish + punisherDLC.timeouts.penaltyTimeout.toInt + 1
      _ <- waitUntilBlock(penaltyHeight - 1)
      _ <- justiceClosingTxOpt
        .map { tx =>
          recoverToSucceededIf[BitcoindException](
            publishTransaction(tx)
          )
        }
        .getOrElse(FutureUtil.unit)
      _ <- waitUntilBlock(penaltyHeight)
      _ <- justiceClosingTxOpt
        .map(publishTransaction)
        .getOrElse(FutureUtil.unit)
      _ <- validateOutcome(toRemoteOutcome)
      _ <- validateOutcome(justiceOutcome)
    } yield {
      assert(justiceOutcome.fundingTx == toRemoteOutcome.fundingTx)
    }
  }

  val numOutcomesToTest: Vector[Int] = Vector(2, 8) //, 100)

  def indicesToTest(numOutcomes: Int): Vector[Int] = {
    if (numOutcomes == 2) {
      Vector(0, 1)
    } else {
      Vector(0, numOutcomes / 2, numOutcomes - 1)
    }
  }

  def runTests(
      exec: (Int, Int, Boolean) => Future[Assertion],
      local: Boolean): Future[Assertion] = {
    val testFs = numOutcomesToTest.flatMap { numOutcomes =>
      indicesToTest(numOutcomes).map { outcomeIndex => () =>
        exec(outcomeIndex, numOutcomes, local)
      }
    }

    testFs.foldLeft(Future.successful(succeed)) {
      case (resultF, testExec) =>
        resultF.flatMap { _ =>
          testExec()
        }
    }
  }

  it should "be able to publish all DLC txs to Regtest for the mutual local case" in {
    runTests(executeForMutualCase, local = true)
  }

  it should "be able to publish all DLC txs to Regtest for the mutual remote case" in {
    runTests(executeForMutualCase, local = false)
  }

  it should "be able to publish all DLC txs to Regtest for the normal local case" in {
    runTests(executeForUnilateralCase, local = true)
  }

  it should "be able to publish all DLC txs to Regtest for the normal remote case" in {
    runTests(executeForUnilateralCase, local = false)
  }

  it should "be able to publish all DLC txs to Regtest for the Refund case" in {
    val testFs = numOutcomesToTest.map { numOutcomes => () =>
      for {
        _ <- executeForRefundCase(numOutcomes, local = true)
        _ <- executeForRefundCase(numOutcomes, local = false)
      } yield succeed
    }

    testFs.foldLeft(Future.successful(succeed)) {
      case (resultF, testExec) =>
        resultF.flatMap { _ =>
          testExec()
        }
    }
  }

  it should "be able to take the justice branch on Regtest for the local case" in {
    runTests(executeForJusticeCase, local = true)
  }

  it should "be able to take the justice branch on Regtest for the remote case" in {
    runTests(executeForJusticeCase, local = false)
  }
}
