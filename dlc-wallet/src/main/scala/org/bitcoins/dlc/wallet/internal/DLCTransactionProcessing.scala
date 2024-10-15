package org.bitcoins.dlc.wallet.internal

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.api.dlc.wallet.db.*
import org.bitcoins.core.api.wallet.{
  ProcessTxResult,
  TransactionProcessingApi,
  UtxoHandlingApi
}
import org.bitcoins.core.api.wallet.db.{SpendingInfoDb, TransactionDb}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.dlc.execution.SetupDLC
import org.bitcoins.core.protocol.dlc.models.DLCMessage.*
import org.bitcoins.core.protocol.dlc.models.*
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.tlv.*
import org.bitcoins.core.protocol.transaction.{
  OutputWithIndex,
  Transaction,
  WitnessTransaction
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.{BlockHashWithConfs, FutureUtil}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{
  AddressTag,
  InputInfo,
  ScriptSignatureParams
}
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECDigitalSignature,
  SchnorrDigitalSignature,
  Sha256Digest
}
import org.bitcoins.db.SafeDatabase
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.dlc.wallet.models.*
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.models.{TransactionDAO, WalletDAOs}

import scala.concurrent.*

/** Overrides TransactionProcessing from Wallet to add extra logic to process
  * transactions that could from our own DLC.
  */
case class DLCTransactionProcessing(
    txProcessing: TransactionProcessingApi,
    dlcWalletDAOs: DLCWalletDAOs,
    walletDAOs: WalletDAOs,
    dlcDataManagement: DLCDataManagement,
    keyManager: BIP39KeyManager,
    transactionDAO: TransactionDAO,
    utxoHandling: UtxoHandlingApi,
    dlcWalletApi: DLCWalletApi)(implicit
    dlcConfig: DLCAppConfig,
    ec: ExecutionContext)
    extends TransactionProcessingApi
    with BitcoinSLogger {
  private val dlcDAO: DLCDAO = dlcWalletDAOs.dlcDAO
  private val dlcInputsDAO: DLCFundingInputDAO = dlcWalletDAOs.dlcInputsDAO
  private val dlcSigsDAO: DLCCETSignaturesDAO = dlcWalletDAOs.dlcSigsDAO
  private val oracleNonceDAO: OracleNonceDAO = dlcWalletDAOs.oracleNonceDAO
  private val dlcAnnouncementDAO: DLCAnnouncementDAO =
    dlcWalletDAOs.dlcAnnouncementDAO
  private lazy val safeDLCDatabase: SafeDatabase = dlcDAO.safeDatabase

  override def findByTxIds(
      txIds: Vector[DoubleSha256DigestBE]
  ): Future[Vector[TransactionDb]] = {
    transactionDAO.findByTxIds(txIds)
  }

  /** Calculates the new state of the DLCDb based on the closing transaction,
    * will delete old CET sigs that are no longer needed after execution
    * @return
    *   a DLCDb if we can calculate the state, else None if we cannot calculate
    *   the state
    */
  def calculateAndSetState(dlcDb: DLCDb): Future[Option[DLCDb]] = {
    (dlcDb.contractIdOpt, dlcDb.closingTxIdOpt) match {
      case (Some(id), Some(txId)) =>
        val fundingInputsF = dlcInputsDAO.findByDLCId(dlcDb.dlcId)
        val scriptSigParamsF =
          fundingInputsF.flatMap(inputs => getScriptSigParams(dlcDb, inputs))

        for {
          scriptSigParams <- scriptSigParamsF
          executorWithSetupOpt <- dlcDataManagement.executorAndSetupFromDb(
            id,
            transactionDAO,
            scriptSigParams,
            keyManager
          )
          updatedDlcDb <- executorWithSetupOpt match {
            case Some(exeutorWithSetup) =>
              calculateAndSetStateWithSetupDLC(
                exeutorWithSetup.setup,
                dlcDb,
                txId
              )
            case None =>
              // this means we have already deleted the cet sigs
              // just return the dlcdb given to us
              Future.successful(Some(dlcDb))
          }
        } yield {
          updatedDlcDb
        }
      case (None, None) | (None, Some(_)) | (Some(_), None) =>
        Future.successful(Some(dlcDb))
    }
  }

  /** Calculates the new closing state for a DLC if we still have adaptor
    * signatures available to us in the database
    */
  private def calculateAndSetStateWithSetupDLC(
      setup: SetupDLC,
      dlcDb: DLCDb,
      closingTxId: DoubleSha256DigestBE
  ): Future[Option[DLCDb]] = {
    val updatedOptF: Future[Option[DLCDb]] = {
      if (closingTxId == setup.refundTx.txIdBE) {
        val updatedOpt = Some(dlcDb.copy(state = DLCState.Refunded))
        Future.successful(updatedOpt)
      } else if (dlcDb.state == DLCState.Claimed) {
        val updatedOpt = Some(dlcDb.copy(state = DLCState.Claimed))
        Future.successful(updatedOpt)
      } else {
        if (dlcDb.state != DLCState.RemoteClaimed) {
          val withState = dlcDb.updateState(DLCState.RemoteClaimed)
          for {
            withOutcomeOpt <- calculateAndSetOutcome(withState)
            dlc <- dlcWalletApi.findDLC(dlcDb.dlcId)
            _ = dlcConfig.walletCallbacks.executeOnDLCStateChange(dlc.get)
          } yield {
            withOutcomeOpt
          }
        } else {
          // if the state is RemoteClaimed... we don't want to
          // calculate and set outcome again
          Future.successful(Some(dlcDb))
        }
      }
    }

    for {
      updatedOpt <- updatedOptF
      _ <- {
        val resultFOpt: Option[Future[Unit]] = {
          updatedOpt.map { updated =>
            updated.state match {
              case DLCState.Claimed | DLCState.RemoteClaimed |
                  DLCState.Refunded =>
                val contractId = updated.contractIdOpt.get.toHex
                logger.info(
                  s"Deleting unneeded DLC signatures for contract $contractId"
                )

                dlcSigsDAO
                  .deleteByDLCId(updated.dlcId)
                  .map(_ => ())
              case DLCState.Offered | DLCState.Accepted | DLCState.Signed |
                  DLCState.Broadcasted | DLCState.Confirmed |
                  _: DLCState.AdaptorSigComputationState =>
                FutureUtil.unit
            }
          }
        }
        resultFOpt match {
          case Some(resultF) => resultF
          case None          => Future.unit
        }
      }
    } yield updatedOpt
  }

  /** Calculates the outcome used for execution based on the closing
    * transaction. Returns None if we cannot calculate the outcome because we
    * have pruned the cet signatures from the database
    */
  private def calculateAndSetOutcome(dlcDb: DLCDb): Future[Option[DLCDb]] = {
    if (dlcDb.state == DLCState.RemoteClaimed) {
      logger.info(
        s"Calculating RemotedClaimed outcome for dlcId=${dlcDb.dlcId.hex} closingTx=${dlcDb.closingTxIdOpt
            .map(_.hex)}"
      )
      val dlcId = dlcDb.dlcId

      for {
        setupStateOpt <- dlcDataManagement.getDLCFundingData(
          dlcId,
          txDAO = transactionDAO
        )
        completeDbState = {
          setupStateOpt.get match {
            case c: SetupCompleteDLCDbState => c
            case offered: OfferedDbState =>
              sys.error(
                s"Cannot calculate and set outcome of dlc that is only offered, id=${offered.dlcDb.dlcId.hex}"
              )
          }
        }
        (sigDbs, refundSigOpt) <- dlcDataManagement.getCetAndRefundSigs(dlcId)
        (announcements, announcementData, nonceDbs) <- dlcDataManagement
          .getDLCAnnouncementDbs(dlcId)

        cet <-
          transactionDAO
            .read(dlcDb.closingTxIdOpt.get)
            .map(_.get.transaction.asInstanceOf[WitnessTransaction])

        sigAndOutcome = recoverSigAndOutcomeForRemoteClaimed(
          completeDbState = completeDbState,
          cet = cet,
          sigDbs = sigDbs,
          refundSigsDbOpt = refundSigOpt
        )
        sig = sigAndOutcome._1
        outcome = sigAndOutcome._2
        oracleInfos = getOutcomeDbInfo(outcome)._2

        noncesByAnnouncement = nonceDbs
          .groupBy(_.announcementId)

        announcementsWithId = dlcDataManagement.getOracleAnnouncementsWithId(
          announcements,
          announcementData,
          nonceDbs
        )

        usedIds = {
          announcementsWithId
            .filter(t => oracleInfos.exists(_.announcement == t._1))
            .map(_._2)
        }

        updatedAnnouncements = announcements
          .filter(t => usedIds.contains(t.announcementId))
          .map(_.copy(used = true))
        updatedNonces = {
          usedIds.flatMap { id =>
            outcome match {
              case e: EnumOracleOutcome =>
                val nonces = noncesByAnnouncement(id).sortBy(_.index)
                nonces.map(_.copy(outcomeOpt = Some(e.outcome.outcome)))
              case numeric: NumericOracleOutcome =>
                numeric.oraclesAndOutcomes.flatMap { case (oracle, outcome) =>
                  val id = announcementsWithId
                    .find(_._1 == oracle.announcement)
                    .map(_._2)
                    .get
                  val nonces = noncesByAnnouncement(id).sortBy(_.index)
                  outcome.digits.zip(nonces).map { case (digit, nonceDb) =>
                    nonceDb.copy(outcomeOpt = Some(digit.toString))
                  }
                }
            }
          }
        }
        updatedDlcDbSig = dlcDb.copy(aggregateSignatureOpt = Some(sig))
        // updates the aggregateSignatureOpt along with the state to RemoteClaimed
        updatedDlcDbA = dlcDAO.updateAction(updatedDlcDbSig)
        updateNonceA = oracleNonceDAO.updateAllAction(updatedNonces)
        updateAnnouncementA = dlcAnnouncementDAO.updateAllAction(
          updatedAnnouncements
        )
        actions = {
          for {
            updatedDlcDb <- updatedDlcDbA
            _ <- updateNonceA
            _ <- updateAnnouncementA
          } yield updatedDlcDb
        }
        updatedDlcDb <- safeDLCDatabase.run(actions)
      } yield {
        logger.info(
          s"Done calculating RemoteClaimed outcome for dlcId=${dlcId.hex}"
        )
        Some(updatedDlcDb)
      }
    } else {
      Future.successful(Some(dlcDb))
    }
  }

  /** Updates DLC states for a funding transaction we've received */
  private def processFundingTx(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Vector[DLCDb]] = {
    val dlcDbsF = dlcDAO.findByFundingTxId(tx.txIdBE)
    for {
      dlcDbs <- dlcDbsF
      _ <-
        if (dlcDbs.nonEmpty) {
          logger.info(
            s"Processing received utxos in tx ${tx.txIdBE.hex} for ${dlcDbs.size} DLC(s)"
          )
          txProcessing.insertTransaction(tx, blockHashOpt)
        } else FutureUtil.unit

      // Update the state to be confirmed or broadcasted
      updated = dlcDbs.map { dlcDb =>
        dlcDb.state match {
          case DLCState.Offered | DLCState.Accepted | DLCState.Signed |
              DLCState.Broadcasted =>
            if (blockHashOpt.isDefined)
              dlcDb.updateState(DLCState.Confirmed)
            else dlcDb.copy(state = DLCState.Broadcasted)
          case _: DLCState.AdaptorSigComputationState =>
            val contractIdOpt = dlcDb.contractIdOpt.map(_.toHex)
            throw new IllegalStateException(
              s"Cannot be settling a DLC when we are computing adaptor sigs! contractId=${contractIdOpt}"
            )
          case DLCState.Confirmed | DLCState.Claimed | DLCState.RemoteClaimed |
              DLCState.Refunded =>
            dlcDb
        }
      }

      _ <- dlcDAO.updateAll(updated)
      dlcIds = updated.map(_.dlcId).distinct
      isRescanning <- walletDAOs.stateDescriptorDAO.isRescanning
      _ <- sendWsDLCStateChange(dlcIds, isRescanning)
    } yield {
      updated
    }
  }

  /** Sends out a websocket event for the given dlcIds since their [[DLCState]]
    * changed
    * @param dlcIds
    *   the dlcIds that had their status change
    * @param isRescanning
    *   if the wallet is rescanning or not, we don't want to send out events if
    *   the wallet is rescanning
    */
  private def sendWsDLCStateChange(
      dlcIds: Vector[Sha256Digest],
      isRescanning: Boolean
  ): Future[Unit] = {
    if (isRescanning) {
      // don't send ws events if we are rescanning the wallet
      Future.unit
    } else {
      val updatedDlcDbsF = Future.traverse(dlcIds)(dlcWalletApi.findDLC)
      val sendF = updatedDlcDbsF.flatMap { updatedDlcDbs =>
        Future.sequence {
          updatedDlcDbs.map(u =>
            dlcConfig.walletCallbacks.executeOnDLCStateChange(u.get))
        }
      }
      sendF.map(_ => ())
    }
  }

  /** Processes all settled DLCs in this transaction and updates their states */
  private def processSettledDLCs(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Vector[DLCDb]] = {
    val outPoints = transaction.inputs.map(_.previousOutput).toVector
    val dlcDbsF = dlcDAO.findByFundingOutPoints(outPoints)
    for {
      dlcDbs <- dlcDbsF
      _ <-
        if (dlcDbs.nonEmpty) {
          logger.info(
            s"Processing spent utxos in tx ${transaction.txIdBE.hex} for ${dlcDbs.size} DLC(s)"
          )
          txProcessing.insertTransaction(transaction, blockHashOpt)
        } else FutureUtil.unit

      withTx = dlcDbs.map(_.updateClosingTxId(transaction.txIdBE))
      updated <- Future.traverse(withTx)(calculateAndSetState)
      _ <- dlcDAO.updateAll(updated.flatten)
    } yield {
      updated.flatten
    }
  }

  private def getOutcomeDbInfo(
      oracleOutcome: OracleOutcome
  ): (Vector[DLCOutcomeType], Vector[SingleOracleInfo]) = {
    oracleOutcome match {
      case EnumOracleOutcome(oracles, outcome) =>
        (Vector.fill(oracles.length)(outcome), oracles)
      case numeric: NumericOracleOutcome =>
        (numeric.outcomes, numeric.oracles)
    }
  }

  private def buildSignMessage(
      dlcDb: DLCDb,
      sigDbs: Vector[DLCCETSignaturesDb],
      offerRefundSig: PartialSignature[ECDigitalSignature],
      fundingInputDbs: Vector[DLCFundingInputDb]
  ): DLCSign = {
    {
      // if we don't have an acceptOpt because we don't have CET sigs
      // how are we getting them here?
      val cetSigs: CETSignatures =
        CETSignatures(
          sigDbs.map(dbSig => (dbSig.sigPoint, dbSig.initiatorSig.get))
        )

      val contractId = dlcDb.contractIdOpt.get
      val fundingSigs =
        fundingInputDbs
          .filter(_.isInitiator)
          .map { input =>
            input.witnessScriptOpt match {
              case Some(witnessScript) =>
                witnessScript match {
                  case EmptyScriptWitness =>
                    throw new RuntimeException("Script witness cannot be empty")
                  case taprootWitness: TaprootWitness =>
                    throw new UnsupportedOperationException(
                      s"Taproot not supported, got=$taprootWitness"
                    )
                  case witness: ScriptWitnessV0 =>
                    (input.outPoint, witness)
                }
              case None =>
                throw new RuntimeException("Must be segwit")
            }
          }

      DLCSign(
        cetSigs,
        offerRefundSig,
        FundingSignatures(fundingSigs),
        contractId
      )
    }
  }

  /** Recovers the aggregate signature and oracle outcome used to claim the DLC.
    * Remember, this flow is for [[DLCState.RemoteClaimed]] so we do not
    * necessarily have access to what the [[OracleAttestment]] is
    */
  private def recoverSigAndOutcomeForRemoteClaimed(
      completeDbState: SetupCompleteDLCDbState,
      cet: WitnessTransaction,
      sigDbs: Vector[DLCCETSignaturesDb],
      refundSigsDbOpt: Option[DLCRefundSigsDb]
  ): (SchnorrDigitalSignature, OracleOutcome) = {
    val dlcDb = completeDbState.dlcDb
    val dlcId = dlcDb.dlcId
    val isInit = dlcDb.isInitiator

    val offer = completeDbState.offer

    val acceptOpt = completeDbState.acceptOpt
    require(
      acceptOpt.isDefined,
      s"Accept message must still have CET signatures to recover an outcome on chain, dlcId=${dlcId.hex}"
    )
    val accept = acceptOpt.get

    val fundingInputDbs = completeDbState.allFundingInputs
    val offerRefundSigOpt = refundSigsDbOpt.flatMap(_.initiatorSig)

    val signOpt: Option[DLCSign] = offerRefundSigOpt.map { refundSig =>
      buildSignMessage(
        dlcDb = dlcDb,
        sigDbs = sigDbs,
        offerRefundSig = refundSig,
        fundingInputDbs = fundingInputDbs
      )
    }

    require(
      signOpt.isDefined,
      s"Can only recompute outcome if signMsg is defined, dlcId=${dlcId.hex}"
    )
    val sign = signOpt.get

    val sigsAndOutcomeOpt = DLCStatus.calculateOutcomeAndSig(
      isInitiator = isInit,
      offer = offer,
      accept = accept,
      sign = sign,
      cet = cet
    )

    require(
      sigsAndOutcomeOpt.isDefined,
      s"We must be able to calculate an outcome from a CET broadcast on   chain, dlcId=${offer.dlcId.hex} cet.txIdBE=${cet.txIdBE.hex}"
    )

    sigsAndOutcomeOpt.get
  }

  def getScriptSigParams(
      dlcDb: DLCDb,
      fundingInputs: Vector[DLCFundingInputDb]
  ): Future[Vector[ScriptSignatureParams[InputInfo]]] = {
    val outPoints =
      fundingInputs.filter(_.isInitiator == dlcDb.isInitiator).map(_.outPoint)
    val utxosF = utxoHandling.getUtxos(outPoints)
    for {
      utxos <- utxosF
      map = SpendingInfoDb.toPreviousOutputMap(utxos)
      scriptSigParams <-
        FutureUtil.foldLeftAsync(
          Vector.empty[ScriptSignatureParams[InputInfo]],
          utxos
        ) { (accum, utxo) =>
          transactionDAO
            .findByOutPoint(utxo.outPoint)
            .map(txOpt =>
              utxo.toUTXOInfo(keyManager, txOpt.get.transaction, map) +: accum)
        }
    } yield scriptSigParams
  }

  override def processTransaction(
      transaction: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs]): Future[Unit] = {
    txProcessing
      .processTransaction(transaction, blockHashWithConfsOpt)
      .flatMap(_ =>
        processFundingTx(transaction, blockHashWithConfsOpt.map(_.blockHash)))
      .flatMap(_ =>
        processSettledDLCs(transaction, blockHashWithConfsOpt.map(_.blockHash)))
      .map(_ => ())
  }

  override def processReceivedUtxos(
      tx: Transaction,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      spendingInfoDbs: Vector[SpendingInfoDb],
      newTags: Vector[AddressTag],
      relevantReceivedOutputs: Vector[OutputWithIndex])
      : Future[Vector[SpendingInfoDb]] = {
    txProcessing.processReceivedUtxos(tx,
                                      blockHashWithConfsOpt,
                                      spendingInfoDbs,
                                      newTags,
                                      relevantReceivedOutputs)
  }

  override def processSpentUtxos(
      transaction: Transaction,
      outputsBeingSpent: Vector[SpendingInfoDb],
      blockHashWithConfsOpt: Option[BlockHashWithConfs])
      : Future[Vector[SpendingInfoDb]] = {
    txProcessing.processSpentUtxos(transaction,
                                   outputsBeingSpent,
                                   blockHashWithConfsOpt)
  }

  /** Processes TXs originating from our wallet. This is called right after
    * we've signed a TX, updating our UTXO state.
    */
  override def processOurTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashWithConfsOpt: Option[BlockHashWithConfs],
      newTags: Vector[AddressTag]): Future[ProcessTxResult] = {
    txProcessing.processOurTransaction(transaction,
                                       feeRate,
                                       inputAmount,
                                       sentAmount,
                                       blockHashWithConfsOpt,
                                       newTags)
  }

  override def processBlock(block: Block): Future[Unit] =
    txProcessing.processBlock(block)

  override def listTransactions(): Future[Vector[TransactionDb]] =
    txProcessing.listTransactions()

  override def findTransaction(
      txId: DoubleSha256DigestBE): Future[Option[TransactionDb]] =
    txProcessing.findTransaction(txId)

  override def subscribeForBlockProcessingCompletionSignal(
      blockHash: DoubleSha256DigestBE): Future[DoubleSha256DigestBE] = {
    txProcessing.subscribeForBlockProcessingCompletionSignal(blockHash)
  }

  override def insertTransaction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[TransactionDb] = {
    txProcessing.insertTransaction(tx, blockHashOpt)
  }
}
