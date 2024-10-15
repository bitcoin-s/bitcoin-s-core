package org.bitcoins.dlc.wallet.util

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrNonce, Sha256Digest}
import org.bitcoins.db.DbCommonsColumnMappers
import org.bitcoins.dlc.wallet.models._

import scala.concurrent.ExecutionContext

/** Utility class to help build actions to insert things into our DLC tables */
case class DLCActionBuilder(dlcWalletDAOs: DLCWalletDAOs) {

  private val dlcDAO = dlcWalletDAOs.dlcDAO
  private val dlcAnnouncementDAO = dlcWalletDAOs.dlcAnnouncementDAO
  private val dlcInputsDAO = dlcWalletDAOs.dlcInputsDAO
  private val dlcOfferDAO = dlcWalletDAOs.dlcOfferDAO
  private val contractDataDAO = dlcWalletDAOs.contractDataDAO
  private val dlcAcceptDAO = dlcWalletDAOs.dlcAcceptDAO
  private val dlcSigsDAO = dlcWalletDAOs.dlcSigsDAO
  private val dlcRefundSigDAO = dlcWalletDAOs.dlcRefundSigDAO
  private val oracleNonceDAO = dlcWalletDAOs.oracleNonceDAO

  // idk if it matters which profile api i import, but i need access to transactionally
  import dlcWalletDAOs.dlcDAO.profile.api._
  private val mappers = new DbCommonsColumnMappers(dlcWalletDAOs.dlcDAO.profile)
  import mappers.sha256DigestMapper

  /** Builds an offer in our database, adds relevant information to the global
    * table, contract data, announcements, funding inputs, and the actual offer
    * itself
    */
  def buildCreateOfferAction(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcAnnouncementDbs: Vector[DLCAnnouncementDb],
      dlcInputs: Vector[DLCFundingInputDb],
      dlcOfferDb: DLCOfferDb
  )(implicit ec: ExecutionContext): DBIOAction[
    Unit,
    NoStream,
    Effect.Read & Effect.Write & Effect.Transactional
  ] = {
    val globalAction = dlcDAO.upsertAction(dlcDb)
    val contractAction = contractDataDAO.upsertAction(contractDataDb)
    val announcementAction =
      dlcAnnouncementDAO.upsertAllAction(dlcAnnouncementDbs)
    val inputsAction = dlcInputsDAO.upsertAllAction(dlcInputs)
    val offerAction = dlcOfferDAO.upsertAction(dlcOfferDb)
    val actions = Vector(
      globalAction,
      contractAction,
      announcementAction,
      inputsAction,
      offerAction
    )
    val allActions = DBIO
      .sequence(actions)
      .map(_ => ())
    allActions
  }

  /** Builds an accept in our database, adds relevant information to the offer
    * table, accept table, cet sigs table, inputs table, and refund table
    */
  def buildCreateAcceptAction(
      dlcDb: DLCDb,
      offerInputs: Vector[DLCFundingInputDb],
      cetSigsDb: Vector[DLCCETSignaturesDb],
      refundSigsDb: DLCRefundSigsDb
  )(implicit ec: ExecutionContext): DBIOAction[
    Unit,
    NoStream,
    Effect.Write & Effect.Read & Effect.Transactional
  ] = {
    val dlcDbAction = dlcDAO.updateAction(dlcDb)
    val inputAction = dlcInputsDAO.upsertAllAction(offerInputs)
    val sigsAction = dlcSigsDAO.createAllAction(cetSigsDb)
    val refundSigAction = dlcRefundSigDAO.createAction(refundSigsDb)
    val actions = Vector(dlcDbAction, inputAction, sigsAction, refundSigAction)

    val allActions = DBIO
      .sequence(actions)
      .map(_ => ())
    allActions
  }

  /** Creates the action to delete the given dlc from our database. This removes
    * references to the dlc in our various tables
    */
  def deleteDLCAction(dlcId: Sha256Digest)(implicit
      ec: ExecutionContext
  ): DBIOAction[Unit, NoStream, Effect.Write & Effect.Transactional] = {
    val deleteSigA = dlcSigsDAO.deleteByDLCIdAction(dlcId)
    val deleteRefundSigA = dlcRefundSigDAO.deleteByDLCIdAction(dlcId)
    val deleteInputSigA = dlcInputsDAO.deleteByDLCIdAction(dlcId)
    val deleteAcceptA = dlcAcceptDAO.deleteByDLCIdAction(dlcId)
    val deleteOfferA = dlcOfferDAO.deleteByDLCIdAction(dlcId)
    val deleteContractDataA = contractDataDAO.deleteByDLCIdAction(dlcId)
    val deleteAnnouncementDataA = dlcAnnouncementDAO.deleteByDLCIdAction(dlcId)
    val deleteDlcA = dlcDAO.deleteByDLCIdAction(dlcId)

    val action = for {
      _ <- deleteSigA
      _ <- deleteRefundSigA
      _ <- deleteInputSigA
      _ <- deleteAcceptA
      _ <- deleteOfferA
      _ <- deleteContractDataA
      _ <- deleteAnnouncementDataA
      _ <- deleteDlcA
    } yield ()

    action
  }

  /** Retrieves a DBIOAction that fetches the global dlc db, the contract, the
    * offer, and funding inputs
    */
  def getDLCOfferDataAction(
      dlcId: Sha256Digest
  )(implicit ec: ExecutionContext): DBIOAction[
    (
        Option[DLCDb],
        Option[DLCContractDataDb],
        Option[DLCOfferDb],
        Vector[DLCFundingInputDb]
    ),
    NoStream,
    Effect.Read & Effect.Transactional
  ] = {
    val dlcDbQ = dlcDAO.findByPrimaryKey(dlcId)
    val contractDbsQ = contractDataDAO.findByPrimaryKey(dlcId)
    val offerDbsQ = dlcOfferDAO.findByPrimaryKey(dlcId)
    // optimization to use sql queries rather than action
    // as this method gets called a lot.
    val dlcDbOfferDbContractDataDbOptA
        : DBIOAction[Option[
                       ((DLCDb, DLCOfferDb), DLCContractDataDb)
                     ],
                     NoStream,
                     Effect.Read & Effect.Transactional] = {
      dlcDbQ
        .join(offerDbsQ)
        .on(_.dlcId === _.dlcId)
        .join(contractDbsQ)
        .on(_._1.dlcId === _.dlcId)
        .result
        .transactionally
        .map(_.headOption)
    }

    val fundingInputsAction = dlcInputsDAO.findByDLCIdAction(dlcId)
    val combined = for {
      dlcDbOfferDbContractDataDbOpt <- dlcDbOfferDbContractDataDbOptA
      inputs <- fundingInputsAction
      // only want offerer inputs
      offerInputs = inputs.filter(_.isInitiator)
    } yield {
      dlcDbOfferDbContractDataDbOpt match {
        case Some(((dlcDb, dlcOfferDb), contractDataDb)) =>
          (Some(dlcDb), Some(contractDataDb), Some(dlcOfferDb), offerInputs)
        case None =>
          (None, None, None, offerInputs)
      }
    }

    combined
  }

  /** Updates various tables in our database with oracle attestations that are
    * published by the oracle
    */
  def updateDLCOracleSigsAction(
      outcomeAndSigByNonce: Map[SchnorrNonce, (String, SchnorrDigitalSignature)]
  )(implicit ec: ExecutionContext)
      : DBIOAction[Vector[
                     OracleNonceDb
                   ],
                   NoStream,
                   Effect.Write & Effect.Read & Effect.Transactional] = {
    val updateAction = for {
      nonceDbs <- oracleNonceDAO.findByNoncesAction(
        outcomeAndSigByNonce.keys.toVector
      )
      _ = assert(
        nonceDbs.size == outcomeAndSigByNonce.keys.size,
        "Didn't receive all nonce dbs"
      )

      updated = nonceDbs.map { db =>
        val (outcome, sig) = outcomeAndSigByNonce(db.nonce)
        db.copy(outcomeOpt = Some(outcome), signatureOpt = Some(sig))
      }

      updateNonces <- oracleNonceDAO.updateAllAction(updated)

      announcementDbs <- {
        val announcementIds = updateNonces.map(_.announcementId).distinct
        dlcAnnouncementDAO.findByAnnouncementIdsAction(announcementIds)
      }
      updatedDbs = announcementDbs.map(_.copy(used = true))
      _ <- dlcAnnouncementDAO.updateAllAction(updatedDbs)
    } yield updateNonces

    updateAction
  }
}
