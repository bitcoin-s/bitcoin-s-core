package org.bitcoins.dlc.wallet

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.{
  DLCNeutrinoHDWalletApi,
  IncomingDLCOfferHandlingApi
}
import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.{
  AccountHandlingApi,
  AddressHandlingApi,
  BlockSyncState,
  FundTransactionHandlingApi,
  NeutrinoHDWalletApi,
  RescanHandlingApi,
  SendFundsHandlingApi,
  SyncHeightDescriptor,
  UtxoHandlingApi,
  WalletInfo
}
import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.*
import org.bitcoins.core.dlc.accounting.DLCWalletAccounting
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.hd.*
import org.bitcoins.core.number.*
import org.bitcoins.core.protocol.*
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.core.protocol.dlc.models.DLCMessage.*
import org.bitcoins.core.protocol.dlc.models.DLCState.*
import org.bitcoins.core.protocol.dlc.models.*
import org.bitcoins.core.protocol.dlc.sign.*
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.protocol.tlv.*
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.util.{FutureUtil, TimeUtil}
import org.bitcoins.core.wallet.builder.{
  FundRawTxHelper,
  ShufflingNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.*
import org.bitcoins.crypto.*
import org.bitcoins.db.SafeDatabase
import org.bitcoins.dlc.wallet.DLCWallet.InvalidAnnouncementSignature
import org.bitcoins.dlc.wallet.internal.*
import org.bitcoins.dlc.wallet.models.*
import org.bitcoins.dlc.wallet.util.{
  DLCAcceptUtil,
  DLCActionBuilder,
  DLCStatusBuilder,
  DLCTxUtil,
  IntermediaryDLCStatus
}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.internal.{
  RescanHandling,
  TransactionProcessing,
  UtxoHandling
}
import org.bitcoins.wallet.models.{
  AddressDAO,
  ScriptPubKeyDAO,
  TransactionDAO,
  WalletDAOs
}
import org.bitcoins.wallet.{Wallet, WalletLogger}
import scodec.bits.ByteVector
import slick.dbio.*

import java.net.InetSocketAddress
import java.time.Instant
import scala.concurrent.Future

/** A [[Wallet]] with full DLC Functionality */
case class DLCWallet(override val walletApi: Wallet)(implicit
    val dlcConfig: DLCAppConfig,
    val walletConfig: WalletAppConfig)
    extends DLCNeutrinoHDWalletApi
    with BitcoinSLogger {
  import dlcConfig.ec

  import dlcConfig.profile.api._

  private val networkParameters: NetworkParameters = walletConfig.network
  private[wallet] val dlcWalletDAOs = DLCWalletDAOs.fromDLCAppConfig(dlcConfig)
  private[bitcoins] val announcementDAO: OracleAnnouncementDataDAO =
    dlcWalletDAOs.oracleAnnouncementDAO
  private[bitcoins] val oracleNonceDAO: OracleNonceDAO =
    dlcWalletDAOs.oracleNonceDAO

  private[bitcoins] val dlcAnnouncementDAO: DLCAnnouncementDAO =
    dlcWalletDAOs.dlcAnnouncementDAO
  private[bitcoins] val dlcOfferDAO: DLCOfferDAO = dlcWalletDAOs.dlcOfferDAO
  private[bitcoins] val dlcAcceptDAO: DLCAcceptDAO = dlcWalletDAOs.dlcAcceptDAO
  private[bitcoins] val dlcDAO: DLCDAO = dlcWalletDAOs.dlcDAO

  private[bitcoins] val contractDataDAO: DLCContractDataDAO =
    dlcWalletDAOs.contractDataDAO
  private[bitcoins] val dlcInputsDAO: DLCFundingInputDAO =
    dlcWalletDAOs.dlcInputsDAO
  private[bitcoins] val dlcSigsDAO: DLCCETSignaturesDAO =
    dlcWalletDAOs.dlcSigsDAO
  private[bitcoins] val dlcRefundSigDAO: DLCRefundSigsDAO =
    dlcWalletDAOs.dlcRefundSigDAO
  private[bitcoins] val remoteTxDAO: DLCRemoteTxDAO =
    dlcWalletDAOs.dlcRemoteTxDAO

  private[bitcoins] val contactDAO: DLCContactDAO =
    dlcWalletDAOs.contactDAO

  private def walletDAOs: WalletDAOs = walletApi.walletDAOs

  private[bitcoins] def addressDAO: AddressDAO = walletApi.addressDAO
  private[bitcoins] def transactionDAO: TransactionDAO =
    walletApi.transactionDAO
  private[bitcoins] def scriptPubKeyDAO: ScriptPubKeyDAO =
    walletApi.scriptPubKeyDAO

  override def incomingOfferHandling: IncomingDLCOfferHandlingApi =
    IncomingDLCOffersHandling(dlcWalletDAOs)

  private[wallet] val dlcDataManagement = DLCDataManagement(dlcWalletDAOs)

  protected lazy val actionBuilder: DLCActionBuilder = {
    DLCActionBuilder(dlcWalletDAOs)
  }

  override def transactionProcessing: DLCTransactionProcessing = {
    val txProcessing = TransactionProcessing(
      walletApi = this,
      chainQueryApi = chainQueryApi,
      utxoHandling = utxoHandling.asInstanceOf[UtxoHandling],
      walletDAOs = walletDAOs
    )(walletApi.walletConfig, ec)
    DLCTransactionProcessing(
      txProcessing = txProcessing,
      dlcWalletDAOs = dlcWalletDAOs,
      walletDAOs = walletDAOs,
      dlcDataManagement = dlcDataManagement,
      keyManager = walletApi.keyManager,
      transactionDAO = walletDAOs.transactionDAO,
      utxoHandling = utxoHandling,
      dlcWalletApi = this
    )
  }

  override def rescanHandling: RescanHandlingApi = {
    RescanHandling(
      transactionProcessing = transactionProcessing,
      accountHandling = accountHandling,
      addressHandling = addressHandling,
      chainQueryApi = chainQueryApi,
      nodeApi = nodeApi,
      walletDAOs = walletDAOs
    )(walletConfig, walletConfig.system)
  }
  private lazy val safeDLCDatabase: SafeDatabase = dlcDAO.safeDatabase
  private lazy val walletDatabase: SafeDatabase =
    walletDAOs.addressDAO.safeDatabase

  /** Updates the contract Id in the wallet database for the given offer and
    * accept
    */
  private def updateDLCContractIds(
      offer: DLCOffer,
      accept: DLCAccept
  ): Future[DLCDb] = {
    require(
      accept.tempContractId == offer.tempContractId,
      "Offer and Accept have differing tempContractIds!"
    )
    val dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))
    for {
      dlcOpt <- dlcDAO.read(dlcId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with dlcId ${dlcId.hex}"
            )
          )
      }
      contractId = DLCUtil.calcContractId(offer, accept)

      newDLCDb = dlcDb.updateContractId(contractId)
      _ = logger.debug(s"Updating DLC contract Ids")
      updated <- dlcDAO.update(newDLCDb)
    } yield updated
  }

  /** Updates the [[DLCState]] of a DLC with the given contractId in the
    * wallet's database
    */
  private def updateDLCState(
      contractId: ByteVector,
      state: DLCState
  ): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with contractId ${contractId.toHex}"
            )
          )
      }
      _ = logger.debug(s"Updating DLC (${contractId.toHex}) to state $state")
      updated <- dlcDAO.update(dlcDb.updateState(state))
    } yield updated
  }

  /** Updates the funding outpoint in the DLCDb corresponding to the given
    * contractId
    */
  private def updateFundingOutPoint(
      contractId: ByteVector,
      outPoint: TransactionOutPoint
  ): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with contractId ${contractId.toHex}"
            )
          )
      }
      _ = logger.debug(
        s"Updating DLC (${contractId.toHex}) funding outpoint to ${outPoint.hex}"
      )
      updated <- dlcDAO.update(dlcDb.updateFundingOutPoint(outPoint))
    } yield updated
  }

  /** Updates the closing txId in the DLCDb corresponding to the given
    * contractId
    */
  private def updateClosingTxId(
      contractId: ByteVector,
      txId: DoubleSha256DigestBE
  ): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with contractId ${contractId.toHex}"
            )
          )
      }
      _ = logger.info(
        s"Updating DLC (${contractId.toHex}) closing txId to txIdBE=${txId.hex}"
      )
      updated <- dlcDAO.update(dlcDb.updateClosingTxId(txId))
    } yield updated
  }

  /** Updates the aggregate signature in the DLCDb corresponding to the given
    * contractId
    */
  private def updateAggregateSignature(
      contractId: ByteVector,
      aggregateSignature: SchnorrDigitalSignature
  ): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with contractId ${contractId.toHex}"
            )
          )
      }
      _ = logger.debug(
        s"Updating DLC (${contractId.toHex}) aggregate signature to ${aggregateSignature.hex}"
      )
      updated <- dlcDAO.update(
        dlcDb.updateAggregateSignature(aggregateSignature)
      )
    } yield updated
  }

  /** Updates the signatures in the oracle nonce database */
  private def updateDLCOracleSigs(
      sigs: Vector[OracleSignatures]
  ): Future[Vector[OracleNonceDb]] = {
    val outcomeAndSigByNonce = sigs.flatMap {
      case e: EnumOracleSignature =>
        Vector((e.sig.rx, (e.getOutcome.outcome, e.sig)))
      case numeric: NumericOracleSignatures =>
        val nonces = numeric.sigs.map(_.rx)
        val outcomes = numeric.getOutcome.digits.map(_.toString)
        val outcomeAndSigs = outcomes.zip(numeric.sigs)
        nonces.zip(outcomeAndSigs)
    }.toMap

    require(
      outcomeAndSigByNonce.forall(t => t._1 == t._2._2.rx),
      "nonces out of order"
    )
    val updateOracleSigsA =
      actionBuilder.updateDLCOracleSigsAction(outcomeAndSigByNonce)
    for {
      updates <- safeDLCDatabase.runVec(updateOracleSigsA)
    } yield updates
  }

  /** Writes the addresses corresponding to wallet's keys in a DLC to the
    * address database, this includes the address associated with the funding
    * public key
    */
  private def writeDLCKeysToAddressDb(
      account: AccountDb,
      chainType: HDChainType,
      index: Int
  ): Future[Vector[AddressDb]] = {
    for {
      zero <- walletApi.addressHandling.getAddress(account, chainType, index)
      one <- walletApi.addressHandling.getAddress(account, chainType, index + 1)
    } yield {
      logger.debug(s"Wrote DLC key addresses to database using index $index")
      Vector(zero, one)
    }
  }

  /** If the DLC has not reached the Signed state, it can be canceled. Canceling
    * a DLC deletes all data about it from the database, as well as unreserves
    * the utxos associated with it.
    */
  override def cancelDLC(dlcId: Sha256Digest): Future[Unit] = {
    for {
      dlcOpt <- dlcDAO.read(dlcId)

      dlcDb = dlcOpt match {
        case Some(db) =>
          require(
            DLCState.cancellableState.exists(_ == db.state),
            s"Cannot cancel a DLC after it has been signed, state=${db.state}"
          )
          db
        case None =>
          throw new IllegalArgumentException(
            s"No DLC Found with dlc id ${dlcId.hex}"
          )
      }
      _ = logger.info(
        s"Canceling DLC with tempContractId=${dlcDb.tempContractId.hex} dlcId=${dlcId.hex} contractId=${dlcDb.contractIdOpt}"
      )
      inputs <- dlcInputsDAO.findByDLCId(dlcId, dlcDb.isInitiator)
      dbs <- walletDAOs.utxoDAO.findByOutPoints(inputs.map(_.outPoint))
      // allow this to fail in the case they have already been unreserved
      _ <- utxoHandling.unmarkUTXOsAsReserved(dbs).recoverWith {
        case scala.util.control.NonFatal(_) => Future.successful(Vector.empty)
      }
      action = actionBuilder.deleteDLCAction(dlcId)
      _ <- safeDLCDatabase.run(action)
    } yield ()
  }

  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCOffer] = {
    chainQueryApi.getBestHashBlockHeight().flatMap { height =>
      createDLCOffer(
        contractInfo,
        collateral,
        feeRateOpt,
        locktime = UInt32(height),
        refundLT,
        peerAddressOpt,
        externalPayoutAddressOpt,
        externalChangeAddressOpt
      )
    }
  }

  /** Creates a DLCOffer, if one has already been created with the given
    * parameters then that one will be returned instead.
    *
    * This is the first step of the initiator
    */
  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLocktime: UInt32,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCOffer] = {
    logger.info("Creating DLC Offer")
    if (
      !walletConfig.allowExternalDLCAddresses && (externalPayoutAddressOpt.nonEmpty || externalChangeAddressOpt.nonEmpty)
    ) {
      return Future.failed(
        new IllegalArgumentException("External DLC addresses are not allowed")
      )
    }
    if (!validateAnnouncementSignatures(contractInfo.oracleInfos)) {
      return Future.failed(
        InvalidAnnouncementSignature(
          s"Contract info contains invalid announcement signature(s)"
        )
      )
    }

    val announcements =
      contractInfo.oracleInfos.head.singleOracleInfos.map(_.announcement)

    val feeRateF = determineFeeRate(feeRateOpt).map { fee =>
      SatoshisPerVirtualByte(fee.currencyUnit)
    }

    for {
      feeRate <- feeRateF
      // hack for now to get around https://github.com/bitcoin-s/bitcoin-s/issues/3127
      // filter announcements that we already have in the db
      groupedAnnouncements <- groupByExistingAnnouncements(announcements)
      announcementDataDbs <- announcementDAO.createAll(
        groupedAnnouncements.newAnnouncements
      )
      allAnnouncementDbs =
        announcementDataDbs ++ groupedAnnouncements.existingAnnouncements

      newAnnouncements = announcements.filter(a =>
        groupedAnnouncements.newAnnouncements.exists(
          _.announcementSignature == a.announcementSignature
        ))

      newAnnouncementsWithId = newAnnouncements.map { tlv =>
        val idOpt: Option[Long] =
          announcementDataDbs
            .find(_.announcementSignature == tlv.announcementSignature)
            .flatMap(_.id)
        (tlv, idOpt.get)
      }
      nonceDbs = OracleNonceDbHelper.fromAnnouncements(newAnnouncementsWithId)
      _ <- oracleNonceDAO.createAll(nonceDbs)
      chainType = HDChainType.External

      account <- accountHandling.getDefaultAccount()
      nextIndex <- walletApi.addressHandling.getNextAvailableIndex(account,
                                                                   chainType)
      _ <- writeDLCKeysToAddressDb(account, chainType, nextIndex)

      fundRawTxHelper <- walletApi.fundTxHandling.fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        fromTagOpt = None,
        markAsReserved = true
      )

      spendingInfos = fundRawTxHelper.scriptSigParams
      serialIds = DLCMessage.genSerialIds(spendingInfos.size)
      utxos = spendingInfos.zip(serialIds).map { case (utxo, id) =>
        DLCFundingInput.fromInputSigningInfo(
          utxo,
          id,
          TransactionConstants.enableRBFSequence
        )
      }

      dlcId = calcDLCId(utxos.map(_.outPoint))
      dlcAnnouncementDbs = allAnnouncementDbs.zipWithIndex.map {
        case (a, index) =>
          DLCAnnouncementDb(
            dlcId = dlcId,
            announcementId = a.id.get,
            index = index,
            used = false
          )
      }

      txBuilder = fundRawTxHelper.txBuilderWithFinalizer
      changeAddr = externalChangeAddressOpt.getOrElse {
        val changeSPK = txBuilder.finalizer.changeSPK
        BitcoinAddress.fromScriptPubKey(changeSPK, networkParameters)
      }

      dlcPubKeys = DLCUtil.calcDLCPubKeys(
        xpub = account.xpub,
        chainType = chainType,
        keyIndex = nextIndex,
        networkParameters = networkParameters,
        externalPayoutAddressOpt = externalPayoutAddressOpt
      )

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${dlcId.hex}"
      )

      payoutSerialId = DLCMessage.genSerialId()
      changeSerialId = DLCMessage.genSerialId()
      fundOutputSerialId = DLCMessage.genSerialId(Vector(changeSerialId))

      timeouts = DLCTimeouts(
        BlockTimeStamp(locktime),
        BlockTimeStamp(refundLocktime)
      )

      isExternalAddress <- addressDAO
        .findAddress(dlcPubKeys.payoutAddress)
        .map(_.isEmpty)

      offer = DLCOffer(
        protocolVersionOpt = DLCOfferTLV.currentVersionOpt,
        contractInfo = contractInfo,
        pubKeys = dlcPubKeys,
        collateral = collateral.satoshis,
        fundingInputs = utxos,
        changeAddress = changeAddr,
        payoutSerialId = payoutSerialId,
        changeSerialId = changeSerialId,
        fundOutputSerialId = fundOutputSerialId,
        feeRate = feeRate,
        timeouts = timeouts,
        isExternalAddress = isExternalAddress
      )

      oracleParamsOpt = OracleInfo.getOracleParamsOpt(
        contractInfo.oracleInfos.head
      )

      dlcDb = DLCDb(
        dlcId = dlcId,
        tempContractId = offer.tempContractId,
        contractIdOpt = None,
        protocolVersion = 0,
        state = DLCState.Offered,
        isInitiator = true,
        account = account.hdAccount,
        changeIndex = chainType,
        keyIndex = nextIndex,
        feeRate = feeRate,
        fundOutputSerialId = fundOutputSerialId,
        lastUpdated = TimeUtil.now,
        fundingOutPointOpt = None,
        fundingTxIdOpt = None,
        closingTxIdOpt = None,
        aggregateSignatureOpt = None,
        serializationVersion = contractInfo.serializationVersion,
        peerOpt = peerAddressOpt.map(a => a.getHostString + ":" + a.getPort)
      )

      contractDataDb = DLCContractDataDb(
        dlcId = dlcId,
        oracleThreshold = contractInfo.oracleInfos.head.threshold,
        oracleParamsTLVOpt = oracleParamsOpt,
        contractDescriptorTLV = contractInfo.contractDescriptors.head.toTLV,
        contractMaturity = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout,
        totalCollateral = contractInfo.totalCollateral
      )
      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(dlcId, offer)

      dlcInputs = spendingInfos.zip(utxos).zipWithIndex.map {
        case ((utxo, fundingInput), idx) =>
          DLCFundingInputDb(
            dlcId = dlcId,
            isInitiator = true,
            index = idx,
            inputSerialId = fundingInput.inputSerialId,
            outPoint = utxo.outPoint,
            output = utxo.output,
            nSequence = fundingInput.sequence,
            maxWitnessLength = fundingInput.maxWitnessLen.toLong,
            redeemScriptOpt = InputInfo.getRedeemScript(utxo.inputInfo),
            witnessScriptOpt = InputInfo.getScriptWitness(utxo.inputInfo)
          )
      }
      _ = logger.info(
        s"Created offer with tempContractId ${offer.tempContractId.hex}"
      )

      offerActions = actionBuilder.buildCreateOfferAction(
        dlcDb = dlcDb,
        contractDataDb = contractDataDb,
        dlcAnnouncementDbs = dlcAnnouncementDbs,
        dlcInputs = dlcInputs,
        dlcOfferDb = dlcOfferDb
      )

      _ <- safeDLCDatabase.run(offerActions)
      status <- findDLC(dlcId)
      _ <- dlcConfig.walletCallbacks.executeOnDLCStateChange(status.get)
    } yield offer
  }

  private def initDLCForAccept(
      offer: DLCOffer,
      account: AccountDb,
      fundRawTxHelper: FundRawTxHelper[ShufflingNonInteractiveFinalizer],
      collateral: CurrencyUnit,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[InitializedAccept] = {
    logger.info(
      s"Initializing DLC from received offer with tempContractId ${offer.tempContractId.hex}"
    )
    val dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))
    val contractInfo = offer.contractInfo
    val dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(dlcId, offer)
    val announcements =
      offer.contractInfo.oracleInfos.head.singleOracleInfos
        .map(_.announcement)

    val chainType = HDChainType.External

    dlcDataManagement.getDLCFundingData(dlcId, txDAO = transactionDAO).flatMap {
      case Some(dlcDbSetupState) =>
        dlcDbSetupState match {
          case _: OfferedDbState =>
            Future.failed(
              new RuntimeException(
                s"We cannot accept a DLC we offered, dlcId=${dlcId.hex}"
              )
            )
          case _: SignDbState =>
            Future.failed(
              new RuntimeException(
                s"We cannot accept a DLC we have already signed, dlcId=${dlcId.hex}"
              )
            )
          case a: AcceptDbState =>
            val dlcPubKeys = DLCUtil.calcDLCPubKeys(
              xpub = account.xpub,
              chainType = chainType,
              keyIndex = a.dlcDb.keyIndex,
              networkParameters = networkParameters,
              externalPayoutAddressOpt = externalPayoutAddressOpt
            )
            val initAccept = InitializedAccept(
              dlc = a.dlcDb,
              offerDb = a.offerDb,
              acceptDb = a.acceptDb,
              fundingInputsDb = a.acceptFundingInputsDb,
              pubKeys = dlcPubKeys,
              contractDataDb = a.contractDataDb,
              acceptWithoutSigs = a.acceptWithoutSigs
            )

            Future.successful(initAccept)
        }
      case None =>
        val nextIndexF =
          walletApi.addressHandling.getNextAvailableIndex(account, chainType)
        val acceptWithoutSigsWithKeysF
            : Future[(DLCAcceptWithoutSigs, DLCPublicKeys)] =
          nextIndexF.map { nextIndex =>
            DLCAcceptUtil.buildAcceptWithoutSigs(
              keyIndex = nextIndex,
              chainType = chainType,
              offer = offer,
              fundRawTxHelper = fundRawTxHelper,
              account = account,
              fundingPrivKey = getFundingPrivKey(account, nextIndex),
              collateral = collateral,
              networkParameters = networkParameters,
              externalPayoutAddressOpt = externalPayoutAddressOpt,
              externalChangeAddressOpt = externalChangeAddressOpt
            )
          }

        for {
          (dlcAcceptWithoutSigs, dlcPubKeys) <- acceptWithoutSigsWithKeysF
          nextIndex <- nextIndexF
          contractId = DLCUtil.calcContractId(offer, dlcAcceptWithoutSigs)
          dlc = DLCAcceptUtil.buildAcceptDlcDb(
            offer,
            dlcId,
            Some(contractId),
            account,
            chainType,
            nextIndex,
            contractInfo,
            peerOpt = peerAddressOpt.map(a => a.getHostString + ":" + a.getPort)
          )
          acceptDb = DLCAcceptUtil.buildAcceptDb(
            dlc = dlc,
            acceptWithoutSigs = dlcAcceptWithoutSigs,
            dlcPubKeys = dlcPubKeys,
            collateral = collateral
          )
          acceptInputs = fundRawTxHelper.scriptSigParams
            .zip(dlcAcceptWithoutSigs.fundingInputs)
            .zipWithIndex
            .map { case ((utxo, fundingInput), idx) =>
              DLCFundingInputDb(
                dlcId = dlc.dlcId,
                isInitiator = false,
                index = idx,
                inputSerialId = fundingInput.inputSerialId,
                outPoint = utxo.outPoint,
                output = utxo.output,
                nSequence = fundingInput.sequence,
                maxWitnessLength = fundingInput.maxWitnessLen.toLong,
                redeemScriptOpt = InputInfo.getRedeemScript(utxo.inputInfo),
                witnessScriptOpt = InputInfo.getScriptWitness(utxo.inputInfo)
              )
            }
          contractDataDb = DLCAcceptUtil
            .buildAcceptContractDataDb(contractInfo, dlcId, offer)
          _ <- writeDLCKeysToAddressDb(account, chainType, nextIndex)
          groupedAnnouncements <- groupByExistingAnnouncements(announcements)
          dlcDbAction = dlcDAO.createAction(dlc)
          dlcOfferAction = dlcOfferDAO.createAction(dlcOfferDb)
          acceptAction = dlcAcceptDAO.createAction(acceptDb)
          inputsAction = dlcInputsDAO.upsertAllAction(acceptInputs)
          contractAction = contractDataDAO.createAction(contractDataDb)
          createdAnnouncementsAction = announcementDAO
            .createAllAction(groupedAnnouncements.newAnnouncements)
          zipped = {
            for {
              dlcDb <- dlcDbAction
              ann <- createdAnnouncementsAction
              // we don't need the contract data db, so don't return it
              contractDataDb <- contractAction
              offer <- dlcOfferAction
              accept <- acceptAction
              inputs <- inputsAction
            } yield (dlcDb, ann, offer, accept, inputs, contractDataDb)
          }
          (
            writtenDLC,
            createdDbs,
            offerDb,
            acceptDb,
            inputsDb,
            contractDataDb
          ) <-
            safeDLCDatabase.run(zipped)
          announcementDataDbs =
            createdDbs ++ groupedAnnouncements.existingAnnouncements

          newAnnouncements = announcements.filter(a =>
            groupedAnnouncements.newAnnouncements
              .exists(_.announcementSignature == a.announcementSignature))

          newAnnouncementsWithId = newAnnouncements.map { tlv =>
            val idOpt = createdDbs
              .find(_.announcementSignature == tlv.announcementSignature)
              .flatMap(_.id)
            (tlv, idOpt.get)
          }
          nonceDbs = OracleNonceDbHelper
            .fromAnnouncements(newAnnouncementsWithId)
          createNonceAction = oracleNonceDAO.createAllAction(nonceDbs)

          dlcAnnouncementDbs = announcementDataDbs.zipWithIndex.map {
            case (a, index) =>
              DLCAnnouncementDb(
                dlcId = dlcId,
                announcementId = a.id.get,
                index = index,
                used = false
              )
          }
          createAnnouncementAction = dlcAnnouncementDAO
            .createAllAction(dlcAnnouncementDbs)

          _ <- safeDLCDatabase
            .run(DBIOAction.seq(createNonceAction, createAnnouncementAction))
        } yield {
          InitializedAccept(
            dlc = writtenDLC,
            offerDb = offerDb,
            acceptDb = acceptDb,
            fundingInputsDb = inputsDb,
            pubKeys = dlcPubKeys,
            contractDataDb = contractDataDb,
            acceptWithoutSigs = dlcAcceptWithoutSigs
          )
        }
    }
  }

  /** Creates a DLCAccept from the default Segwit account from a given offer, if
    * one has already been created with the given parameters then that one will
    * be returned instead.
    *
    * This is the first step of the recipient
    */
  override def acceptDLCOffer(
      offer: DLCOffer,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCAccept] = {
    logger.debug("Calculating relevant wallet data for DLC Accept")
    if (
      !walletConfig.allowExternalDLCAddresses && (externalPayoutAddressOpt.nonEmpty || externalChangeAddressOpt.nonEmpty)
    ) {
      return Future.failed(
        new IllegalArgumentException("External DLC addresses are not allowed")
      )
    }
    if (!validateAnnouncementSignatures(offer.oracleInfos)) {
      return Future.failed(
        InvalidAnnouncementSignature(
          s"Offer ${offer.tempContractId.hex} contains invalid announcement signature(s)"
        )
      )
    }

    val dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

    val collateral = offer.contractInfo.totalCollateral - offer.collateral
    logger.debug(s"Checking if Accept (${dlcId.hex}) has already been made")
    for {
      dlcAcceptOpt <- DLCAcceptUtil.findDLCAccept(
        dlcId = dlcId,
        offer = offer,
        dlcWalletDAOs = dlcWalletDAOs,
        transactionDAO = transactionDAO
      )
      dlcAccept <- {
        dlcAcceptOpt match {
          case Some(accept) => Future.successful(accept)
          case None =>
            createNewDLCAccept(
              collateral,
              offer,
              peerAddressOpt,
              externalPayoutAddressOpt,
              externalChangeAddressOpt
            )
        }
      }
      status <- findDLC(dlcId)
      _ <- dlcConfig.walletCallbacks.executeOnDLCStateChange(status.get)
    } yield dlcAccept
  }

  private def validateAnnouncementSignatures(
      oracleInfos: Vector[OracleInfo]
  ): Boolean = {
    oracleInfos.forall(infos =>
      infos.singleOracleInfos.forall(_.announcement.validateSignature))
  }

  private def fundDLCAcceptMsg(
      offer: DLCOffer,
      collateral: CurrencyUnit,
      account: AccountDb
  ): Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
    val txBuilderAndSpendingInfosF
        : Future[FundRawTxHelper[ShufflingNonInteractiveFinalizer]] = {
      for {
        fundRawTxHelper <- walletApi.fundTxHandling.fundRawTransactionInternal(
          destinations =
            Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
          feeRate = offer.feeRate,
          fromAccount = account,
          fromTagOpt = None,
          markAsReserved = true
        )
      } yield fundRawTxHelper
    }
    txBuilderAndSpendingInfosF
  }

  private def getFundingPrivKey(
      account: AccountDb,
      keyIndex: Int
  ): AdaptorSign = {
    val bip32Path = BIP32Path(
      account.hdAccount.path ++ Vector(
        BIP32Node(0, hardenedOpt = None),
        BIP32Node(keyIndex, hardenedOpt = None)
      )
    )
    val privKeyPath = HDPath.fromString(bip32Path.toString)
    walletApi.keyManager.toSign(privKeyPath)
  }

  private def createNewDLCAccept(
      collateral: CurrencyUnit,
      offer: DLCOffer,
      peerAddressOpt: Option[java.net.InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCAccept] =
    Future {
      DLCWallet.AcceptingOffersLatch.startAccepting(offer.tempContractId)
      logger.info(
        s"Creating DLC Accept for tempContractId ${offer.tempContractId.hex}"
      )
      val result = for {
        account <- accountHandling.getDefaultAccount()
        fundRawTxHelper <- fundDLCAcceptMsg(
          offer = offer,
          collateral = collateral,
          account = account
        )

        initializedAccept <-
          initDLCForAccept(
            offer = offer,
            account = account,
            fundRawTxHelper = fundRawTxHelper,
            collateral = collateral,
            externalPayoutAddressOpt = externalPayoutAddressOpt,
            externalChangeAddressOpt = externalChangeAddressOpt,
            peerAddressOpt = peerAddressOpt
          )
        _ = require(
          initializedAccept.acceptWithoutSigs.tempContractId == offer.tempContractId,
          s"Offer and Accept have differing tempContractIds! offer=${offer.tempContractId} accept=${initializedAccept.acceptWithoutSigs.tempContractId}"
        )
        offerPrevTxs = offer.fundingInputs.map(funding =>
          TransactionDbHelper.fromTransaction(
            funding.prevTx,
            blockHashOpt = None
          ))
        _ <- remoteTxDAO.upsertAll(offerPrevTxs)

        fundingPrivKey = getFundingPrivKey(
          account,
          initializedAccept.dlc.keyIndex
        )

        builder = DLCTxBuilder(offer, initializedAccept.acceptWithoutSigs)

        contractId = builder.calcContractId

        signer = DLCTxSigner(
          builder = builder,
          isInitiator = false,
          fundingKey = fundingPrivKey,
          finalAddress = initializedAccept.pubKeys.payoutAddress,
          fundingUtxos = fundRawTxHelper.scriptSigParams
        )

        spkDb = ScriptPubKeyDb(builder.fundingSPK)
        // only update spk db if we don't have it
        _ <- scriptPubKeyDAO.createIfNotExists(spkDb)

        _ = logger.info(s"Creating CET Sigs for ${contractId.toHex}")
        // emit websocket event that we are now computing adaptor signatures
        isExternalAddress <- addressDAO
          .findAddress(initializedAccept.pubKeys.payoutAddress)
          .map(_.isEmpty)
        status = DLCStatusBuilder.buildInProgressDLCStatus(
          dlcDb = initializedAccept.dlc,
          contractInfo = offer.contractInfo,
          contractData = initializedAccept.contractDataDb,
          offerDb = initializedAccept.offerDb,
          payoutAddress = Some(
            PayoutAddress(
              initializedAccept.pubKeys.payoutAddress,
              isExternalAddress
            )
          )
        )
        _ = dlcConfig.walletCallbacks.executeOnDLCStateChange(status)
        cetSigs <- signer.createCETSigsAsync()
        refundSig = signer.signRefundTx
        dlc = initializedAccept.dlc
        _ = logger.debug(
          s"DLC Accept data collected, creating database entry, ${dlc.dlcId.hex}"
        )

        sigsDbs = cetSigs.outcomeSigs.zipWithIndex.map { case (sig, index) =>
          DLCCETSignaturesDb(dlc.dlcId, index = index, sig._1, sig._2, None)
        }

        refundSigsDb =
          DLCRefundSigsDb(dlc.dlcId, refundSig, None)

        offerInputs = offer.fundingInputs.zipWithIndex.map {
          case (funding, idx) =>
            DLCFundingInputDb(
              dlcId = dlc.dlcId,
              isInitiator = true,
              index = idx,
              inputSerialId = funding.inputSerialId,
              outPoint = funding.outPoint,
              output = funding.output,
              nSequence = funding.sequence,
              maxWitnessLength = funding.maxWitnessLen.toLong,
              redeemScriptOpt = funding.redeemScriptOpt,
              witnessScriptOpt = None
            )
        }

        accept =
          initializedAccept.acceptDb
            .toDLCAccept(
              tempContractId = dlc.tempContractId,
              fundingInputs = initializedAccept.acceptWithoutSigs.fundingInputs,
              outcomeSigs = cetSigs.outcomeSigs,
              refundSig = refundSig
            )
            .copy(isExternalAddress = status.payoutAddress.forall(_.isExternal))

        actions = actionBuilder.buildCreateAcceptAction(
          dlcDb = dlc.updateState(DLCState.Accepted),
          offerInputs = offerInputs,
          cetSigsDb = sigsDbs,
          refundSigsDb = refundSigsDb
        )
        _ <- safeDLCDatabase.run(actions)
        dlcDb <- updateDLCContractIds(offer, accept)
        _ = logger.info(
          s"Created DLCAccept for tempContractId ${offer.tempContractId.hex} with contract Id ${contractId.toHex}"
        )

        fundingTx = builder.buildFundingTx
        outPoint = TransactionOutPoint(
          fundingTx.txId,
          UInt32(builder.fundOutputIndex)
        )
        _ <- updateFundingOutPoint(dlcDb.contractIdOpt.get, outPoint)
      } yield accept
      result.onComplete(_ =>
        DLCWallet.AcceptingOffersLatch.doneAccepting(offer.tempContractId))
      result
    }.flatten

  def registerDLCAccept(
      accept: DLCAccept
  ): Future[(DLCDb, Vector[DLCCETSignaturesDb])] = {
    logger.info(
      s"Checking if DLC Accept with tempContractId ${accept.tempContractId.hex} has already been registered"
    )
    val dbsF = for {
      dlcDbOpt <- dlcDAO.findByTempContractId(accept.tempContractId)
      (dlcDb, acceptDbOpt) <- dlcDbOpt match {
        case Some(db) =>
          require(
            db.isInitiator,
            "Cannot call DLC Sign on our own DLC Accept message"
          )
          dlcAcceptDAO
            .findByDLCId(db.dlcId)
            .map(acceptDbOpt => (db, acceptDbOpt))
        case None =>
          Future.failed(
            new RuntimeException(
              s"No DLC Offer found with corresponding tempContractId ${accept.tempContractId.hex}, this wallet did not create the corresponding offer"
            )
          )
      }
    } yield (dlcDb, acceptDbOpt)

    dbsF.flatMap {
      case (dlc, None) =>
        require(
          dlc.isInitiator,
          s"We cannot register a DLCAccept if we are not the initiator, got $dlc"
        )

        logger.info(
          s"DLC Offer contractId=${dlc.contractIdOpt.map(_.toHex)} dlcId=(${dlc.dlcId.hex}) found, adding accept data"
        )

        val dlcId = dlc.dlcId
        lazy val dlcAcceptDb = DLCAcceptDbHelper.fromDLCAccept(dlcId, accept)
        lazy val acceptInputs = accept.fundingInputs.zipWithIndex.map {
          case (funding, idx) =>
            DLCFundingInputDb(
              dlcId = dlcId,
              isInitiator = false,
              index = idx,
              inputSerialId = funding.inputSerialId,
              outPoint = funding.outPoint,
              output = funding.output,
              nSequence = funding.sequence,
              maxWitnessLength = funding.maxWitnessLen.toLong,
              redeemScriptOpt = funding.redeemScriptOpt,
              witnessScriptOpt = None
            )
        }

        lazy val acceptPrevTxs = accept.fundingInputs.map { funding =>
          TransactionDbHelper.fromTransaction(
            funding.prevTx,
            blockHashOpt = None
          )
        }

        lazy val sigsDbs = accept.cetSigs.outcomeSigs.zipWithIndex.map {
          case (sig, index) =>
            DLCCETSignaturesDb(dlc.dlcId, index = index, sig._1, sig._2, None)
        }

        lazy val refundSigsDb =
          DLCRefundSigsDb(dlcId, accept.refundSig, None)

        logger.info(
          s"Verifying ${accept.cetSigs.outcomeSigs.size} CET Signatures"
        )
        for {
          isCETSigsValidOpt <- verifyCETSigs(accept)
          _ = if (!(isCETSigsValidOpt.getOrElse(false)))
            throw new IllegalArgumentException(
              s"CET sigs provided are not valid! got ${accept.cetSigs.outcomeSigs}"
            )
          isRefundSigValid <- verifyRefundSig(accept)
          _ = if (!(isRefundSigValid.getOrElse(false)))
            throw new IllegalArgumentException(
              s"Refund sig provided is not valid! got ${accept.refundSig}"
            )

          _ = logger.debug(
            s"CET Signatures for tempContractId ${accept.tempContractId.hex} were valid, adding to database"
          )

          remoteTxUpsertAction = remoteTxDAO.upsertAllAction(acceptPrevTxs)
          inputAction = dlcInputsDAO.upsertAllAction(acceptInputs)
          sigsAction = dlcSigsDAO.upsertAllAction(sigsDbs)
          refundSigAction = dlcRefundSigDAO.upsertAction(refundSigsDb)
          acceptDbAction = dlcAcceptDAO.upsertAction(dlcAcceptDb)

          actions = DBIO.sequence(
            Vector(
              remoteTxUpsertAction,
              inputAction,
              sigsAction,
              refundSigAction,
              acceptDbAction
            )
          )
          _ <- safeDLCDatabase.run(actions)

          // .get is safe here because we must have an offer if we have a dlcDAO
          offerDb <- dlcOfferDAO.findByDLCId(dlc.dlcId).map(_.head)
          offerInputs <-
            dlcInputsDAO.findByDLCId(dlc.dlcId, isInitiator = true)
          prevTxs <-
            transactionDAO.findByTxIdBEs(offerInputs.map(_.outPoint.txIdBE))

          contractData <- contractDataDAO.read(dlcId).map(_.get)
          (announcements, announcementData, nonceDbs) <- dlcDataManagement
            .getDLCAnnouncementDbs(dlcId)

          contractInfo = dlcDataManagement.getContractInfo(
            contractData,
            announcements,
            announcementData,
            nonceDbs
          )

          offer =
            offerDb.toDLCOffer(
              contractInfo,
              DLCTxUtil.matchPrevTxsWithInputs(offerInputs, prevTxs),
              dlc,
              contractData
            )

          dlcDb <- updateDLCContractIds(offer, accept)

          builder = DLCTxBuilder(offer, accept.withoutSigs)
          fundingTx = builder.buildFundingTx
          outPoint = TransactionOutPoint(
            fundingTx.txId,
            UInt32(builder.fundOutputIndex)
          )

          spkDb = ScriptPubKeyDb(builder.fundingSPK)
          // only update spk db if we don't have it
          spkDbOpt <- scriptPubKeyDAO.findScriptPubKey(spkDb.scriptPubKey)
          _ <- spkDbOpt match {
            case Some(_) => Future.unit
            case None    => scriptPubKeyDAO.create(spkDb)
          }

          updatedDLCDb <- dlcDAO.update(
            dlcDb
              .updateState(DLCState.Accepted)
              .updateFundingOutPoint(outPoint)
          )
        } yield (updatedDLCDb, sigsDbs)
      case (dlc, Some(_)) =>
        logger.info(
          s"DLC Accept contractId=(${dlc.contractIdOpt.get.toHex}) dlcId=${dlc.dlcId.hex} has already been registered"
        )
        dlcSigsDAO
          .findByDLCId(dlc.dlcId)
          .map(sigOpt => (dlc, sigOpt))
    }
  }

  override def signDLC(acceptTLV: DLCAcceptTLV): Future[DLCSign] = {
    val tempId = acceptTLV.tempContractId

    for {
      dlcDbOpt <- dlcDAO.findByTempContractId(tempId)
      dlcDb <- dlcDbOpt match {
        case Some(db) => Future.successful(db)
        case None =>
          Future.failed(
            new RuntimeException(
              s"No DLC found with corresponding tempContractId ${tempId.hex}, this wallet did not create the corresponding offer"
            )
          )
      }
      contractInfo <- dlcDataManagement.getContractInfo(dlcDb.dlcId)

      accept =
        DLCAccept.fromTLV(acceptTLV, walletConfig.network, contractInfo)
      dlcSign <- signDLC(accept)
    } yield dlcSign
  }

  /** Creates signatures for the DLCs CETs and Funding Inputs
    *
    * This is the second step of the initiator
    */
  override def signDLC(accept: DLCAccept): Future[DLCSign] = {
    logger.info(
      s"Received accept message, tempContractId=${accept.tempContractId.hex}"
    )
    for {
      (dlc, cetSigsDbs) <- registerDLCAccept(accept)
      // .get should be safe now
      contractId = dlc.contractIdOpt.get
      dlcId = dlc.dlcId
      fundingInputs <- dlcInputsDAO.findByDLCId(dlcId)
      scriptSigParams <- getScriptSigParams(dlc, fundingInputs)
      signerOpt <- dlcDataManagement.signerFromDb(
        dlcId = dlc.dlcId,
        transactionDAO = transactionDAO,
        fundingUtxoScriptSigParams = scriptSigParams,
        keyManager = walletApi.keyManager
      )

      mySigs <- dlcSigsDAO.findByDLCId(dlc.dlcId)
      refundSigsDb <- dlcRefundSigDAO.findByDLCId(dlc.dlcId).map(_.head)
      cetSigsOpt <- {
        signerOpt match {
          case Some(signer) =>
            val cetSigsF = getCetSigs(
              signer = signer,
              dlcDb = dlc,
              contractId = contractId,
              cetSigsDbs = cetSigsDbs,
              mySigs = mySigs
            )
            cetSigsF.map(Some(_))
          case None =>
            Future.successful(None)
        }
      }

      _ = logger.info(s"Creating funding sigs for ${contractId.toHex}")
      fundingSigsOpt <- {
        signerOpt match {
          case Some(signer) =>
            val fundingSignaturesT = signer.signFundingTx()
            Future
              .fromTry(fundingSignaturesT)
              .map(Some(_))
          case None =>
            Future.successful(None)
        }
      }

      // order fundingSigs by index
      inputDbs <- dlcInputsDAO.findByDLCId(dlc.dlcId, isInitiator = true)

      _ = require(
        fundingSigsOpt.isDefined,
        s"Cannot sign a DLC message when we cannot generate funding signatures, dlcId=${dlcId}"
      )
      fundingSigs = fundingSigsOpt.get
      cetSigs = cetSigsOpt.get
      sortedSigVec = inputDbs.sortBy(_.index).map { db =>
        val sig = fundingSigs(db.outPoint)
        (db.outPoint, sig)
      }

      updatedRefundSigsDb = refundSigsDb.copy(initiatorSig =
        signerOpt.map(_.signRefundTx))
      _ <- dlcRefundSigDAO.update(updatedRefundSigsDb)

      _ <- updateDLCState(dlc.contractIdOpt.get, DLCState.Signed)
      _ = logger.info(s"DLC ${contractId.toHex} is signed")
      status <- findDLC(dlcId)
      _ <- dlcConfig.walletCallbacks.executeOnDLCStateChange(status.get)
    } yield {
      logger.info(
        s"Done creating sign message with contractId=${contractId.toHex} tempContractId=${dlc.tempContractId.hex}"
      )
      // ?? is signer.signRefundTx persisted anywhere ??
      DLCSign(
        cetSigs,
        signerOpt.map(_.signRefundTx).get,
        FundingSignatures(sortedSigVec),
        contractId
      )
    }
  }

  private def getCetSigs(
      signer: DLCTxSigner,
      dlcDb: DLCDb,
      contractId: ByteVector,
      cetSigsDbs: Vector[DLCCETSignaturesDb],
      mySigs: Vector[DLCCETSignaturesDb]
  ): Future[CETSignatures] = {
    if (mySigs.forall(_.initiatorSig.isEmpty)) {
      logger.info(s"Creating CET Sigs for contract ${contractId.toHex}")
      val dlcDbSignComputingAdaptorSigs =
        dlcDb.updateState(DLCState.SignComputingAdaptorSigs)
      val updatedF = dlcDAO.update(dlcDbSignComputingAdaptorSigs)
      for {
        _ <- updatedF
        status <- findDLC(dlcDb.dlcId)
        _ <- dlcConfig.walletCallbacks.executeOnDLCStateChange(status.get)
        sigs <- signer.createCETSigsAsync()

        sigsDbs: Vector[DLCCETSignaturesDb] = sigs.outcomeSigs
          .zip(cetSigsDbs.sortBy(_.index))
          .map { case (sig, db) =>
            db.copy(initiatorSig = Some(sig._2))
          }
        _ <- dlcSigsDAO.updateAll(sigsDbs)
      } yield sigs

    } else {
      logger.debug(s"CET Sigs already created for ${contractId.toHex}")
      val outcomeSigs = mySigs.map { dbSig =>
        dbSig.sigPoint -> dbSig.initiatorSig.get
      }

      val signatures = CETSignatures(outcomeSigs)
      Future.successful(signatures)
    }
  }

  /** Verify CET sigs for the given accept message if it exists If it doesnt not
    * exist, return None
    */
  def verifyCETSigs(accept: DLCAccept): Future[Option[Boolean]] = {
    val verifierAcceptOptF =
      dlcDataManagement.verifierFromAccept(accept, transactionDAO)
    verifierAcceptOptF.flatMap {
      case Some(verifier) =>
        verifier
          .verifyCETSigs(accept.cetSigs.indexedOutcomeSigs)
          .map(Some(_))
      case None => Future.successful(None)
    }
  }

  /** Verify CET sigs for the given sign message if it exists If it doesnt not
    * exist, return None
    */
  def verifyCETSigs(sign: DLCSign): Future[Option[Boolean]] = {
    val verifierF =
      dlcDataManagement.verifierFromDb(sign.contractId, transactionDAO)
    verifierF.flatMap {
      case Some(verifier) =>
        val boolF = verifier.verifyCETSigs(sign.cetSigs.indexedOutcomeSigs)
        boolF.map(Some(_))
      case None =>
        Future.successful(None)
    }
  }

  def verifyRefundSig(accept: DLCAccept): Future[Option[Boolean]] = {
    val verifierOptF =
      dlcDataManagement.verifierFromAccept(accept, transactionDAO)
    verifierOptF.map {
      case Some(verifier) =>
        val bool = verifier.verifyRefundSig(accept.refundSig)
        Some(bool)
      case None => None
    }
  }

  def verifyRefundSig(sign: DLCSign): Future[Option[Boolean]] = {
    val verifierOptF = dlcDataManagement.verifierFromDb(
      contractId = sign.contractId,
      transactionDAO = transactionDAO
    )
    verifierOptF.map {
      case Some(verifier) =>
        val bool = verifier.verifyRefundSig(sign.refundSig)
        Some(bool)
      case None =>
        None
    }
  }

  def verifyFundingSigs(
      inputs: Vector[DLCFundingInputDb],
      sign: DLCSign
  ): Future[Option[Boolean]] = {
    if (inputs.count(_.isInitiator) == sign.fundingSigs.length) {
      val verifierOptF = dlcDataManagement.verifierFromDb(
        contractId = sign.contractId,
        transactionDAO = transactionDAO
      )
      verifierOptF.map {
        case Some(verifier) =>
          val bool = verifier.verifyRemoteFundingSigs(sign.fundingSigs)
          Some(bool)
        case None =>
          None
      }
    } else {
      logger.info(
        "Funding Signatures provided did not have the correct amount of inputs"
      )
      Future.successful(Some(false))
    }
  }

  /** Takes a DLCSign an inserts the funding signatures into the database This
    * is the only way one should insert sigs to the database
    */
  def addFundingSigs(sign: DLCSign): Future[Vector[DLCFundingInputDb]] = {
    for {
      dlc <- dlcDAO.findByContractId(sign.contractId).map(_.get)
      inputs <- dlcInputsDAO.findByDLCId(dlc.dlcId).map(_.sortBy(_.index))

      _ = logger.info(
        s"Verifying ${sign.fundingSigs.length} funding sigs for contract ${sign.contractId.toHex}"
      )
      isValidOpt <- verifyFundingSigs(inputs = inputs, sign = sign)
      _ <- {
        if (!(isValidOpt.getOrElse(false)))
          Future.failed(
            new IllegalArgumentException(
              s"Funding Signatures provided are not valid! got ${sign.fundingSigs}"
            )
          )
        else FutureUtil.unit
      }

      updatedInputs = sign.fundingSigs.map { case (outPoint, witness) =>
        inputs.find(_.outPoint == outPoint) match {
          case Some(inputDb) =>
            inputDb.copy(witnessScriptOpt = Some(witness))
          case None =>
            throw new NoSuchElementException(
              s"Received signature for outPoint (${outPoint.hex}) that does not correspond to this contractId (${sign.contractId.toHex})"
            )
        }
      }
      written <- dlcInputsDAO.upsertAll(updatedInputs.toVector)
    } yield written
  }

  override def addDLCSigs(signTLV: DLCSignTLV): Future[DLCDb] = {
    val contractId = signTLV.contractId

    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcDbOpt match {
        case Some(db) =>
          require(!db.isInitiator, "Cannot add DLC sigs as initiator")
          Future.successful(db)
        case None =>
          Future.failed(
            new RuntimeException(
              s"No DLC found with corresponding contractId ${contractId.toHex}"
            )
          )
      }
      builderOpt <- dlcDataManagement.builderFromDbData(
        dlcDb = dlcDb,
        transactionDAO = transactionDAO
      )

      _ = require(
        builderOpt.isDefined,
        s"Cannot add DLC sigs when the builder is not defined, dlcId=${dlcDb.dlcId.hex}"
      )
      builder = builderOpt.get
      sign = DLCSign.fromTLV(signTLV, builder.offer)
      result <- addDLCSigs(sign)
    } yield result
  }

  /** Inputs the received signatures for a DLC into our database
    *
    * This is the second step of the recipient
    */
  override def addDLCSigs(sign: DLCSign): Future[DLCDb] = {
    dlcDAO.findByContractId(sign.contractId).flatMap {
      case Some(dlc) =>
        require(!dlc.isInitiator, "Cannot add DLC sigs as initiator")
        dlc.state match {
          case Offered =>
            Future.failed(
              new RuntimeException(
                "Cannot add sigs to a DLC before it has been accepted"
              )
            )
          case _: AdaptorSigComputationState =>
            val err = new RuntimeException(
              s"Cannot add sigs to a DLC while adaptor sigs are being computed, contractId=${sign.contractId.toHex}"
            )
            Future.failed(err)
          case Accepted =>
            logger.info(
              s"Verifying CET Signatures for contract ${sign.contractId.toHex}"
            )
            for {
              isRefundSigValid <- verifyRefundSig(sign)
              _ = if (!(isRefundSigValid.getOrElse(false)))
                throw new IllegalArgumentException(
                  s"Refund sig provided is not valid! got ${sign.refundSig}"
                )

              isCETSigsValid <- verifyCETSigs(sign)
              _ = if (!(isCETSigsValid.getOrElse(false)))
                throw new IllegalArgumentException(
                  s"CET sigs provided are not valid! got ${sign.cetSigs.outcomeSigs}"
                )

              refundSigsDb <- dlcRefundSigDAO.findByDLCId(dlc.dlcId).map(_.head)
              sigsDbs <- dlcSigsDAO.findByDLCId(dlc.dlcId)

              updatedRefund = refundSigsDb
                .copy(initiatorSig = Some(sign.refundSig))
              updatedSigsDbs = sigsDbs
                .sortBy(_.index)
                .zip(sign.cetSigs.outcomeSigs)
                .map { case (db, (_, sig)) =>
                  db.copy(initiatorSig = Some(sig))
                }

              _ = logger.info(
                s"CET Signatures are valid for contract ${sign.contractId.toHex}"
              )

              _ <- addFundingSigs(sign)
              _ <- dlcSigsDAO.updateAll(updatedSigsDbs)
              _ <- dlcRefundSigDAO.update(updatedRefund)
              updated <- dlcDAO.update(dlc.updateState(DLCState.Signed))
              _ = logger.info(
                s"DLC ${sign.contractId.toHex} sigs are verified and stored, ready to broadcast"
              )
            } yield updated
          case _: DLCState.ClosedState | Broadcasted | Confirmed | Signed =>
            logger.info(
              s"DLC sigs already added for ${sign.contractId.toHex}, skipping.."
            )
            Future.successful(dlc)
        }
      case None =>
        Future.failed(
          new NoSuchElementException(
            s"No DLC found with corresponding contractId ${sign.contractId.toHex}"
          )
        )
    }
  }

  private def getScriptSigParams(
      dlcDb: DLCDb,
      fundingInputs: Vector[DLCFundingInputDb]
  ): Future[Vector[ScriptSignatureParams[InputInfo]]] = {
    transactionProcessing.getScriptSigParams(dlcDb, fundingInputs)
  }

  override def getDLCFundingTx(contractId: ByteVector): Future[Transaction] = {
    for {
      setupStateOpt <- dlcDataManagement.getDLCFundingData(
        contractId,
        txDAO = transactionDAO
      )
      complete = {
        setupStateOpt.map {
          case _: OfferedDbState =>
            sys.error(
              s"Cannot retrieve funding transaction when DLC is in offered state"
            )
          case complete: SetupCompleteDLCDbState => complete
        }.get // bad but going to have to save this refactor for future
      }
      dlcDb = complete.dlcDb
      // is this right? We don't have counterpart scriptSigParams
      fundingInputs = complete.allFundingInputs
      scriptSigParams <- getScriptSigParams(dlcDb, fundingInputs)
      signerOpt <- dlcDataManagement.signerFromDb(
        dlcDb = dlcDb,
        fundingUtxoScriptSigParams = scriptSigParams,
        transactionDAO = transactionDAO,
        keyManager = walletApi.keyManager
      )
      _ = require(
        signerOpt.isDefined,
        s"Cannot get dlc funding tx when signerOpt isn't defined, dlcId=${dlcDb.dlcId.hex}"
      )
      signer = signerOpt.get
      fundingTx <-
        if (dlcDb.isInitiator) {
          transactionDAO.findByTxId(signer.builder.buildFundingTx.txIdBE).map {
            case Some(txDb) => txDb.transaction
            case None =>
              signer.builder.buildFundingTx
          }
        } else {
          Future.fromTry {
            val remoteSigs = fundingInputs
              .filter(_.isInitiator)
              .map { input =>
                input.witnessScriptOpt match {
                  case Some(witnessScript) =>
                    witnessScript match {
                      case EmptyScriptWitness =>
                        throw new RuntimeException(
                          "Script witness cannot be empty"
                        )
                      case witness: ScriptWitnessV0 => (input.outPoint, witness)
                      case _: TaprootWitness =>
                        throw new UnsupportedOperationException(
                          s"Taproot not implemented"
                        )
                    }
                  case None => throw new RuntimeException("")
                }
              }
            signer.completeFundingTx(FundingSignatures(remoteSigs))
          }
        }
      _ = logger.info(
        s"Created funding transaction ${fundingTx.txIdBE.hex} for contract ${contractId.toHex}"
      )
    } yield fundingTx
  }

  override def broadcastDLCFundingTx(
      contractId: ByteVector
  ): Future[Transaction] = {
    logger.info(s"broadcasting $contractId")
    val dlcDbOptF = dlcDAO.findByContractId(contractId)
    val fundingTxF = getDLCFundingTx(contractId)
    for {
      dlcDbOpt <- dlcDbOptF
      _ = dlcDbOpt match {
        case None =>
          sys.error(
            s"Cannot broadcast DLC when we don't know the contract, given contractId=${contractId}"
          )
        case Some(dlcDb) =>
          isValidBroadcastState(dlcDb)
      }
      tx <- fundingTxF
      state <- updateDLCState(contractId, DLCState.Broadcasted)
      _ = logger.info(
        s"Broadcasting funding transaction ${tx.txIdBE.hex} for contract ${contractId.toHex}"
      )
      _ <- broadcastTransaction(tx)
      status <- findDLC(state.dlcId)
      _ <- dlcConfig.walletCallbacks.executeOnDLCStateChange(status.get)
      _ = logger.info(s"Done broadcast tx ${contractId}")
    } yield tx
  }

  /** Checks if the DLC is in a valid state to broadcast the funding tx. This is
    * particurarily useful for situations when users want to re-broadcast
    * funding txs. You should only be able to re-broadcast a funding tx in two
    * states, [[DLCState.Signed]] or [[DLCState.Broadcasted]] The reason
    * accepted is needed is that is the state the DLC is in when a user gives us
    * their sign message
    */
  private def isValidBroadcastState(dlcDb: DLCDb): DLCDb = {
    dlcDb.state match {
      case DLCState.Broadcasted | DLCState.Signed => dlcDb
      case state @ (DLCState.Offered | DLCState.Confirmed | DLCState.Accepted |
          DLCState.Claimed | DLCState.RemoteClaimed | DLCState.Refunded |
          _: DLCState.AdaptorSigComputationState) =>
        sys.error(
          s"Cannot broadcast the dlc when it is in the state=${state} contractId=${dlcDb.contractIdOpt}"
        )
    }
  }

  override def executeDLC(
      contractId: ByteVector,
      sigs: Seq[OracleAttestmentTLV]
  ): Future[Option[Transaction]] = {
    logger.info(
      s"Executing dlc with contractId=${contractId.toHex} sigs=${sigs.map(_.hex)}"
    )
    require(sigs.nonEmpty, "Must provide at least one oracle signature")
    val dlcDbOpt = dlcDAO.findByContractId(contractId)
    for {
      dlcDbOpt <- dlcDbOpt
      txOpt <- {
        dlcDbOpt match {
          case Some(dlcDb) =>
            executeDLC(dlcDb, sigs)
          case None =>
            Future.successful(None)
        }

      }
    } yield txOpt
  }

  def executeDLC(
      dlcDb: DLCDb,
      sigs: Seq[OracleAttestmentTLV]
  ): Future[Option[Transaction]] = {
    val _ = dlcDb.state match {
      case state @ (Offered | AcceptComputingAdaptorSigs | Accepted |
          SignComputingAdaptorSigs | Signed) =>
        sys.error(
          s"Cannot execute DLC before the DLC is broadcast to the blockchain, state=$state"
        )
      case Broadcasted | Confirmed | _: ClosedState =>
      // can continue executing, do nothing
    }
    for {
      (announcements, announcementData, nonceDbs) <- dlcDataManagement
        .getDLCAnnouncementDbs(dlcDb.dlcId)

      announcementTLVs = dlcDataManagement.getOracleAnnouncements(
        announcements,
        announcementData,
        nonceDbs
      )

      oracleSigs = DLCUtil.buildOracleSignaturesNaive(
        announcements = announcementTLVs,
        attestments = sigs.toVector
      )

      tx <- executeDLC(dlcDb.contractIdOpt.get, oracleSigs)
    } yield tx
  }

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]
  ): Future[Option[Transaction]] = {
    require(oracleSigs.nonEmpty, "Must provide at least one oracle signature")
    dlcDAO.findByContractId(contractId).flatMap {
      case None =>
        Future.failed(
          new IllegalArgumentException(
            s"No DLC found with contractId $contractId"
          )
        )
      case Some(db) =>
        db.closingTxIdOpt match {
          case Some(txId) =>
            transactionDAO.findByTxId(txId).flatMap {
              case Some(tx) => Future.successful(Some(tx.transaction))
              case None     => createDLCExecutionTx(contractId, oracleSigs)
            }
          case None =>
            createDLCExecutionTx(contractId, oracleSigs)
        }
    }
  }

  /** Returns a execution transaction if we can build one. Returns None if the
    * dlc db state is incorrect, or we have pruned CET signatures from the
    * database
    */
  private def createDLCExecutionTx(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]
  ): Future[Option[Transaction]] = {
    require(oracleSigs.nonEmpty, "Must provide at least one oracle signature")
    val setupStateOptF =
      dlcDataManagement.getDLCFundingData(contractId, txDAO = transactionDAO)
    setupStateOptF.flatMap {
      case None => Future.successful(None)
      case Some(setupDbState) =>
        setupDbState match {
          case o: OfferedDbState =>
            logger.info(
              s"Cannot create execution tx for dlc in state=${o.state}"
            )
            Future.successful(None)
          case c: SetupCompleteDLCDbState =>
            val dlcDb = c.dlcDb
            val fundingInputs = c.allFundingInputs
            val scriptSigParamsF = getScriptSigParams(dlcDb, fundingInputs)
            val executorWithSetupOptF = scriptSigParamsF.flatMap {
              scriptSigParams =>
                dlcDataManagement.executorAndSetupFromDb(
                  contractId = contractId,
                  txDAO = transactionDAO,
                  fundingUtxoScriptSigParams = scriptSigParams,
                  keyManager = walletApi.keyManager
                )
            }
            executorWithSetupOptF.flatMap {
              case Some(executorWithSetup) =>
                buildExecutionTxWithExecutor(
                  executorWithSetup,
                  oracleSigs,
                  contractId
                ).map(Some(_))
              case None =>
                // means we don't have cet sigs in the db anymore
                // can we retrieve the CET some other way?

                // lets try to retrieve it from our transactionDAO
                val dlcDbOptF = dlcDAO.findByContractId(contractId)

                for {
                  dlcDbOpt <- dlcDbOptF
                  _ = require(
                    dlcDbOpt.isDefined,
                    s"Could not find dlc associated with this contractId=${contractId.toHex}"
                  )
                  dlcDb = dlcDbOpt.get
                  _ = require(
                    dlcDb.closingTxIdOpt.isDefined,
                    s"If we don't have CET signatures, the closing tx must be defined, contractId=${contractId.toHex}, state=${dlcDb.state}"
                  )
                  closingTxId = dlcDb.closingTxIdOpt.get
                  closingTxOpt <- transactionDAO.findByTxId(closingTxId)
                } yield {
                  require(
                    closingTxOpt.isDefined,
                    s"Could not find closing tx for DLC in db, contactId=${contractId.toHex} closingTxId=${closingTxId.hex}"
                  )
                  Some(closingTxOpt.get.transaction)
                }
            }
        }
    }
  }

  private def buildExecutionTxWithExecutor(
      executorWithSetup: DLCExecutorWithSetup,
      oracleSigs: Vector[OracleSignatures],
      contractId: ByteVector
  ): Future[Transaction] = {
    val executor = executorWithSetup.executor
    val setup = executorWithSetup.setup
    val executed = executor.executeDLC(setup, oracleSigs)
    val (tx, outcome, sigsUsed) =
      (executed.cet, executed.outcome, executed.sigsUsed)
    logger.info(
      s"Created DLC execution transaction ${tx.txIdBE.hex} for contract ${contractId.toHex}"
    )

    for {
      _ <- updateDLCOracleSigs(sigsUsed)
      _ <- updateDLCState(contractId, DLCState.Claimed)
      dlcDb <- updateClosingTxId(contractId, tx.txIdBE)
      oracleSigSum =
        OracleSignatures.computeAggregateSignature(outcome, sigsUsed)
      aggSig = SchnorrDigitalSignature(
        outcome.aggregateNonce,
        oracleSigSum.fieldElement,
        hashTypeOpt = None
      )
      _ <- updateAggregateSignature(contractId, aggSig)

      _ <- transactionProcessing.processTransaction(tx, None)
      dlcStatusOpt <- findDLC(dlcId = dlcDb.dlcId)
      _ <- dlcConfig.walletCallbacks.executeOnDLCStateChange(dlcStatusOpt.get)
    } yield tx
  }

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get
      offerDbOpt <- dlcOfferDAO.findByDLCId(dlcDb.dlcId)
      _ = require(
        offerDbOpt.nonEmpty,
        s"Invalid DLC $dlcDb.dlcId: no offer data"
      )
      contractData <- contractDataDAO.read(dlcDb.dlcId).map(_.get)

      currentHeight <- chainQueryApi.getBestHashBlockHeight()
      _ = contractData.contractTimeout match {
        case BlockStamp.BlockHeight(height) =>
          require(
            currentHeight >= height,
            s"Refund transaction is not valid yet, current height: $currentHeight, refund valid at height $height"
          )
        case BlockStamp.BlockTime(time) =>
          val currentTime = TimeUtil.currentEpochSecond
          require(
            currentTime >= time.toLong,
            s"Refund transaction is not valid yet, current time: $currentTime, refund valid at time $time"
          )
      }

      fundingInputs <- dlcInputsDAO.findByDLCId(dlcDb.dlcId)
      scriptSigParams <- getScriptSigParams(dlcDb, fundingInputs)
      executorOpt <- dlcDataManagement.executorFromDb(
        dlcDb.dlcId,
        transactionDAO,
        scriptSigParams,
        walletApi.keyManager
      )
      _ = require(
        executorOpt.isDefined,
        s"Cannot execute refund transaction when the executor isn't defined, dlcId=${dlcDb.dlcId.hex}"
      )
      executor = executorOpt.get
      refundSigsDbOpt <- dlcRefundSigDAO.findByDLCId(dlcDb.dlcId)

      refundSig =
        if (dlcDb.isInitiator) refundSigsDbOpt.head.accepterSig
        else refundSigsDbOpt.head.initiatorSig.get

      refundTx = executor.executeRefundDLC(refundSig).refundTx
      _ = logger.info(
        s"Created DLC refund transaction ${refundTx.txIdBE.hex} for contract ${contractId.toHex}"
      )

      _ <- updateDLCState(contractId, DLCState.Refunded)
      _ <- updateClosingTxId(contractId, refundTx.txIdBE)

      _ <- transactionProcessing.processTransaction(refundTx,
                                                    blockHashWithConfsOpt =
                                                      None)
      status <- findDLC(dlcDb.dlcId)
      _ <- dlcConfig.walletCallbacks.executeOnDLCStateChange(status.get)
    } yield refundTx
  }

  override def getWalletAccounting(): Future[DLCWalletAccounting] = {
    val dlcsF = listDLCs(DLCState.closedStates)
    for {
      dlcs <- dlcsF
      closed = dlcs.collect { case c: ClaimedDLCStatus =>
        c
      } // only get claimed dlcs for accounting
      accountings = closed.map(_.accounting)
      walletAccounting = DLCWalletAccounting.fromDLCAccounting(accountings)
    } yield walletAccounting
  }

  override def listDLCs(states: Vector[DLCState]): Future[Vector[DLCStatus]] = {
    val dlcAction = for {
      dlcs <- dlcDAO.findByStatesAction(states)
      ids = dlcs.map(_.dlcId)
      dlcAs = ids.map(findDLCAction)
      dlcs <- DBIO.sequence(dlcAs)
    } yield {
      dlcs.collect { case Some(dlc) =>
        dlc
      }
    }
    fetchWalletDbInfo(dlcAction)
  }

  override def listDLCs(): Future[Vector[DLCStatus]] = {
    listDLCs(None)
  }

  override def listDLCsByContact(
      contactId: InetSocketAddress
  ): Future[Vector[DLCStatus]] = {
    listDLCs(Some(contactId))
  }

  private def listDLCs(
      contactIdOpt: Option[InetSocketAddress]
  ): Future[Vector[DLCStatus]] = {
    val dlcAction
        : DBIOAction[Vector[IntermediaryDLCStatus], NoStream, Effect.Read] =
      for {
        dlcs <- contactIdOpt match {
          case Some(contactId) =>
            dlcDAO.findByContactIdAction(
              contactId.getHostString + ":" + contactId.getPort
            )
          case None => dlcDAO.findAllAction()
        }
        dlcsResult <- findDLCStatusesAction(dlcs)
      } yield {
        dlcsResult
      }
    fetchWalletDbInfo(dlcAction)
  }

  private def fetchWalletDbInfo(
      dlcAction: DBIOAction[Vector[
                              IntermediaryDLCStatus
                            ],
                            NoStream,
                            Effect.Read]
  ): Future[Vector[DLCStatus]] = {
    safeDLCDatabase.run(dlcAction).flatMap { intermediaries =>
      val actions = intermediaries.map { intermediary =>
        getWalletDLCDbsAction(intermediary).map {
          case (closingTxOpt, payoutAddrOpt) =>
            intermediary.complete(payoutAddrOpt, closingTxOpt)
        }
      }
      walletDatabase.run(DBIO.sequence(actions))
    }
  }

  private def getClosingTxOptAction(
      dlcDb: DLCDb
  ): DBIOAction[Option[TransactionDb], NoStream, Effect.Read] = {
    dlcDb.closingTxIdOpt match {
      case None       => DBIOAction.successful(None)
      case Some(txid) => transactionDAO.findByTxIdAction(txid)
    }
  }

  override def getDLCOffer(dlcId: Sha256Digest): Future[Option[DLCOffer]] =
    dlcDataManagement.getOffer(dlcId, transactionDAO)

  override def findDLCByTemporaryContractId(
      tempContractId: Sha256Digest
  ): Future[Option[DLCStatus]] = {
    val start = System.currentTimeMillis()

    val dlcOptF = for {
      dlcDbOpt <- dlcDAO.findByTempContractId(tempContractId)
      dlcStatusOpt <- dlcDbOpt match {
        case None        => Future.successful(None)
        case Some(dlcDb) => findDLC(dlcDb.dlcId)
      }
    } yield dlcStatusOpt

    dlcOptF.foreach(_ =>
      logger.debug(
        s"Done finding tempContractId=$tempContractId, it took=${System
            .currentTimeMillis() - start}ms"
      ))
    dlcOptF
  }

  override def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]] = {
    val intermediaryF = safeDLCDatabase.run(findDLCAction(dlcId))

    intermediaryF.flatMap {
      case None => Future.successful(None)
      case Some(intermediary) =>
        val action = getWalletDLCDbsAction(intermediary)
        walletDatabase.run(action).map { case (closingTxOpt, payoutAddress) =>
          val res = intermediary.complete(payoutAddress, closingTxOpt)
          Some(res)
        }
    }
  }

  private def getWalletDLCDbsAction(intermediary: IntermediaryDLCStatus) = {
    val dlcDb = intermediary.dlcDb
    for {
      closingTxOpt <- getClosingTxOptAction(dlcDb)
      payoutAddress <- getPayoutAddressAction(
        dlcDb,
        intermediary.offerDb,
        intermediary.acceptDbOpt
      )
    } yield (closingTxOpt, payoutAddress)
  }

  private def findDLCAction(
      dlcDb: DLCDb
  ): DBIOAction[Option[IntermediaryDLCStatus], NoStream, Effect.Read] = {
    findDLCStatusAction(dlcDb)
  }

  private def findDLCAction(
      dlcId: Sha256Digest
  ): DBIOAction[Option[IntermediaryDLCStatus], NoStream, Effect.Read] = {
    val start = System.currentTimeMillis()

    val dlcOptA = for {
      dlcDbOpt <- dlcDAO.findByPrimaryKeyAction(dlcId)
      dlcStatusOpt <- dlcDbOpt match {
        case None        => DBIO.successful(None)
        case Some(dlcDb) => findDLCAction(dlcDb)
      }
    } yield dlcStatusOpt

    dlcOptA.map { res =>
      logger.debug(
        s"Done finding dlc=$dlcId, it took=${System.currentTimeMillis() - start}ms"
      )
      res
    }
  }

  private val mappers =
    new org.bitcoins.db.DbCommonsColumnMappers(dlcConfig.profile)

  import mappers._

  private def findDLCStatusAction(
      dlcDb: DLCDb
  ): DBIOAction[Option[IntermediaryDLCStatus], NoStream, Effect.Read] = {
    findDLCStatusesAction(Vector(dlcDb))
      .map(_.headOption)
  }

  private def findDLCStatusesAction(
      dlcDbs: Vector[DLCDb]
  ): DBIOAction[Vector[IntermediaryDLCStatus], NoStream, Effect.Read] = {
    val dlcIds = dlcDbs.map(_.dlcId)
    val contractDbsQ = contractDataDAO.findByPrimaryKeys(dlcIds)
    val offerDbsQ = dlcOfferDAO.findByPrimaryKeys(dlcIds)
    val acceptDbsQ = dlcAcceptDAO.findByPrimaryKeys(dlcIds)

    // optimization to use sql queries rather than action
    // as this method gets called a lot.
    val offerAndContractA
        : DBIOAction[Vector[
                       ((DLCOfferDb, DLCContractDataDb), Option[DLCAcceptDb])
                     ],
                     NoStream,
                     Effect.Read] = {
      offerDbsQ
        .join(contractDbsQ)
        .on(_.dlcId === _.dlcId)
        .joinLeft(acceptDbsQ)
        .on(_._1.dlcId === _.dlcId)
        .result
        .map(_.toVector)
    }

    val dlcDlbIds: Map[Sha256Digest, DLCDb] =
      dlcDbs.map(d => (d.dlcId, d)).toMap

    val retval
        : DBIOAction[Vector[IntermediaryDLCStatus], NoStream, Effect.Read] =
      for {
        offerAndContractDbs <- offerAndContractA
        result = {
          offerAndContractDbs.map {
            case ((offerDb, contractData), acceptDbOpt) =>
              val dlcId = offerDb.dlcId
              buildDLCStatusAction(
                dlcDlbIds(dlcId),
                contractData,
                offerDb,
                acceptDbOpt
              )
          }
        }
        seq <- DBIOAction.sequence(result).map(_.flatten)
      } yield seq

    retval
  }

  /** Helper method to assemble a [[DLCStatus]] */
  private def buildDLCStatusAction(
      dlcDb: DLCDb,
      contractData: DLCContractDataDb,
      offerDb: DLCOfferDb,
      acceptDbOpt: Option[DLCAcceptDb]
  ): DBIOAction[Option[IntermediaryDLCStatus], NoStream, Effect.Read] = {
    val dlcId = dlcDb.dlcId
    val aggregatedA =
      dlcDataManagement.getDLCAnnouncementDbsAction(dlcId)

    val contractInfoAndAnnouncementsA = {
      aggregatedA.map { case (announcements, announcementData, nonceDbs) =>
        val contractInfo = dlcDataManagement.getContractInfo(
          contractData,
          announcements,
          announcementData,
          nonceDbs
        )
        val announcementsWithId =
          dlcDataManagement.getOracleAnnouncementsWithId(
            announcements,
            announcementData,
            nonceDbs
          )
        (contractInfo, announcementsWithId)
      }
    }

    val statusA = for {
      (contractInfo, announcementsWithId) <- contractInfoAndAnnouncementsA
      (announcementIds, _, nonceDbs) <- aggregatedA
    } yield IntermediaryDLCStatus(
      dlcDb,
      contractInfo,
      contractData,
      offerDb,
      acceptDbOpt,
      nonceDbs,
      announcementsWithId,
      announcementIds
    )

    statusA.map(Some(_))
  }

  private def getPayoutAddressAction(
      dlcDb: DLCDb,
      offerDb: DLCOfferDb,
      acceptDbOpt: Option[DLCAcceptDb]
  ): DBIOAction[Option[PayoutAddress], NoStream, Effect.Read] = {
    val addressOpt = if (dlcDb.isInitiator) {
      Some(offerDb.payoutAddress)
    } else {
      acceptDbOpt.map(_.payoutAddress)
    }
    addressOpt match {
      case None => DBIOAction.successful(None)
      case Some(address) =>
        for {
          isExternal <- addressDAO.findAddressAction(address).map(_.isEmpty)
        } yield Some(PayoutAddress(address, isExternal))
    }
  }

  /** @param newAnnouncements
    *   announcements we do not have in our db
    * @param existingAnnouncements
    *   announcements we already have in our db
    */
  private case class AnnouncementGrouping(
      newAnnouncements: Vector[OracleAnnouncementDataDb],
      existingAnnouncements: Vector[OracleAnnouncementDataDb]
  ) {
    require(existingAnnouncements.forall(_.id.isDefined))
    require(
      newAnnouncements.forall(_.id.isEmpty),
      s"announcmeent had id defined=${newAnnouncements.map(_.id)}"
    )
  }

  /** This is needed because our upserts do not work we need to filter
    * announcements we already have in the database to avoid issues below
    * @see
    *   https://github.com/bitcoin-s/bitcoin-s/issues/1623
    * @see
    *   https://github.com/bitcoin-s/bitcoin-s/issues/3127
    * @param announcementDataDbs
    */
  private def groupByExistingAnnouncements(
      announcementTLVs: Vector[OracleAnnouncementTLV]
  ): Future[AnnouncementGrouping] = {

    val announcementSignatures: Vector[SchnorrDigitalSignature] = {
      announcementTLVs.map(a => a.announcementSignature)
    }

    val existingAnnouncementsInDbF: Future[Vector[OracleAnnouncementDataDb]] =
      announcementDAO
        .findByAnnouncementSignatures(announcementSignatures)

    for {
      existingAnnouncementsDb <- existingAnnouncementsInDbF
      newAnnouncements = announcementTLVs.filterNot(a =>
        existingAnnouncementsDb.exists(
          _.announcementSignature == a.announcementSignature
        ))
    } yield {
      val newAnnouncementsDb =
        OracleAnnouncementDbHelper.fromAnnouncements(newAnnouncements)

      AnnouncementGrouping(
        newAnnouncements = newAnnouncementsDb,
        existingAnnouncements = existingAnnouncementsDb
      )
    }
  }

  private def determineFeeRate(feeRateOpt: Option[FeeUnit]): Future[FeeUnit] =
    feeRateOpt match {
      case None =>
        feeRateApi.getFeeRate()
      case Some(feeRate) =>
        Future.successful(feeRate)
    }

  override def broadcastTransaction(transaction: Transaction): Future[Unit] = {
    walletApi.broadcastTransaction(transaction)
  }

  override def processCompactFilters(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)])
      : Future[NeutrinoHDWalletApi] =
    walletApi.processCompactFilters(blockFilters)

  override def accountHandling: AccountHandlingApi = walletApi.accountHandling

  override def fundTxHandling: FundTransactionHandlingApi =
    walletApi.fundTxHandling

  override def addressHandling: AddressHandlingApi = walletApi.addressHandling

  override def utxoHandling: UtxoHandlingApi = walletApi.utxoHandling

  override def sendFundsHandling: SendFundsHandlingApi =
    walletApi.sendFundsHandling

  override val nodeApi: NodeApi = walletApi.nodeApi
  override val chainQueryApi: ChainQueryApi = walletApi.chainQueryApi

  override def feeRateApi: FeeRateApi = walletApi.feeRateApi

  override val creationTime: Instant = walletApi.creationTime

  /** Gets the sum of all confirmed UTXOs in this wallet */
  override def getConfirmedBalance(): Future[CurrencyUnit] =
    walletApi.getConfirmedBalance()

  override def getNewAddress(): Future[BitcoinAddress] =
    walletApi.getNewAddress()

  override def getNewChangeAddress(): Future[BitcoinAddress] =
    walletApi.getNewChangeAddress()

  /** Gets the sum of all unconfirmed UTXOs in this wallet */
  override def getUnconfirmedBalance(): Future[CurrencyUnit] =
    walletApi.getUnconfirmedBalance()

  /** Checks if the wallet contains any data */
  override def isEmpty(): Future[Boolean] = walletApi.isEmpty()

  override def getSyncState(): Future[BlockSyncState] = walletApi.getSyncState()

  override def isRescanning(): Future[Boolean] = walletApi.isRescanning()

  override def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] =
    walletApi.getSyncDescriptorOpt()

  override def getWalletName(): Future[String] = walletApi.getWalletName()

  override def getInfo(): Future[WalletInfo] = walletApi.getInfo()
}

object DLCWallet extends WalletLogger {

  case class DuplicateOfferException(message: String)
      extends RuntimeException(message)

  case class InvalidAnnouncementSignature(message: String)
      extends RuntimeException(message)

  private object AcceptingOffersLatch {

    private val tempContractIds =
      new java.util.concurrent.ConcurrentHashMap[Sha256Digest, Sha256Digest]()

    def startAccepting(tempContractId: Sha256Digest): Unit = {
      if (tempContractIds.putIfAbsent(tempContractId, tempContractId) != null) {
        throw DuplicateOfferException(
          s"Offer with temporary contract ID ${tempContractId.hex} is already being accepted"
        )
      }
    }

    def doneAccepting(tempContractId: Sha256Digest): Unit = {
      val _ = tempContractIds.remove(tempContractId)
    }

  }
}
