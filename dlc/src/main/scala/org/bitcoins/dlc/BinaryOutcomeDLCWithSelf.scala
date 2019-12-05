package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  ECPublicKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrDigitalSignature
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.hd.{BIP32Node, BIP32Path}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.BlockStamp.{BlockHeight, BlockTime}
import org.bitcoins.core.protocol.BlockStampWithFuture
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  ConditionalScriptPubKey,
  MultiSignatureScriptPubKey,
  MultiSignatureWithTimeoutScriptPubKey,
  P2PKHScriptPubKey,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil, FutureUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  ConditionalPath,
  ConditionalSpendingInfo,
  MultiSignatureSpendingInfo,
  P2PKHSpendingInfo
}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** This case class allows for the construction and execution of binary outcome
  * Discreet Log Contracts between one party and itself. In the future this will
  * be split amongst two separate parties. This change will likely require that
  * this class be largely altered.
  *
  * The "two parties", which are actually just one node taking both positions, are
  * referred to as Local and Remote. The two outcomes are called Win and Lose but
  * note that Win refers to the case where Local wins money and Remote loses money.
  * Likewise Lose refers to the case where Remote wins and Local loses money.
  *
  * @param outcomeWin The String whose hash is signed by the oracle in the Win case
  * @param outcomeLose The String whose hash is signed by the oracle in the Lose case
  * @param oraclePubKey The Oracle's permanent public key
  * @param preCommittedR The Oracle's one-time event-specific public key
  * @param localExtPrivKey Local's extended private key for this event
  * @param remoteExtPrivKey Remote's extended private key for this event
  * @param localInput Local's total collateral contribution
  * @param remoteInput Remote's total collateral contribution
  * @param localFundingUtxos Local's funding BitcoinUTXOSpendingInfo collection
  * @param remoteFundingUtxos Remote's funding BitcoinUTXOSpendingInfo collection
  * @param localWinPayout Local's payout in the Win case
  * @param localLosePayout Local's payout in the Lose case
  * @param timeout The CLTV timeout in milliseconds used in all CETs
  * @param feeRate The predicted fee rate used for all transactions
  * @param changeSPK The place-holder change ScriptPubKey used for all transactions
  */
case class BinaryOutcomeDLCWithSelf(
    outcomeWin: String,
    outcomeLose: String,
    oraclePubKey: ECPublicKey,
    preCommittedR: ECPublicKey,
    localExtPrivKey: ExtPrivateKey,
    remoteExtPrivKey: ExtPrivateKey,
    localInput: CurrencyUnit,
    remoteInput: CurrencyUnit,
    localFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    remoteFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    localWinPayout: CurrencyUnit,
    localLosePayout: CurrencyUnit,
    timeout: BlockStampWithFuture,
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    network: BitcoinNetwork)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  import BinaryOutcomeDLCWithSelf.subtractFeeAndSign

  /** Hash signed by oracle in Win case */
  val messageWin: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip.bytes

  /** Hash signed by oracle in Lose case */
  val messageLose: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip.bytes

  /** sig*G in the Win case */
  val sigPubKeyWin: ECPublicKey =
    Schnorr.computePubKey(messageWin, preCommittedR, oraclePubKey)

  /** sig*G in the Lose case */
  val sigPubKeyLose: ECPublicKey =
    Schnorr.computePubKey(messageLose, preCommittedR, oraclePubKey)

  val fundingLocalPrivKey: ECPrivateKey =
    localExtPrivKey.deriveChildPrivKey(UInt32(0)).key

  val fundingRemotePrivKey: ECPrivateKey =
    remoteExtPrivKey.deriveChildPrivKey(UInt32(0)).key

  val finalLocalPrivKey: ECPrivateKey =
    localExtPrivKey.deriveChildPrivKey(UInt32(2)).key

  val finalRemotePrivKey: ECPrivateKey =
    remoteExtPrivKey.deriveChildPrivKey(UInt32(2)).key

  /** The derivation index for the win and lose cases respectively.
    * We assign this index based on lexicographical order to keep things deterministic.
    */
  private val (winIndex, loseIndex) =
    if (outcomeWin.compareTo(outcomeLose) > 0) {
      (1, 2)
    } else {
      (2, 1)
    }

  def cetPrivKey(rootKey: ExtPrivateKey, index: Int): ECPrivateKey = {
    rootKey
      .deriveChildPrivKey(
        BIP32Path(BIP32Node(1, hardened = false),
                  BIP32Node(index, hardened = false)))
      .key
  }

  val cetLocalRefundPrivKey: ECPrivateKey =
    cetPrivKey(localExtPrivKey, index = 0)
  val cetLocalWinPrivKey: ECPrivateKey = cetPrivKey(localExtPrivKey, winIndex)
  val cetLocalLosePrivKey: ECPrivateKey = cetPrivKey(localExtPrivKey, loseIndex)

  val cetRemoteRefundPrivKey: ECPrivateKey =
    cetPrivKey(remoteExtPrivKey, index = 0)
  val cetRemoteWinPrivKey: ECPrivateKey = cetPrivKey(remoteExtPrivKey, winIndex)

  val cetRemoteLosePrivKey: ECPrivateKey =
    cetPrivKey(remoteExtPrivKey, loseIndex)

  /** Total funding amount */
  private val totalInput = localInput + remoteInput

  /** Remote's payout in the Win case (in which Remote loses) */
  val remoteWinPayout: CurrencyUnit = totalInput - localWinPayout

  /** Remote's payout in the Lose case (in which Remote wins) */
  val remoteLosePayout: CurrencyUnit = totalInput - localLosePayout

  private val fundingUtxos = localFundingUtxos ++ remoteFundingUtxos

  val fundingLocalPubKey: ECPublicKey = fundingLocalPrivKey.publicKey
  val fundingRemotePubKey: ECPublicKey = fundingRemotePrivKey.publicKey

  val fundingSPK: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(2,
                               Vector(fundingLocalPubKey, fundingRemotePubKey))
  }

  def createFundingTransaction: Future[Transaction] = {
    val output: TransactionOutput =
      TransactionOutput(totalInput, fundingSPK)

    val outputs: Vector[TransactionOutput] = Vector(output)
    val txBuilderF: Future[BitcoinTxBuilder] =
      BitcoinTxBuilder(outputs, fundingUtxos, feeRate, changeSPK, network)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  /** Constructs Local's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: MultiSignatureSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
    val (cetLocalPrivKey, cetRemotePrivKey) = if (sigPubKey == sigPubKeyWin) {
      (cetLocalWinPrivKey, cetRemoteWinPrivKey)
    } else {
      (cetLocalLosePrivKey, cetRemoteLosePrivKey)
    }

    val multiSig = MultiSignatureScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetLocalPrivKey.publicKey, sigPubKey))
    val timeoutSPK = CLTVScriptPubKey(
      locktime = timeout.toScriptNumber,
      scriptPubKey = P2PKHScriptPubKey(cetRemotePrivKey.publicKey))

    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(multiSig, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(localPayout, toLocalSPK)
    val feeSoFar = totalInput - fundingSpendingInfo.output.value
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout - feeSoFar,
                        P2PKHScriptPubKey(cetRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       changeSPK,
                       network)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  /** Constructs Remote's CET given sig*G, the funding tx's UTXOSpendingInfo and payouts */
  def createCETRemote(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: MultiSignatureSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
    val (cetLocalPrivKey, cetRemotePrivKey) = if (sigPubKey == sigPubKeyWin) {
      (cetLocalWinPrivKey, cetRemoteWinPrivKey)
    } else {
      (cetLocalLosePrivKey, cetRemoteLosePrivKey)
    }

    val multiSig = MultiSignatureScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetRemotePrivKey.publicKey, sigPubKey))
    val timeoutSPK = CLTVScriptPubKey(
      locktime = timeout.toScriptNumber,
      scriptPubKey = P2PKHScriptPubKey(cetLocalPrivKey.publicKey))

    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(multiSig, timeoutSPK)

    val toLocal: TransactionOutput =
      TransactionOutput(remotePayout, toLocalSPK)
    val feeSoFar = totalInput - fundingSpendingInfo.output.value
    val toRemote: TransactionOutput =
      TransactionOutput(localPayout - feeSoFar,
                        P2PKHScriptPubKey(cetLocalPrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       changeSPK,
                       network)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  /** Constructs the (time-locked) refund transaction for when the oracle disappears
    * or signs an unknown message.
    * Note that both parties have the same refund transaction.
    */
  def createRefundTx(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    val toLocalValueNotSat =
      (fundingSpendingInfo.amount * localInput).satoshis.toLong / totalInput.satoshis.toLong
    val toLocalValue = Satoshis(Int64(toLocalValueNotSat))
    val toRemoteValue = fundingSpendingInfo.amount - toLocalValue

    val toLocal = TransactionOutput(
      toLocalValue,
      P2PKHScriptPubKey(cetLocalRefundPrivKey.publicKey))
    val toRemote = TransactionOutput(
      toRemoteValue,
      P2PKHScriptPubKey(cetRemoteRefundPrivKey.publicKey))

    val outputs = Vector(toLocal, toRemote)
    val txBuilderF = BitcoinTxBuilder(outputs,
                                      Vector(fundingSpendingInfo),
                                      feeRate,
                                      changeSPK,
                                      network,
                                      timeout.toUInt32)

    txBuilderF.flatMap(subtractFeeAndSign)
  }

  def createCETWinLocal(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseLocal(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def createCETWinRemote(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(
      fundingSpendingInfo: MultiSignatureSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  case class SetupDLC(
      fundingTx: Transaction,
      fundingSpendingInfo: MultiSignatureSpendingInfo,
      cetWinLocal: Transaction,
      cetLoseLocal: Transaction,
      cetWinRemote: Transaction,
      cetLoseRemote: Transaction,
      refundTx: Transaction
  )

  def setupDLC(): Future[SetupDLC] = {
    // Construct Funding Transaction
    createFundingTransaction.flatMap { fundingTx =>
      logger.info(s"Funding Transaction: ${fundingTx.hex}\n")

      val fundingTxId = fundingTx.txIdBE
      val output = fundingTx.outputs.head
      val fundingSpendingInfo = MultiSignatureSpendingInfo(
        outPoint = TransactionOutPoint(fundingTxId, UInt32.zero),
        amount = output.value,
        scriptPubKey =
          output.scriptPubKey.asInstanceOf[MultiSignatureScriptPubKey],
        signers = Vector(fundingLocalPrivKey, fundingRemotePrivKey),
        hashType = HashType.sigHashAll
      )

      // Construct all CETs
      val cetWinLocalF = createCETWinLocal(fundingSpendingInfo)
      val cetLoseLocalF = createCETLoseLocal(fundingSpendingInfo)
      val cetWinRemoteF = createCETWinRemote(fundingSpendingInfo)
      val cetLoseRemoteF = createCETLoseRemote(fundingSpendingInfo)
      val refundTxF = createRefundTx(fundingSpendingInfo)

      cetWinLocalF.foreach(cet => logger.info(s"CET Win Local: ${cet.hex}\n"))
      cetLoseLocalF.foreach(cet => logger.info(s"CET Lose Local: ${cet.hex}\n"))
      cetWinRemoteF.foreach(cet => logger.info(s"CET Win Remote: ${cet.hex}\n"))
      cetLoseRemoteF.foreach(cet =>
        logger.info(s"CET Lose Remote: ${cet.hex}\n"))
      refundTxF.foreach(refundTx =>
        logger.info(s"Refund Tx: ${refundTx.hex}\n"))

      for {
        cetWinLocal <- cetWinLocalF
        cetLoseLocal <- cetLoseLocalF
        cetWinRemote <- cetWinRemoteF
        cetLoseRemote <- cetLoseRemoteF
        refundTx <- refundTxF
      } yield {
        SetupDLC(fundingTx,
                 fundingSpendingInfo,
                 cetWinLocal,
                 cetLoseLocal,
                 cetWinRemote,
                 cetLoseRemote,
                 refundTx)
      }
    }
  }

  def publishClosingTx(
      cetPublishedF: Future[Unit],
      cetOutput: TransactionOutput,
      privKey: ECPrivateKey,
      spendingInfo: BitcoinUTXOSpendingInfo,
      isLocal: Boolean,
      messengerOpt: Option[BitcoinP2PMessenger]): Future[Transaction] = {
    // Construct Closing Transaction
    val txBuilder = BitcoinTxBuilder(
      Vector(
        TransactionOutput(cetOutput.value,
                          P2PKHScriptPubKey(privKey.publicKey))),
      Vector(spendingInfo),
      feeRate,
      changeSPK,
      network
    )

    val spendingTxF = txBuilder.flatMap(subtractFeeAndSign)

    spendingTxF.foreach(
      tx =>
        logger.info(
          s"${if (isLocal) "Local" else "Remote"} Closing Tx: ${tx.hex}"))

    val spendingTxPublishedF = spendingTxF.flatMap { spendingTx =>
      cetPublishedF.flatMap { _ =>
        messengerOpt match {
          case Some(messenger) =>
            messenger
              .sendTransaction(spendingTx)
              .flatMap(_ => messenger.waitForConfirmations(blocks = 6))
          case None => FutureUtil.unit
        }
      }
    }

    spendingTxF.flatMap { spendingTx =>
      spendingTxPublishedF.map { _ =>
        spendingTx
      }
    }
  }

  /** Constructs and executes on the unilateral spending branch of a DLC
    *
    * @return Each transaction published and its spending info
    */
  def executeUnilateralDLC(
      dlcSetup: SetupDLC,
      oracleSigF: Future[SchnorrDigitalSignature],
      local: Boolean,
      messengerOpt: Option[BitcoinP2PMessenger] = None): Future[DLCOutcome] = {
    val SetupDLC(fundingTx,
                 fundingSpendingInfo,
                 cetWinLocal,
                 cetLoseLocal,
                 cetWinRemote,
                 cetLoseRemote,
                 _) = dlcSetup

    oracleSigF.flatMap { oracleSig =>
      // Pick the CET to use and payout by checking which message was signed
      val (cet, cetPrivKey, otherCetPrivKey) =
        if (Schnorr.verify(messageWin, oracleSig, oraclePubKey)) {
          if (local) {
            (cetWinLocal, cetLocalWinPrivKey, cetRemoteWinPrivKey)
          } else {
            (cetWinRemote, cetRemoteWinPrivKey, cetLocalWinPrivKey)
          }
        } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
          if (local) {
            (cetLoseLocal, cetLocalLosePrivKey, cetRemoteLosePrivKey)
          } else {
            (cetLoseRemote, cetRemoteLosePrivKey, cetLocalLosePrivKey)
          }
        } else {
          throw new IllegalStateException(
            "Signature does not correspond to either possible outcome!")
        }

      val cetPublishedF = messengerOpt match {
        case Some(messenger) =>
          messenger
            .sendTransaction(cet)
            .flatMap(_ => messenger.waitForConfirmations(blocks = 6))
        case None => FutureUtil.unit
      }

      // The prefix other refers to remote if local == true and local otherwise
      val output = cet.outputs.head
      val otherOutput = cet.outputs.last

      // Spend the true case on the correct CET
      val cetSpendingInfo = ConditionalSpendingInfo(
        TransactionOutPoint(cet.txIdBE, UInt32.zero),
        output.value,
        output.scriptPubKey.asInstanceOf[ConditionalScriptPubKey],
        Vector(cetPrivKey, ECPrivateKey(oracleSig.s)),
        HashType.sigHashAll,
        ConditionalPath.nonNestedTrue
      )

      val otherCetSpendingInfo = P2PKHSpendingInfo(
        TransactionOutPoint(cet.txIdBE, UInt32.one),
        otherOutput.value,
        otherOutput.scriptPubKey.asInstanceOf[P2PKHScriptPubKey],
        otherCetPrivKey,
        HashType.sigHashAll
      )

      val (localOutput,
           localCetSpendingInfo,
           remoteOutput,
           remoteCetSpendingInfo) = if (local) {
        (output, cetSpendingInfo, otherOutput, otherCetSpendingInfo)
      } else {
        (otherOutput, otherCetSpendingInfo, output, cetSpendingInfo)
      }

      val localSpendingTxF = publishClosingTx(cetPublishedF,
                                              localOutput,
                                              finalLocalPrivKey,
                                              localCetSpendingInfo,
                                              isLocal = true,
                                              messengerOpt)
      val remoteSpendingTxF = publishClosingTx(cetPublishedF,
                                               remoteOutput,
                                               finalRemotePrivKey,
                                               remoteCetSpendingInfo,
                                               isLocal = false,
                                               messengerOpt)

      localSpendingTxF.flatMap { localSpendingTx =>
        remoteSpendingTxF.map { remoteSpendingTx =>
          DLCOutcome(
            fundingTx,
            cet,
            localSpendingTx,
            remoteSpendingTx,
            fundingUtxos,
            fundingSpendingInfo,
            localCetSpendingInfo,
            remoteCetSpendingInfo
          )
        }
      }
    }
  }

  /** Constructs and executes on the refund spending branch of a DLC
    *
    * @return Each transaction published and its spending info
    */
  def executeRefundDLC(
      dlcSetup: SetupDLC,
      messengerOpt: Option[BitcoinP2PMessenger] = None): Future[DLCOutcome] = {
    val SetupDLC(fundingTx, fundingSpendingInfo, _, _, _, _, refundTx) =
      dlcSetup

    val waitForRefundF = messengerOpt match {
      case Some(messenger) =>
        timeout match {
          case BlockHeight(height) => messenger.waitUntilBlockHeight(height)
          case BlockTime(time) =>
            Future {
              val timeToWait = System.currentTimeMillis - time.toInt * 1000

              Thread.sleep(timeToWait)
            }
        }
      case None => FutureUtil.unit
    }

    waitForRefundF.flatMap { _ =>
      val refundTxPublishedF = messengerOpt match {
        case Some(messenger) =>
          messenger
            .sendTransaction(refundTx)
            .flatMap(_ => messenger.waitForConfirmations(blocks = 6))
        case None => FutureUtil.unit
      }

      val localOutput = refundTx.outputs.head
      val remoteOutput = refundTx.outputs.last

      val localRefundSpendingInfo = P2PKHSpendingInfo(
        TransactionOutPoint(refundTx.txIdBE, UInt32.zero),
        localOutput.value,
        localOutput.scriptPubKey.asInstanceOf[P2PKHScriptPubKey],
        cetLocalRefundPrivKey,
        HashType.sigHashAll
      )

      val remoteRefundSpendingInfo = P2PKHSpendingInfo(
        TransactionOutPoint(refundTx.txIdBE, UInt32.one),
        remoteOutput.value,
        remoteOutput.scriptPubKey.asInstanceOf[P2PKHScriptPubKey],
        cetRemoteRefundPrivKey,
        HashType.sigHashAll
      )

      val localSpendingTxF = publishClosingTx(refundTxPublishedF,
                                              localOutput,
                                              finalLocalPrivKey,
                                              localRefundSpendingInfo,
                                              isLocal = true,
                                              messengerOpt)
      val remoteSpendingTxF = publishClosingTx(refundTxPublishedF,
                                               remoteOutput,
                                               finalRemotePrivKey,
                                               remoteRefundSpendingInfo,
                                               isLocal = false,
                                               messengerOpt)

      localSpendingTxF.flatMap { localSpendingTx =>
        remoteSpendingTxF.map { remoteSpendingTx =>
          DLCOutcome(
            fundingTx,
            refundTx,
            localSpendingTx,
            remoteSpendingTx,
            fundingUtxos,
            fundingSpendingInfo,
            localRefundSpendingInfo,
            remoteRefundSpendingInfo
          )
        }
      }
    }
  }
}

object BinaryOutcomeDLCWithSelf {

  /** Subtracts the estimated fee by removing from each output evenly */
  def subtractFeeAndSign(txBuilder: BitcoinTxBuilder)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    txBuilder.unsignedTx.flatMap { tx =>
      val fee = txBuilder.feeRate.calc(tx)

      val outputs = txBuilder.destinations

      val feePerOutput = Satoshis(Int64(fee.satoshis.toLong / outputs.length))
      val feeRemainder = Satoshis(Int64(fee.satoshis.toLong % outputs.length))

      val newOutputsWithoutRemainder = outputs.map(output =>
        TransactionOutput(output.value - feePerOutput, output.scriptPubKey))
      val lastOutput = newOutputsWithoutRemainder.last
      val newLastOutput = TransactionOutput(lastOutput.value - feeRemainder,
                                            lastOutput.scriptPubKey)
      val newOutputs = newOutputsWithoutRemainder.dropRight(1).:+(newLastOutput)

      val newBuilder =
        BitcoinTxBuilder(newOutputs,
                         txBuilder.utxoMap,
                         txBuilder.feeRate,
                         txBuilder.changeSPK,
                         txBuilder.network,
                         txBuilder.lockTimeOverrideOpt)

      newBuilder.flatMap(_.sign)
    }
  }
}

/** Contains all DLC transactions and the BitcoinUTXOSpendingInfos they use. */
case class DLCOutcome(
    fundingTx: Transaction,
    cet: Transaction,
    localClosingTx: Transaction,
    remoteClosingTx: Transaction,
    fundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    fundingSpendingInfo: BitcoinUTXOSpendingInfo,
    localCetSpendingInfo: BitcoinUTXOSpendingInfo,
    remoteCetSpendingInfo: BitcoinUTXOSpendingInfo
)
