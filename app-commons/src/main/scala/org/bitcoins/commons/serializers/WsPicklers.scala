package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.ws.ChainNotification.{
  BlockProcessedNotification,
  CompactFilterHeaderProcessedNotification,
  CompactFilterProcessedNotification,
  SyncFlagChangedNotification
}
import org.bitcoins.commons.jsonmodels.ws.DLCNodeNotification.{
  DLCAcceptFailed,
  DLCAcceptSucceed,
  DLCNodeConnectionEstablished,
  DLCNodeConnectionFailed,
  DLCNodeConnectionInitiated,
  DLCOfferSendFailed,
  DLCOfferSendSucceed,
  DLCSignFailed,
  DLCSignSucceed
}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.{
  DLCOfferAddNotification,
  DLCOfferRemoveNotification,
  DLCStateChangeNotification,
  FeeRateChange,
  NewAddressNotification,
  RescanComplete,
  ReservedUtxosNotification,
  TxBroadcastNotification,
  TxProcessedNotification
}
import org.bitcoins.commons.jsonmodels.ws.{
  ChainNotification,
  ChainWsType,
  DLCNodeNotification,
  DLCNodeWsType,
  TorNotification,
  TorWsType,
  WalletNotification,
  WalletWsType
}
import org.bitcoins.core.config.DLC
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.crypto.Sha256Digest
import upickle.default._

import java.net.InetSocketAddress

object WsPicklers {

  implicit val chainWsTypePickler: ReadWriter[ChainWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => ChainWsType.fromString(str.str))
  }

  implicit val walletWsTypePickler: ReadWriter[WalletWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => WalletWsType.fromString(str.str))
  }

  implicit val torWsTypePickler: ReadWriter[TorWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => TorWsType.fromString(str.str))
  }

  implicit val dlcNodeWsTypePickler: ReadWriter[DLCNodeWsType] = {
    readwriter[ujson.Str]
      .bimap(_.toString.toLowerCase, str => DLCNodeWsType.fromString(str.str))
  }

  private def writeChainNotification(
      notification: ChainNotification[?]): ujson.Obj = {
    val payloadJson: ujson.Value = notification match {
      case BlockProcessedNotification(block) =>
        upickle.default.writeJs(block)(using Picklers.getBlockHeaderResultPickler)
      case CompactFilterHeaderProcessedNotification(filterHeader) =>
        upickle.default.writeJs(filterHeader)(
          using Picklers.compactFilterHeaderPickler)
      case CompactFilterProcessedNotification(filter) =>
        upickle.default.writeJs(filter)(using Picklers.compactFilterDbPickler)
      case SyncFlagChangedNotification(syncing) =>
        upickle.default.writeJs(syncing)
    }
    val typeJson = upickle.default.writeJs(notification.`type`)
    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> typeJson,
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readChainNotification(obj: ujson.Obj): ChainNotification[?] = {
    val typeObj = read[ChainWsType](obj(PicklerKeys.typeKey))
    val payloadObj = obj(PicklerKeys.payloadKey)

    typeObj match {
      case ChainWsType.BlockProcessed =>
        val block =
          upickle.default.read(payloadObj)(using Picklers.getBlockHeaderResultPickler)
        BlockProcessedNotification(block)
      case ChainWsType.CompactFilterHeaderProcessed =>
        val filterheader =
          upickle.default.read(payloadObj)(using Picklers.compactFilterHeaderPickler)

        CompactFilterHeaderProcessedNotification(filterheader)

      case ChainWsType.CompactFilterProcessed =>
        val filter =
          upickle.default.read(payloadObj)(using Picklers.compactFilterDbPickler)
        CompactFilterProcessedNotification(filter)
      case ChainWsType.SyncFlagChanged =>
        val syncing = payloadObj.bool
        SyncFlagChangedNotification(syncing)
    }
  }

  private def writeWalletNotification(
      notification: WalletNotification[?]): ujson.Obj = {
    val payloadJson: ujson.Value = notification match {
      case TxBroadcastNotification(tx) =>
        upickle.default.writeJs(tx)(using Picklers.transactionPickler)
      case TxProcessedNotification(tx) =>
        upickle.default.writeJs(tx)(using Picklers.transactionPickler)
      case NewAddressNotification(address) =>
        upickle.default.writeJs(address)(using Picklers.bitcoinAddressPickler)
      case ReservedUtxosNotification(utxos) =>
        val vec = utxos.map(u =>
          upickle.default.writeJs(u)(using Picklers.spendingInfoDbPickler))
        ujson.Arr.from(vec)
      case DLCStateChangeNotification(status) =>
        upickle.default.writeJs(status)(using Picklers.dlcStatusW)
      case DLCOfferAddNotification(offerDb) =>
        upickle.default.writeJs(offerDb)(using Picklers.dlcOfferAddW)
      case DLCOfferRemoveNotification(offerHash) =>
        upickle.default.writeJs(offerHash)(using Picklers.dlcOfferRemoveW)
      case r: RescanComplete =>
        upickle.default.writeJs(r)(using Picklers.rescanComplete)
      case FeeRateChange(feeRate) =>
        upickle.default.writeJs(feeRate)(using Picklers.feeUnit)
    }

    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readWalletNotification(obj: ujson.Obj): WalletNotification[?] = {
    val typeObj = read[WalletWsType](obj(PicklerKeys.typeKey))
    val payloadObj = obj(PicklerKeys.payloadKey)
    typeObj match {
      case WalletWsType.TxBroadcast =>
        val tx = upickle.default.read(payloadObj)(using Picklers.transactionPickler)
        TxBroadcastNotification(tx)
      case WalletWsType.TxProcessed =>
        val tx = upickle.default.read(payloadObj)(using Picklers.transactionPickler)
        TxProcessedNotification(tx)
      case WalletWsType.NewAddress =>
        val address =
          upickle.default.read(payloadObj)(using Picklers.bitcoinAddressPickler)
        NewAddressNotification(address)
      case WalletWsType.ReservedUtxos =>
        val utxos = obj(PicklerKeys.payloadKey).arr.toVector.map { utxoJson =>
          upickle.default.read(utxoJson)(using Picklers.spendingInfoDbPickler)
        }
        ReservedUtxosNotification(utxos)
      case WalletWsType.DLCStateChange =>
        val status = upickle.default.read(payloadObj)(using Picklers.dlcStatusR)
        DLCStateChangeNotification(status)
      case WalletWsType.DLCOfferAdd =>
        val offerDb = upickle.default.read(payloadObj)(using Picklers.dlcOfferAddR)
        DLCOfferAddNotification(offerDb)
      case WalletWsType.DLCOfferRemove =>
        val offerHash =
          upickle.default.read(payloadObj)(using Picklers.dlcOfferRemoveR)
        DLCOfferRemoveNotification(offerHash)
      case WalletWsType.RescanComplete =>
        val complete = upickle.default.read(payloadObj)(using Picklers.rescanComplete)
        complete
      case WalletWsType.FeeRateChange =>
        FeeRateChange(upickle.default.read(payloadObj)(using Picklers.feeUnit))
    }
  }

  private def writeTorNotification(
      notification: TorNotification[?]): ujson.Obj = {
    val payloadJson = notification.`type` match {
      case TorWsType.TorStarted =>
        ujson.Null
    }

    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readTorNotification(obj: ujson.Obj): TorNotification[?] = {
    val typeObj = read[TorWsType](obj(PicklerKeys.typeKey))
    typeObj match {
      case TorWsType.TorStarted =>
        TorNotification.TorStartedNotification
    }
  }

  private def writeDLCNodeNotification(
      notification: DLCNodeNotification[?]): ujson.Obj = {
    def addr2str(address: InetSocketAddress) =
      address.getHostName + ":" + address.getPort
    def failure2obj(payload: (Sha256Digest, String)): ujson.Obj = {
      ujson.Obj(PicklerKeys.idKey -> writeJs(payload._1.hex),
                PicklerKeys.errorKey -> writeJs(payload._2))
    }
    val payloadJson: ujson.Value = notification match {
      case DLCNodeConnectionInitiated(address) =>
        upickle.default.writeJs(addr2str(address))
      case DLCNodeConnectionEstablished(address) =>
        upickle.default.writeJs(addr2str(address))
      case DLCNodeConnectionFailed(address) =>
        upickle.default.writeJs(addr2str(address))
      case DLCAcceptFailed(payload) => failure2obj(payload)
      case DLCAcceptSucceed(id) =>
        upickle.default.writeJs(id.hex)
      case DLCOfferSendFailed(payload) => failure2obj(payload)
      case DLCOfferSendSucceed(id) =>
        upickle.default.writeJs(id.hex)
      case DLCSignFailed(payload) => failure2obj(payload)
      case DLCSignSucceed(id) =>
        upickle.default.writeJs(id.hex)
    }
    val notificationObj = ujson.Obj(
      PicklerKeys.typeKey -> writeJs(notification.`type`),
      PicklerKeys.payloadKey -> payloadJson
    )
    notificationObj
  }

  private def readDLCNodeNotification(
      obj: ujson.Obj): DLCNodeNotification[?] = {
    val typeObj = read[DLCNodeWsType](obj(PicklerKeys.typeKey))
    val payloadObj = obj(PicklerKeys.payloadKey)

    def obj2failure(payload: ujson.Value): (Sha256Digest, String) = {
      (Sha256Digest.fromHex(payload.obj(PicklerKeys.idKey).str),
       payload.obj(PicklerKeys.errorKey).str)
    }

    typeObj match {
      case DLCNodeWsType.DLCConnectionInitiated =>
        val address: InetSocketAddress =
          NetworkUtil.parseInetSocketAddress(payloadObj.str, DLC.DefaultPort)
        DLCNodeConnectionInitiated(address)
      case DLCNodeWsType.DLCConnectionEstablished =>
        val address: InetSocketAddress =
          NetworkUtil.parseInetSocketAddress(payloadObj.str, DLC.DefaultPort)
        DLCNodeConnectionEstablished(address)
      case DLCNodeWsType.DLCConnectionFailed =>
        val address: InetSocketAddress =
          NetworkUtil.parseInetSocketAddress(payloadObj.str, DLC.DefaultPort)
        DLCNodeConnectionFailed(address)
      case DLCNodeWsType.DLCAcceptFailed =>
        DLCAcceptFailed(obj2failure(payloadObj))
      case DLCNodeWsType.DLCAcceptSucceed =>
        DLCAcceptSucceed(Sha256Digest.fromHex(payloadObj.str))
      case DLCNodeWsType.DLCOfferSendFailed =>
        DLCOfferSendFailed(obj2failure(payloadObj))
      case DLCNodeWsType.DLCOfferSendSucceed =>
        DLCOfferSendSucceed(Sha256Digest.fromHex(payloadObj.str))
      case DLCNodeWsType.DLCSignFailed =>
        DLCSignFailed(obj2failure(payloadObj))
      case DLCNodeWsType.DLCSignSucceed =>
        DLCSignSucceed(Sha256Digest.fromHex(payloadObj.str))
    }
  }

  implicit val newAddressPickler: ReadWriter[NewAddressNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[NewAddressNotification])
  }

  implicit val txProcessedPickler: ReadWriter[TxProcessedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[TxProcessedNotification])
  }

  implicit val txBroadcastPickler: ReadWriter[TxBroadcastNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[TxBroadcastNotification])
  }

  implicit val reservedUtxosPickler: ReadWriter[ReservedUtxosNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[ReservedUtxosNotification])
  }

  implicit val rescanPickler: ReadWriter[RescanComplete] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[RescanComplete]
    )
  }

  implicit val feeRatePickler: ReadWriter[FeeRateChange] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[FeeRateChange]
    )
  }

  implicit val dlcNodeNotificationPickler: ReadWriter[
    DLCNodeNotification[?]] = {
    readwriter[ujson.Obj]
      .bimap(writeDLCNodeNotification, readDLCNodeNotification)
  }

  implicit val walletNotificationPickler: ReadWriter[WalletNotification[?]] = {
    readwriter[ujson.Obj].bimap(writeWalletNotification, readWalletNotification)
  }

  implicit val chainNotificationPickler: ReadWriter[ChainNotification[?]] = {
    readwriter[ujson.Obj].bimap(writeChainNotification, readChainNotification)
  }

  implicit val blockProcessedPickler: ReadWriter[BlockProcessedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeChainNotification(_),
      readChainNotification(_).asInstanceOf[BlockProcessedNotification]
    )
  }

  implicit val compactFilterHeaderProcessedPickler: ReadWriter[
    CompactFilterHeaderProcessedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeChainNotification(_),
      readChainNotification(_)
        .asInstanceOf[CompactFilterHeaderProcessedNotification]
    )
  }

  implicit val compactFilterProcessedPickler: ReadWriter[
    CompactFilterProcessedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeChainNotification(_),
      readChainNotification(_).asInstanceOf[CompactFilterProcessedNotification]
    )
  }

  implicit val syncFlagChangedPickler: ReadWriter[
    SyncFlagChangedNotification] = {
    readwriter[ujson.Obj].bimap(
      writeChainNotification(_),
      readChainNotification(_).asInstanceOf[SyncFlagChangedNotification]
    )
  }

  implicit val dlcStateChangePickler: ReadWriter[DLCStateChangeNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[DLCStateChangeNotification])
  }

  implicit val dlcOfferAddPickler: ReadWriter[DLCOfferAddNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[DLCOfferAddNotification])
  }

  implicit val dlcOfferRemovePickler: ReadWriter[DLCOfferRemoveNotification] = {
    readwriter[ujson.Obj].bimap(
      writeWalletNotification(_),
      readWalletNotification(_).asInstanceOf[DLCOfferRemoveNotification])
  }

  implicit val torStartedPickler: ReadWriter[
    TorNotification.TorStartedNotification.type] = {
    readwriter[ujson.Obj].bimap(
      writeTorNotification(_),
      readTorNotification(_)
        .asInstanceOf[TorNotification.TorStartedNotification.type])
  }

  implicit val dlcNodeConnectionInitiatedPickler: ReadWriter[
    DLCNodeConnectionInitiated] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCNodeConnectionInitiated])
  }

  implicit val dlcNodeConnectionFailedPickler: ReadWriter[
    DLCNodeConnectionFailed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCNodeConnectionFailed])
  }

  implicit val dlcNodeConnectionEstablishedPickler: ReadWriter[
    DLCNodeConnectionEstablished] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCNodeConnectionEstablished])
  }

  implicit val dlcAcceptSucceedPickler: ReadWriter[DLCAcceptSucceed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCAcceptSucceed])
  }

  implicit val dlcAcceptFailedPickler: ReadWriter[DLCAcceptFailed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCAcceptFailed])
  }

  implicit val dlcSignSucceedPickler: ReadWriter[DLCSignSucceed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCSignSucceed])
  }

  implicit val dlcSignFailedPickler: ReadWriter[DLCSignFailed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCSignFailed])
  }

  implicit val dlcOfferSendSucceedPickler: ReadWriter[DLCOfferSendSucceed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCOfferSendSucceed])
  }

  implicit val dlcOfferSendFailedPickler: ReadWriter[DLCOfferSendFailed] = {
    readwriter[ujson.Obj].bimap(
      writeDLCNodeNotification(_),
      readDLCNodeNotification(_).asInstanceOf[DLCOfferSendFailed])
  }
}
