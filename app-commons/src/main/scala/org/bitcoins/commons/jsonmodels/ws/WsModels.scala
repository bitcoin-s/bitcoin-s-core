package org.bitcoins.commons.jsonmodels.ws

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.commons.serializers.WsPicklers
import org.bitcoins.core.api.chain.db.{CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.crypto.{Sha256Digest, StringFactory}
import ujson.Value

import java.net.InetSocketAddress

/** The event type being sent over the websocket. An example is [[WalletWsType.NewAddress]] */
sealed trait WsType

object WsType extends StringFactory[WsType] {

  override def fromString(string: String): WsType = {
    ChainWsType.fromStringOpt(string) match {
      case Some(t) => t
      case None =>
        WalletWsType.fromString(string)
    }
  }
}

sealed trait WalletWsType extends WsType
sealed trait ChainWsType extends WsType
sealed trait TorWsType extends WsType
sealed trait DLCNodeWsType extends WsType

object WalletWsType extends StringFactory[WalletWsType] {
  case object TxProcessed extends WalletWsType
  case object TxBroadcast extends WalletWsType
  case object ReservedUtxos extends WalletWsType
  case object NewAddress extends WalletWsType

  case object DLCStateChange extends WalletWsType
  case object DLCOfferAdd extends WalletWsType
  case object DLCOfferRemove extends WalletWsType
  case object RescanComplete extends WalletWsType
  case object FeeRateChange extends WalletWsType

  private val all =
    Vector(TxProcessed,
           TxBroadcast,
           ReservedUtxos,
           NewAddress,
           DLCStateChange,
           DLCOfferAdd,
           DLCOfferRemove,
           RescanComplete,
           FeeRateChange)

  override def fromStringOpt(string: String): Option[WalletWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): WalletWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find wallet ws type for string=$string"))
  }
}

object ChainWsType extends StringFactory[ChainWsType] {
  case object BlockProcessed extends ChainWsType

  case object CompactFilterHeaderProcessed extends ChainWsType

  case object CompactFilterProcessed extends ChainWsType
  case object SyncFlagChanged extends ChainWsType

  private val all: Vector[ChainWsType] =
    Vector(
      BlockProcessed,
      CompactFilterHeaderProcessed,
      CompactFilterProcessed,
      SyncFlagChanged
    )

  override def fromStringOpt(string: String): Option[ChainWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): ChainWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find chain ws type for string=$string"))
  }
}

object TorWsType extends StringFactory[TorWsType] {
  case object TorStarted extends TorWsType

  private val all = Vector(TorStarted)

  override def fromStringOpt(string: String): Option[TorWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): TorWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find chain ws type for string=$string"))
  }
}

object DLCNodeWsType extends StringFactory[DLCNodeWsType] {
  case object DLCConnectionInitiated extends DLCNodeWsType
  case object DLCConnectionEstablished extends DLCNodeWsType
  case object DLCConnectionFailed extends DLCNodeWsType
  case object DLCOfferSendSucceed extends DLCNodeWsType
  case object DLCOfferSendFailed extends DLCNodeWsType
  case object DLCAcceptSucceed extends DLCNodeWsType
  case object DLCAcceptFailed extends DLCNodeWsType
  case object DLCSignSucceed extends DLCNodeWsType
  case object DLCSignFailed extends DLCNodeWsType

  private val all = Vector(
    DLCConnectionInitiated,
    DLCConnectionEstablished,
    DLCConnectionFailed,
    DLCOfferSendSucceed,
    DLCOfferSendFailed,
    DLCAcceptSucceed,
    DLCAcceptFailed,
    DLCSignSucceed,
    DLCSignFailed
  )

  override def fromStringOpt(string: String): Option[DLCNodeWsType] = {
    all.find(_.toString.toLowerCase() == string.toLowerCase)
  }

  override def fromString(string: String): DLCNodeWsType = {
    fromStringOpt(string)
      .getOrElse(sys.error(s"Cannot find chain ws type for string=$string"))
  }
}

/** A notification that we send over the websocket.
  * The type of the notification is indicated by [[WsType]].
  * An example is [[org.bitcoins.commons.jsonmodels.ws.WalletNotification.NewAddressNotification]]
  * This sends a notification that the wallet generated a new address
  */
sealed trait WsNotification[T] {
  def `type`: WsType
  def payload: T
  def json: ujson.Value
}

sealed trait ChainNotification[T] extends WsNotification[T] {
  override def `type`: ChainWsType
}

sealed trait WalletNotification[T] extends WsNotification[T] {
  override def `type`: WalletWsType
}

sealed trait TorNotification[T] extends WsNotification[T] {
  override def `type`: TorWsType
}

sealed trait DLCNodeNotification[T] extends WsNotification[T] {
  override def `type`: DLCNodeWsType
}

object WalletNotification {

  case class NewAddressNotification(payload: BitcoinAddress)
      extends WalletNotification[BitcoinAddress] {
    override val `type`: WalletWsType = WalletWsType.NewAddress

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.newAddressPickler)
    }
  }

  case class TxProcessedNotification(payload: Transaction)
      extends WalletNotification[Transaction] {
    override val `type`: WalletWsType = WalletWsType.TxProcessed

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.txProcessedPickler)
    }
  }

  case class TxBroadcastNotification(payload: Transaction)
      extends WalletNotification[Transaction] {
    override val `type`: WalletWsType = WalletWsType.TxBroadcast

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.txBroadcastPickler)
    }
  }

  case class ReservedUtxosNotification(payload: Vector[SpendingInfoDb])
      extends WalletNotification[Vector[SpendingInfoDb]] {
    override val `type`: WalletWsType = WalletWsType.ReservedUtxos

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.reservedUtxosPickler)
    }
  }

  case class DLCStateChangeNotification(payload: DLCStatus)
      extends WalletNotification[DLCStatus] {
    override val `type`: WalletWsType = WalletWsType.DLCStateChange

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.dlcStateChangePickler)
    }
  }

  case class DLCOfferAddNotification(payload: IncomingDLCOfferDb)
      extends WalletNotification[IncomingDLCOfferDb] {
    override val `type`: WalletWsType = WalletWsType.DLCOfferAdd

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.dlcOfferAddPickler)
    }
  }

  case class DLCOfferRemoveNotification(payload: Sha256Digest)
      extends WalletNotification[Sha256Digest] {
    override val `type`: WalletWsType = WalletWsType.DLCOfferRemove

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.dlcOfferRemovePickler)
    }
  }

  case class RescanComplete(payload: String)
      extends WalletNotification[String] {
    override val `type`: WalletWsType = WalletWsType.RescanComplete

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.rescanPickler)
    }
  }

  case class FeeRateChange(payload: FeeUnit)
      extends WalletNotification[FeeUnit] {
    override val `type`: WalletWsType = WalletWsType.FeeRateChange

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.feeRatePickler)
    }
  }
}

object ChainNotification {

  case class BlockProcessedNotification(payload: GetBlockHeaderResult)
      extends ChainNotification[GetBlockHeaderResult] {
    override val `type`: ChainWsType = ChainWsType.BlockProcessed

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.blockProcessedPickler)
    }
  }

  case class CompactFilterHeaderProcessedNotification(
      payload: CompactFilterHeaderDb)
      extends ChainNotification[CompactFilterHeaderDb] {
    override val `type`: ChainWsType = ChainWsType.CompactFilterHeaderProcessed

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(
        using WsPicklers.compactFilterHeaderProcessedPickler)
    }
  }

  case class CompactFilterProcessedNotification(payload: CompactFilterDb)
      extends ChainNotification[CompactFilterDb] {
    override val `type`: ChainWsType = ChainWsType.CompactFilterProcessed

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.compactFilterProcessedPickler)
    }
  }

  case class SyncFlagChangedNotification(payload: Boolean)
      extends ChainNotification[Boolean] {
    override val `type`: ChainWsType = ChainWsType.SyncFlagChanged

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.syncFlagChangedPickler)
    }
  }
}

object TorNotification {

  case object TorStartedNotification extends TorNotification[Unit] {
    override val `type`: TorWsType = TorWsType.TorStarted
    override val payload: Unit = ()

    override val json: ujson.Value = {
      upickle.default.writeJs(this)(using WsPicklers.torStartedPickler)
    }
  }
}

object DLCNodeNotification {

  case class DLCNodeConnectionInitiated(payload: InetSocketAddress)
      extends DLCNodeNotification[InetSocketAddress] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCConnectionInitiated

    override def json: Value = upickle.default.writeJs(this)(
      using WsPicklers.dlcNodeConnectionInitiatedPickler)
  }

  case class DLCNodeConnectionEstablished(payload: InetSocketAddress)
      extends DLCNodeNotification[InetSocketAddress] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCConnectionEstablished

    override def json: Value = upickle.default.writeJs(this)(
      using WsPicklers.dlcNodeConnectionEstablishedPickler)
  }

  case class DLCNodeConnectionFailed(payload: InetSocketAddress)
      extends DLCNodeNotification[InetSocketAddress] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCConnectionFailed

    override def json: Value =
      upickle.default.writeJs(this)(using WsPicklers.dlcNodeConnectionFailedPickler)
  }

  case class DLCAcceptFailed(payload: (Sha256Digest, String))
      extends DLCNodeNotification[(Sha256Digest, String)] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCAcceptFailed

    override def json: Value =
      upickle.default.writeJs(this)(using WsPicklers.dlcAcceptFailedPickler)
  }

  case class DLCAcceptSucceed(payload: Sha256Digest)
      extends DLCNodeNotification[Sha256Digest] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCAcceptSucceed

    override def json: Value =
      upickle.default.writeJs(this)(using WsPicklers.dlcAcceptSucceedPickler)
  }

  case class DLCOfferSendFailed(payload: (Sha256Digest, String))
      extends DLCNodeNotification[(Sha256Digest, String)] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCOfferSendFailed

    override def json: Value =
      upickle.default.writeJs(this)(using WsPicklers.dlcOfferSendFailedPickler)
  }

  case class DLCOfferSendSucceed(payload: Sha256Digest)
      extends DLCNodeNotification[Sha256Digest] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCOfferSendSucceed

    override def json: Value =
      upickle.default.writeJs(this)(using WsPicklers.dlcOfferSendSucceedPickler)
  }

  case class DLCSignFailed(payload: (Sha256Digest, String))
      extends DLCNodeNotification[(Sha256Digest, String)] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCSignFailed

    override def json: Value =
      upickle.default.writeJs(this)(using WsPicklers.dlcSignFailedPickler)
  }

  case class DLCSignSucceed(payload: Sha256Digest)
      extends DLCNodeNotification[Sha256Digest] {
    override def `type`: DLCNodeWsType = DLCNodeWsType.DLCSignSucceed

    override def json: Value =
      upickle.default.writeJs(this)(using WsPicklers.dlcSignSucceedPickler)
  }
}
