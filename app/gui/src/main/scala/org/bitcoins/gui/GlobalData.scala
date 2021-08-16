package org.bitcoins.gui

import org.bitcoins.cli.Config
import org.bitcoins.core.config._
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.gui.settings.Themes
import scalafx.beans.property._

object GlobalData {
  val currentConfirmedBalance: StringProperty = StringProperty("0")
  val currentUnconfirmedBalance: StringProperty = StringProperty("0")
  val currentReservedBalance: StringProperty = StringProperty("0")
  val currentTotalBalance: StringProperty = StringProperty("0")

  val currentPNL: StringProperty = StringProperty("0")
  val rateOfReturn: StringProperty = StringProperty("0%")

  val syncHeight: StringProperty = StringProperty("Syncing headers...")

  var network: BitcoinNetwork = _
  val proxyEnabled = BooleanProperty(false)
  val networkString: StringProperty = new StringProperty("")

  def setBitcoinNetwork(
      network: BitcoinNetwork,
      proxyEnabled: Boolean): Unit = {
    this.network = network
    this.proxyEnabled.value = proxyEnabled
    networkString.value = "Network: " + network
    if (GlobalData.proxyEnabled.value) networkString.value += " over Tor"
  }

  val statusText: StringProperty = StringProperty("")

  private val isConnectedStr = "● Connected"
  private val isDisconnectedStr = "○ Disconnected"

  val connected: BooleanProperty = BooleanProperty(true)

  connected.addListener { (_, _, newValue) =>
    if (newValue) {
      connectedStr.value = isConnectedStr
    } else {
      connectedStr.value = isDisconnectedStr
    }
  }

  val connectedStr: StringProperty = StringProperty(isConnectedStr)

  var darkThemeEnabled: Boolean = true

  def currentStyleSheets: Seq[String] =
    if (GlobalData.darkThemeEnabled) {
      Seq(Themes.DarkTheme.fileLocation)
    } else {
      Seq.empty
    }

  var rpcPortOpt: Option[Int] = None

  var debug = false

  def consoleCliConfig: Config =
    rpcPortOpt match {
      case None =>
        Config(debug = debug)
      case Some(rpcPort) =>
        Config(debug = debug, rpcPortOpt = Some(rpcPort))
    }

  lazy val broadcastUrl: String = GlobalData.network match {
    case MainNet =>
      "https://blockstream.info/api/tx"
    case TestNet3 =>
      "https://blockstream.info/testnet/api/tx"
    case net @ (RegTest | SigNet) => s"Broadcast from your own node on $net"
  }

  /** Builds a url for the Blockstream Explorer to view the tx */
  def buildBlockstreamExplorerTxUrl(txIdHex: String): String = {
    network match {
      case MainNet =>
        s"https://blockstream.info/tx/${txIdHex}"
      case TestNet3 =>
        s"https://blockstream.info/testnet/tx/${txIdHex}"
      case net @ (RegTest | SigNet) =>
        s"View transaction on your own node on $net"
    }
  }

  /** Builds a url for the mempool.space to view the tx */
  def buildMempoolSpaceTxUrl(txIdHex: String): String = {
    network match {
      case MainNet =>
        s"https://mempool.space/tx/${txIdHex}"
      case TestNet3 =>
        s"https://mempool.space/testnet/tx/${txIdHex}"
      case net @ RegTest =>
        s"View transaction on your own node on $net"
      case SigNet =>
        s"https://mempool.space/signet/tx/${txIdHex}"
    }
  }

  /** Builds a url for the Oracle Explorer to view an Announcement */
  def buildAnnouncementUrl(announcementHash: String): String = {
    network match {
      case MainNet =>
        s"https://oracle.suredbits.com/announcement/${announcementHash}"
      case TestNet3 =>
        s"https://test.oracle.suredbits.com/announcement/${announcementHash}"
      case net @ (RegTest | SigNet) =>
        s"View transaction on your own node on $net"
    }
  }

  var feeRate: FeeUnit = SatoshisPerVirtualByte.fromLong(50)

  val torDLCHostAddress = StringProperty("")
}
