package org.bitcoins.gui

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.ConsoleCli
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.gui.dialog.{GetNewAddressDialog, SendDialog}
import org.bitcoins.gui.util.GUIUtil
import scalafx.beans.property.{ObjectProperty, StringProperty}
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.stage.Window

import scala.util.{Failure, Success, Try}

class WalletGUIModel() {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  private case object UpdateBalanceRunnable extends Runnable {

    override def run(): Unit = {
      while (true) {
        updateBalance()
        // wait 10 seconds
        Thread.sleep(10000)
      }
    }
  }
  lazy val updateBalanceThread = new Thread(UpdateBalanceRunnable)

  def startBalanceThread(): Unit = {
    updateBalanceThread.setDaemon(true)
    updateBalanceThread.setName(
      s"bitcoin-s-gui-balance-${System.currentTimeMillis()}")
    updateBalanceThread.start()
  }

  startBalanceThread()

  def updateFeeRate(): Try[FeeUnit] = {
    ConsoleCli.exec(EstimateFee, GlobalData.consoleCliConfig).map { feeStr =>
      val feeUnit = FeeUnit.fromString(feeStr)
      GlobalData.feeRate = feeUnit
      feeUnit
    }
  }

  updateFeeRate()

  def onGetNewAddress(): Unit = {
    val address = StringProperty("")

    taskRunner.run(
      caption = "Get New Address",
      op = {
        ConsoleCli.exec(GetNewAddress(None),
                        GlobalData.consoleCliConfig) match {
          case Success(commandReturn) => address.value = commandReturn
          case Failure(err)           => throw err
        }
      }
    )

    GetNewAddressDialog.showAndWait(parentWindow.value, address)
  }

  def onSend(): Unit = {
    val result = SendDialog.showAndWait(parentWindow.value)

    result match {
      case Some((address, amount)) =>
        taskRunner.run(
          caption = s"Send $amount to $address",
          op = {
            val sats = Satoshis(amount.toLong)

            ConsoleCli.exec(
              SendToAddress(BitcoinAddress.fromString(address),
                            Bitcoins(sats),
                            satoshisPerVirtualByte = None,
                            noBroadcast = false),
              GlobalData.consoleCliConfig
            ) match {
              case Success(txid) =>
                GlobalData.log.value =
                  s"Sent $amount to $address in tx: $txid\n\n${GlobalData.log()}"
              case Failure(err) => throw err
            }
          }
        )
      case None => ()
    }

    updateBalance()
  }

  private def updateBalance(): Unit = {
    ConsoleCli.exec(GetBalances(isSats = true),
                    GlobalData.consoleCliConfig) match {
      case Success(commandReturn) =>
        val json = ujson.read(commandReturn).obj
        val confirmedBalance =
          GUIUtil.numberFormatter.format(
            json("confirmed").str.split(' ').head.toLong)
        val unconfirmedBalance =
          GUIUtil.numberFormatter.format(
            json("unconfirmed").str.split(' ').head.toLong)
        val reservedBalance =
          GUIUtil.numberFormatter.format(
            json("reserved").str.split(' ').head.toLong)
        val totalBalance =
          GUIUtil.numberFormatter.format(
            json("total").str.split(' ').head.toLong)

        GlobalData.currentConfirmedBalance.value = confirmedBalance
        GlobalData.currentUnconfirmedBalance.value = unconfirmedBalance
        GlobalData.currentReservedBalance.value = reservedBalance
        GlobalData.currentTotalBalance.value = totalBalance
      case Failure(err) =>
        err.printStackTrace()
        val _ = new Alert(AlertType.Error) {
          initOwner(owner)
          title = "Could not retrieve wallet balance"
          headerText = s"Operation failed. Exception: ${err.getClass}"
          contentText = err.getMessage
        }.showAndWait()
    }
  }
}
