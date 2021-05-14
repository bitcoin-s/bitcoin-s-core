package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.CreateDLCOffer
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.{
  ContractInfo,
  EnumContractDescriptor,
  EnumSingleOracleInfo
}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil.{numberFormatter, setNumericInput}
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Window

import java.time.ZoneOffset
import scala.collection._
import scala.util.{Failure, Success, Try}

object InitEnumOfferDialog {

  def showAndWait(parentWindow: Window): Option[CreateDLCOffer] = {
    val dialog = new Dialog[Option[CreateDLCOffer]]() {
      initOwner(parentWindow)
      title = "Create DLC Offer"
      headerText = "Enter DLC Contract Details"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    val fields: mutable.Map[Int, (TextField, TextField)] = mutable.Map.empty

    var outcomesAdded = false
    val announcementTF = new TextField() {
      promptText = ""
    }

    var nextRow: Int = 3
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(
        new Label("Oracle Announcement") {
          tooltip = Tooltip(
            "Announcement given by an oracle, this will dictate the rest of the contract.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        0
      )
      add(announcementTF, 1, 0)
    }

    def addOutcomeRow(outcomeText: String): Unit = {

      val outcomeTF = new TextField() {
        text = outcomeText
        disable = true
        editable = false
      }
      val amtTF = new TextField() {
        promptText = "Bitcoins"
        tooltip = Tooltip(
          s"""Amount you will win if the oracle signs for "$outcomeText".""")
        tooltip.value.setShowDelay(new javafx.util.Duration(100))
      }
      setNumericInput(amtTF)

      val row = nextRow
      val _ = fields.put(row, (outcomeTF, amtTF))

      gridPane.add(outcomeTF, 0, row)
      gridPane.add(amtTF, 1, row)

      nextRow += 1
    }

    val feeRateTF = new TextField() {
      text = GlobalData.feeRate.toLong.toString
      promptText = "(optional)"
    }

    val collateralTF = new TextField() {
      promptText = "Bitcoins"
    }

    val refundDatePicker = new DatePicker()

    def addRemainingFields(): Unit = {
      gridPane.add(
        new Label("Your Collateral") {
          tooltip =
            Tooltip("How much funds you will be putting up for this DLC.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      gridPane.add(collateralTF, 1, nextRow)
      nextRow += 1

      gridPane.add(
        new Label("Fee Rate (sats/vbyte)") {
          tooltip = Tooltip(
            "Fee rate to be used for both funding and closing transactions.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      gridPane.add(feeRateTF, 1, nextRow)
      nextRow += 1

      gridPane.add(
        new Label("Refund Date") {
          tooltip = Tooltip(
            "If no oracle signatures are given, the DLC can be refunded after this date.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      gridPane.add(refundDatePicker, 1, nextRow)
      nextRow += 1
    }

    announcementTF.onKeyTyped = _ => {
      if (!outcomesAdded) {
        Try(OracleAnnouncementV0TLV.fromHex(announcementTF.text.value)) match {
          case Failure(_) => ()
          case Success(announcement) =>
            announcement.eventTLV.eventDescriptor match {
              case EnumEventDescriptorV0TLV(outcomes) =>
                outcomesAdded = true
                gridPane.add(new Label("Event Id"), 0, 1)
                gridPane.add(new TextField() {
                               text = announcement.eventTLV.eventId
                               editable = false
                             },
                             1,
                             1)

                gridPane.add(new Label("Outcomes"), 0, 2)
                gridPane.add(new Label("Values"), 1, 2)
                outcomes.foreach(str => addOutcomeRow(str.normStr))
                addRemainingFields()
                dialog.dialogPane().getScene.getWindow.sizeToScene()
              case _: NumericEventDescriptorTLV =>
                () // todo not supported
            }
        }
      }
    }

    dialog.dialogPane().content = new ScrollPane {
      content = new VBox(gridPane)
    }

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val missingOutcomes = fields.values.filter(_._2.text.value.isEmpty)
        if (missingOutcomes.nonEmpty) {
          val missing = missingOutcomes.map(_._1.text.value).mkString(", ")
          throw new RuntimeException(
            s"You missed outcomes $missing. Please enter payouts for these situations")
        }

        val inputs = fields.values.flatMap { case (str, value) =>
          if (str.text.value.nonEmpty && value.text.value.nonEmpty) {
            val amount = numberFormatter.parse(value.text.value).doubleValue()
            Some((str.text(), amount))
          } else None
        }
        val contractMap = inputs.map { case (str, value) =>
          EnumOutcome(str) -> Bitcoins(value).satoshis
        }.toVector

        val descriptor = EnumContractDescriptor(contractMap)

        val announcement = OracleAnnouncementV0TLV(announcementTF.text.value)
        val oracleInfo = EnumSingleOracleInfo(announcement)

        val contractInfo = ContractInfo(descriptor, oracleInfo).toTLV

        val collateral = Bitcoins(collateralTF.text.value.toDouble)

        val feeRateStr = feeRateTF.text.value

        val feeRateOpt = if (feeRateStr.nonEmpty) {
          Some(SatoshisPerVirtualByte(Satoshis(BigInt(feeRateStr))))
        } else None

        val refundLocktime = {
          val value = refundDatePicker.delegate.getValue
          val instant = value.atStartOfDay(ZoneOffset.UTC).toInstant

          UInt32(instant.getEpochSecond)
        }

        Some(
          CreateDLCOffer(
            contractInfo = contractInfo,
            collateral = collateral.satoshis,
            feeRateOpt = feeRateOpt,
            locktime = UInt32.zero,
            refundLT = refundLocktime
          ))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(Some(offer: CreateDLCOffer)) =>
        Some(offer)
      case Some(_) | None => None
    }
  }
}
