package org.bitcoins.gui.dlc

import org.bitcoins.core.protocol.dlc.DLCStatus._
import org.bitcoins.core.protocol.dlc.{AcceptedDLCStatus, DLCStatus}
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.control.{ContextMenu, MenuItem, TableColumn, TableView}

class DLCTableView(model: DLCPaneModel) {

  val tableView: TableView[DLCStatus] = {

    val eventIdCol = new TableColumn[DLCStatus, String] {
      text = "Event Id"
      prefWidth = 150
      cellValueFactory = { status =>
        val eventIdStr =
          status.value.oracleInfo.singleOracleInfos.head.announcement.eventTLV.eventId

        new StringProperty(status, "Event Id", eventIdStr)
      }
    }

    val contractIdCol = new TableColumn[DLCStatus, String] {
      text = "Contract Id"
      prefWidth = 150
      cellValueFactory = { status =>
        val contractIdStr = status.value match {
          case _: Offered => ""
          case signed: AcceptedDLCStatus =>
            signed.contractId.toHex
        }

        new StringProperty(status, "Contract Id", contractIdStr)
      }
    }

    val statusCol = new TableColumn[DLCStatus, String] {
      text = "Status"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status, "Status", status.value.statusString)
      }
    }

    val initiatorCol = new TableColumn[DLCStatus, String] {
      text = "Initiator"
      prefWidth = 80
      cellValueFactory = { status =>
        val str = if (status.value.isInitiator) {
          "Yes"
        } else {
          "No"
        }
        new StringProperty(status, "Initiator", str)
      }
    }

    val collateralCol = new TableColumn[DLCStatus, String] {
      text = "Collateral"
      prefWidth = 110
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Collateral",
                           status.value.localCollateral.toString)
      }
    }

    val otherCollateralCol = new TableColumn[DLCStatus, String] {
      text = "Counter Party Collateral"
      prefWidth = 200
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Counter Party Collateral",
                           status.value.remoteCollateral.toString)
      }
    }

    val totalCollateralCol = new TableColumn[DLCStatus, String] {
      text = "Total Collateral"
      prefWidth = 150
      cellValueFactory = { status =>
        new StringProperty(status,
                           "Total Collateral",
                           status.value.totalCollateral.toString)
      }
    }

    new TableView[DLCStatus](model.dlcs) {
      columns ++= Seq(eventIdCol,
                      contractIdCol,
                      statusCol,
                      initiatorCol,
                      collateralCol,
                      otherCollateralCol,
                      totalCollateralCol)
      margin = Insets(10, 0, 10, 0)

      val infoItem: MenuItem = new MenuItem("View DLC") {
        onAction = _ => {
          val dlc = selectionModel.value.getSelectedItem
          model.viewDLC(dlc)
        }
      }

      contextMenu = new ContextMenu() {
        items ++= Vector(infoItem)
      }
    }
  }
}
