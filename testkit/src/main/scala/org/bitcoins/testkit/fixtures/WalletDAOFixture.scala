package org.bitcoins.testkit.fixtures

import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest._
import org.scalatest.flatspec.FixtureAsyncFlatSpec
import org.bitcoins.wallet.models._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement

case class WalletDAOs(
    accountDAO: AccountDAO,
    addressDAO: AddressDAO,
    utxoDAO: SpendingInfoDAO,
    incomingTxDAO: IncomingTransactionDAO,
    outgoingTxDAO: OutgoingTransactionDAO)

trait WalletDAOFixture extends FixtureAsyncFlatSpec with BitcoinSWalletTest {

  private lazy val daos: WalletDAOs = {
    val account = AccountDAO()
    val address = AddressDAO()
    val utxo = SpendingInfoDAO()
    val incomingTxDAO = IncomingTransactionDAO()
    val outgoingTxDAO = OutgoingTransactionDAO()
    WalletDAOs(account, address, utxo, incomingTxDAO, outgoingTxDAO)
  }

  final override type FixtureParam = WalletDAOs

  implicit private val walletConfig: WalletAppConfig = config

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () => WalletDbManagement.createAll().map(_ => daos),
                destroy = () => WalletDbManagement.dropAll())(test)
}
