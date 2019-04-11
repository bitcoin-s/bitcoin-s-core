package org.bitcoins.wallet

import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.wallet.api.{
  AddUtxoError,
  AddUtxoResult,
  AddUtxoSuccess,
  InitializeWalletError,
  InitializeWalletSuccess,
  WalletApi
}
import org.bitcoins.wallet.util.BitcoinSWalletTest

import scala.concurrent.Future

class WalletIntegrationTest extends BitcoinSWalletTest {
  behavior of "Wallet - integration test"

  val feeRate = SatoshisPerByte(Satoshis.one)

  it should ("create an address, receive funds to it from bitcoind, import the"
    + " UTXO and construct a valid, signed transaction that's"
    + " broadcast and confirmed by bitcoind") in {
    val valueFromBitcoind = Bitcoins.one
    val walletF =
      Wallet.initialize(chainParams = chainParams, dbConfig = dbConfig).map {
        case InitializeWalletSuccess(wallet) => wallet
        case err: InitializeWalletError      => fail(err)
      }

    val bitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

    val addUtxoF: Future[Unit] = for {
      bitcoind <- bitcoindF

      wallet <- walletF
      addr <- wallet.getNewAddress()

      txid <- bitcoind.sendToAddress(addr, valueFromBitcoind)
      _ <- bitcoind.generate(6)
      tx <- bitcoind.getRawTransaction(txid)

      addUtxoRes <- {
        val voutOpt = tx.vout.find { rpcOut =>
          val addressesOpt = rpcOut.scriptPubKey.addresses
          addressesOpt.exists(_.contains(addr))
        }

        val vout = voutOpt.getOrElse(
          throw new IllegalArgumentException(
            "Could not find ouput that spent to our address!"))

        wallet.addUtxo(tx.hex, UInt32(vout.n))
      }
    } yield {
      addUtxoRes match {
        case err: AddUtxoError            => fail(err)
        case AddUtxoSuccess(w: WalletApi) => () // continue test
      }
    }
    for {
      _ <- addUtxoF
      wallet <- walletF
      bitcoind <- bitcoindF

      utxos <- wallet.listUtxos()
      _ = assert(utxos.nonEmpty)

      addressFromBitcoind <- bitcoind.getNewAddress
      signedTx <- wallet.sendToAddress(addressFromBitcoind,
                                       Bitcoins(0.5),
                                       feeRate)

      txid <- bitcoind.sendRawTransaction(signedTx)
      _ <- bitcoind.generate(1)
      tx <- bitcoind.getRawTransaction(txid)
    } yield {
      assert(tx.confirmations.exists(_ > 0))
    }
  }
}
