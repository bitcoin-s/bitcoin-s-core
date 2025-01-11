package org.bitcoins.wallet

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.api.wallet.NeutrinoWalletApi.BlockMatchingResponse
import org.bitcoins.core.api.wallet.db.{AddressDb, TransactionDbHelper}
import org.bitcoins.core.hd.HDChainType.{Change, External}
import org.bitcoins.core.hd.{AddressType, HDAccount, HDChainType}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.*
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{CryptoUtil, ECDigitalSignature, ECPublicKey}
import org.bitcoins.keymanager.{DecryptedMnemonic, WalletStorage}
import org.bitcoins.testkit.chain.MockChainQueryApi
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkitcore.util.TransactionTestUtil.*
import org.scalatest.FutureOutcome
import org.scalatest.compatible.Assertion

import java.nio.file.Files
import java.time.Instant
import scala.concurrent.Future

class WalletUnitTest extends BitcoinSWalletTest {

  override type FixtureParam = Wallet

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withNewWallet(test)(getFreshWalletAppConfig)

  behavior of "Wallet - unit test"

  it must "write the mnemonic seed in the correct directory" in { wallet =>
    assert(Files.exists(wallet.walletConfig.seedPath))
  }

  it should "create a new wallet" in { (wallet: Wallet) =>
    for {
      accounts <- wallet.accountHandling.getAccounts()
      addresses <- wallet.addressHandling.getAddresses()
    } yield {
      assert(accounts.length == 4) // legacy, segwit, nested segwit, taproot
      assert(addresses.isEmpty)
    }
  }

  it should "generate addresses" in { (wallet: Wallet) =>
    for {
      addr <- wallet.getNewAddress()
      otherAddr <- wallet.getNewAddress()
      allAddrs <- wallet.addressHandling.getAddresses()
    } yield {
      assert(allAddrs.length == 2)
      assert(allAddrs.exists(_.address == addr))
      assert(allAddrs.exists(_.address == otherAddr))
    }
  }

  it should "know what the last address index is" in { wallet =>
    def getMostRecent(
        hdAccount: HDAccount,
        chain: HDChainType
    ): Future[AddressDb] = {
      val recentOptFut: Future[Option[AddressDb]] = chain match {
        case Change =>
          wallet.addressDAO.findMostRecentChange(hdAccount)
        case External =>
          wallet.addressDAO.findMostRecentExternal(hdAccount)
      }

      recentOptFut.map {
        case Some(addr) => addr
        case None       => fail(s"Did not find $chain address!")
      }
    }

    def assertIndexIs(
        hdAccount: HDAccount,
        chain: HDChainType,
        addrIndex: Int
    ): Future[Assertion] = {
      getMostRecent(hdAccount, chain) map { addr =>
        assert(addr.path.address.index == addrIndex)
      }
    }

    val addressesToGenerate = 10
    val addrRange = 0 to addressesToGenerate

    /** Generate some addresses, and verify that the correct address index is
      * being reported
      */
    def testChain(
        hdAccount: HDAccount,
        chain: HDChainType
    ): Future[Assertion] = {
      val getAddrFunc: () => Future[BitcoinAddress] = chain match {
        case Change   => (() => wallet.getNewChangeAddress())
        case External => (() => wallet.getNewAddress())
      }
      for {
        _ <- {
          val addrF = chain match {
            case Change =>
              wallet.addressDAO.findMostRecentChange(hdAccount)
            case External =>
              wallet.addressDAO.findMostRecentExternal(hdAccount)
          }
          addrF.map {
            case Some(addr) =>
              fail(
                s"Found ${addr.address} $chain address although there was no previous addreses generated"
              )
            case None =>
          }
        }
        _ <- FutureUtil.sequentially(addrRange)(_ => getAddrFunc())
        _ <- assertIndexIs(hdAccount, chain, addrIndex = addressesToGenerate)
        newest <- getAddrFunc()
        res <- getMostRecent(hdAccount, chain).map { found =>
          assert(found.address == newest)
          assert(found.path.address.index == addressesToGenerate + 1)
        }
      } yield res
    }

    for {
      account <- wallet.accountHandling.getDefaultAccount()
      _ <- testChain(hdAccount = account.hdAccount, External)
      res <- testChain(hdAccount = account.hdAccount, Change)
    } yield res
  }

  it should "match block filters" in { (wallet: Wallet) =>
    for {
      height <- wallet.chainQueryApi.getFilterCount()
      filtersResponse <- chainQueryApi.getFiltersBetweenHeights(
        startHeight = 0,
        endHeight = height
      )
      matched <- wallet.rescanHandling.findMatches(
        filters = filtersResponse,
        scripts = Vector(
          // this is a random address which is included into the test block
          BitcoinAddress("n1RH2x3b3ah4TGQtgrmNAHfmad9wr8U2QY").scriptPubKey
        ),
        parallelismLevel = 1
      )
    } yield {
      assert(
        Vector(
          BlockMatchingResponse(
            blockHash = MockChainQueryApi.testBlockHash,
            blockHeight = 1
          )
        ) == matched
      )
    }
  }

  it must "be able to call initialize twice without throwing an exception if we have the same key manager" in {
    (wallet: Wallet) =>
      val bip39PasswordOpt = wallet.walletConfig.bip39PasswordOpt
      val twiceF = Wallet
        .initialize(wallet = wallet,
                    accountHandling = wallet.accountHandling,
                    bip39PasswordOpt = bip39PasswordOpt)
        .flatMap { _ =>
          Wallet.initialize(wallet = wallet,
                            accountHandling = wallet.accountHandling,
                            bip39PasswordOpt = bip39PasswordOpt)
        }

      twiceF.map(_ => succeed)

  }

  it must "be able to detect an incompatible key manager with a wallet" in {
    (wallet: Wallet) =>
      val bip39PasswordOpt = wallet.walletConfig.bip39PasswordOpt
      recoverToSucceededIf[RuntimeException] {
        Wallet
          .initialize(wallet, wallet.accountHandling, bip39PasswordOpt)
          .flatMap { _ =>
            // use a BIP39 password to make the key-managers different
            Wallet.initialize(
              wallet,
              wallet.accountHandling,
              Some("random-password-to-make-key-managers-different")
            )
          }
      }
  }

  it must "be able to detect different master xpubs on wallet startup" in {
    (wallet: Wallet) =>
      // create new config with different entropy
      // to make the keymanagers differetn
      val config = ConfigFactory.parseString(
        s"bitcoin-s.keymanager.entropy=${CryptoUtil.randomBytes(16).toHex}"
      )
      val uniqueEntropyWalletConfig = wallet.walletConfig.withOverrides(config)
      val startedF = uniqueEntropyWalletConfig.start()
      val walletDiffKeyManagerF: Future[Wallet] = for {
        _ <- startedF
      } yield {
        Wallet(wallet.nodeApi, wallet.chainQueryApi)(
          uniqueEntropyWalletConfig
        )
      }

      recoverToSucceededIf[IllegalArgumentException] {
        walletDiffKeyManagerF.flatMap { walletDiffKeyManager =>
          Wallet.initialize(walletDiffKeyManager, wallet.accountHandling, None)
        }
      }
  }

  it must "be able to sign a psbt with a key path" in { (wallet: Wallet) =>
    val dummyKey = ECPublicKey.freshPublicKey

    for {
      accountDb <- wallet.accountDAO.findAll().map(_.head)
      addr <- wallet.accountHandling.getNewAddress(accountDb)
      addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
      walletKey = addrDb.ecPublicKey
      walletPath = addrDb.path

      spk = MultiSignatureScriptPubKey(2, Vector(dummyKey, walletKey))
      dummyPrevTx = dummyTx(spk = spk)
      prevTxDb = TransactionDbHelper.fromTransaction(dummyPrevTx, None)
      _ <- wallet.transactionDAO.create(prevTxDb)

      psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)
        .addKeyPathToInput(accountDb.xpub, walletPath, walletKey, 0)

      signed <- wallet.sendFundsHandling.signPSBT(psbt)
    } yield {
      assert(signed != psbt)
      assert(
        signed.inputMaps.head
          .partialSignatures[ECDigitalSignature]
          .exists(_.pubKey.toPublicKey == walletKey)
      )
    }
  }

  it must "be able to sign a psbt with our own p2pkh utxo" in {
    (wallet: Wallet) =>
      for {
        addr <- wallet.addressHandling.getNewAddress(AddressType.Legacy)
        addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
        walletKey = addrDb.ecPublicKey

        spk = addr.scriptPubKey
        _ = assert(spk == P2PKHScriptPubKey(walletKey))
        dummyPrevTx = dummyTx(spk = spk)
        _ <- wallet.transactionProcessing.processTransaction(
          dummyPrevTx,
          blockHashWithConfsOpt = None)

        psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)

        signed <- wallet.sendFundsHandling.signPSBT(psbt)
      } yield {
        assert(signed != psbt)
        assert(
          signed.inputMaps.head
            .partialSignatures[ECDigitalSignature]
            .exists(_.pubKey.toPublicKey == walletKey)
        )
      }
  }

  it must "be able to sign a psbt with our own p2sh segwit utxo" in {
    (wallet: Wallet) =>
      for {
        addr <- wallet.addressHandling.getNewAddress(AddressType.NestedSegWit)
        addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
        walletKey = addrDb.ecPublicKey

        spk = addr.scriptPubKey
        _ = assert(spk == P2SHScriptPubKey(P2WPKHWitnessSPKV0(walletKey)))
        dummyPrevTx = dummyTx(spk = spk)
        _ <- wallet.transactionProcessing.processTransaction(
          dummyPrevTx,
          blockHashWithConfsOpt = None)

        psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)

        signed <- wallet.sendFundsHandling.signPSBT(psbt)
      } yield {
        assert(signed != psbt)
        assert(
          signed.inputMaps.head
            .partialSignatures[ECDigitalSignature]
            .exists(_.pubKey.toPublicKey == walletKey)
        )
      }
  }

  it must "be able to sign a psbt with our own p2wpkh utxo" in {
    (wallet: Wallet) =>
      for {
        addr <- wallet.addressHandling.getNewAddress(AddressType.SegWit)
        addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
        walletKey = addrDb.ecPublicKey

        spk = addr.scriptPubKey
        _ = assert(spk == P2WPKHWitnessSPKV0(walletKey))
        dummyPrevTx = dummyTx(spk = spk)
        _ <- wallet.transactionProcessing.processTransaction(
          dummyPrevTx,
          blockHashWithConfsOpt = None)

        psbt = dummyPSBT(prevTxId = dummyPrevTx.txId)
          .addUTXOToInput(dummyPrevTx, 0)

        signed <- wallet.sendFundsHandling.signPSBT(psbt)
      } yield {
        assert(signed != psbt)
        assert(
          signed.inputMaps.head
            .partialSignatures[ECDigitalSignature]
            .exists(_.pubKey.toPublicKey == walletKey)
        )
      }
  }

  it must "be able to sign a psbt with no wallet utxos" in { (wallet: Wallet) =>
    val psbt = dummyPSBT()
    for {
      signed <- wallet.sendFundsHandling.signPSBT(psbt)
    } yield assert(signed == psbt)
  }

  it must "get correct txs to broadcast" in { (wallet: Wallet) =>
    for {
      addr <- wallet.addressHandling.getNewAddress(AddressType.SegWit)
      addrDb <- wallet.addressDAO.findAddress(addr).map(_.get)
      walletKey = addrDb.ecPublicKey

      spk = addr.scriptPubKey
      _ = assert(spk == P2WPKHWitnessSPKV0(walletKey))
      dummyPrevTx = dummyTx(spk = spk)
      _ <- wallet.transactionProcessing.processTransaction(
        dummyPrevTx,
        blockHashWithConfsOpt = None)

      dummyPrevTx1 = dummyTx(prevTxId = dummyPrevTx.txId, spk = spk)
      _ <- wallet.transactionProcessing.processTransaction(
        dummyPrevTx1,
        blockHashWithConfsOpt = None)

      toBroadcast <- wallet.sendFundsHandling.getTransactionsToBroadcast
    } yield assert(toBroadcast.map(_.txIdBE) == Vector(dummyPrevTx1.txIdBE))
  }

  it must "generate addresses for a wallet initialized with a old seed" in {
    (wallet: Wallet) =>
      for {
        isEmpty <- wallet.isEmpty()
        _ = assert(isEmpty)
        // manually override the seeds creation time
        seedPath = wallet.walletConfig.kmConf.seedPath
        mnemonic = WalletStorage
          .decryptSeedFromDisk(seedPath = seedPath, passphraseOpt = None)
          .getOrElse(sys.error(s"failed to decrypt seed for unit test"))
          .asInstanceOf[DecryptedMnemonic]
        _ = {
          // delete old seed file because we do not allow overwriting a seed file
          Files.delete(wallet.walletConfig.seedPath)
        }
        modifiedMnemonic = mnemonic.copy(creationTime =
          Instant.now.minusSeconds(60 * 60 + 1)) // 1 hour and 1 minute

        _ = WalletStorage.writeSeedToDisk(seedPath, modifiedMnemonic)
        // delete old wallet database
        _ = {
          if (pgEnabled) {
            // cannot delete database file if using postgres
            ()
          } else {
            val path = wallet.walletConfig.datadir
              .resolve(wallet.walletConfig.walletName)
              .resolve("walletdb.sqlite")
            Files.delete(path)
          }

        }
        _ = wallet.walletConfig.migrate()
        // initialize it
        initOldWallet <- Wallet.initialize(
          wallet = wallet,
          accountHandling = wallet.accountHandling,
          bip39PasswordOpt = wallet.walletConfig.bip39PasswordOpt
        )
        isOldWalletEmpty <- initOldWallet.isEmpty()
      } yield assert(!isOldWalletEmpty)

  }
}
