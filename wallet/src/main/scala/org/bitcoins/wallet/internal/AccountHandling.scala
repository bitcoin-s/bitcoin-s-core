package org.bitcoins.wallet.internal

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.wallet.AccountHandlingApi
import org.bitcoins.core.api.wallet.db.{AccountDb, AddressDb, AddressDbHelper}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.AddressType.*
import org.bitcoins.core.hd.*
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  SigNetChainParams,
  TestNetChainParams
}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.HDUtil
import org.bitcoins.db.SafeDatabase
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  ScriptPubKeyDAO,
  SpendingInfoDAO,
  WalletDAOs
}
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Provides functionality related enumerating accounts. Account creation does
  * not happen here, as that requires an unlocked wallet.
  */
case class AccountHandling(
    walletDAOs: WalletDAOs,
    keyManager: BIP39KeyManagerApi)(implicit
    walletConfig: WalletAppConfig,
    ec: ExecutionContext)
    extends AccountHandlingApi
    with BitcoinSLogger {
  private val accountDAO: AccountDAO = walletDAOs.accountDAO
  private val spendingInfoDAO: SpendingInfoDAO = walletDAOs.utxoDAO
  private val addressDAO: AddressDAO = walletDAOs.addressDAO
  private val scriptPubKeyDAO: ScriptPubKeyDAO = walletDAOs.scriptPubKeyDAO
  private val safeDatabase: SafeDatabase = spendingInfoDAO.safeDatabase
  private val chainParams: ChainParams = walletConfig.chain
  private val networkParameters: NetworkParameters = walletConfig.network
  private def walletCallbacks: WalletCallbacks = walletConfig.callBacks

  override def createNewAccount(
      hdAccount: HDAccount
  ): Future[ExtPublicKey] = {
    logger.info(
      s"Creating new account at index ${hdAccount.index} for purpose ${hdAccount.purpose}"
    )

    val xpub: ExtPublicKey = {
      keyManager.deriveXPub(hdAccount) match {
        case Failure(exception) =>
          // this won't happen, because we're deriving from a privkey
          // this should really be changed in the method signature
          logger.error(s"Unexpected error when deriving xpub: $exception")
          throw exception
        case Success(xpub) => xpub
      }
    }
    val newAccountDb = AccountDb(xpub, hdAccount)
    accountDAO.create(newAccountDb).map { created =>
      logger.debug(s"Created new account ${created.hdAccount}")
      created.xpub
    }
  }

  /** Creates a new account my reading from our account database, finding the
    * last account, and then incrementing the account index by one, and then
    * creating that account
    *
    * @return
    */
  override def createNewAccount(purpose: HDPurpose): Future[ExtPublicKey] = {
    getLastAccountOpt(purpose).flatMap {
      case Some(accountDb) =>
        val hdAccount = accountDb.hdAccount
        val newAccount = hdAccount.copy(index = hdAccount.index + 1)
        createNewAccount(newAccount)
      case None =>
        createNewAccount(walletConfig.defaultAccount)
    }
  }

  override def getUnconfirmedBalance(
      account: HDAccount
  ): Future[CurrencyUnit] = {
    safeDatabase.run(spendingInfoDAO.getUnconfirmedBalanceAction(Some(account)))
  }

  override def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit] = {
    safeDatabase.run(spendingInfoDAO.getConfirmedBalanceAction(Some(account)))
  }

  /** @inheritdoc */
  override def getAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

  private def getOrThrowAccount(account: Option[AccountDb]): AccountDb =
    account.getOrElse(
      throw new RuntimeException(
        s"Could not find account with ${DEFAULT_HD_COIN.purpose.constant} " +
          s"purpose field and ${DEFAULT_HD_COIN.coinType.toInt} coin field"
      )
    )

  /** @inheritdoc */
  override def getDefaultAccount(): Future[AccountDb] = {
    for {
      account <- accountDAO.read((DEFAULT_HD_COIN, 0))
    } yield {

      val acct = getOrThrowAccount(account)
      require(
        acct.hdAccount == walletConfig.defaultAccount,
        s"Divergence between configured default account and " +
          s"database default account walletConfig=${walletConfig.defaultAccount} database=${acct.hdAccount}"
      )
      acct
    }
  }

  /** @inheritdoc */
  override def getDefaultAccountForType(
      addressType: AddressType
  ): Future[AccountDb] = {
    val hdCoin = addressType match {
      case Legacy       => HDCoin(HDPurpose.Legacy, DEFAULT_HD_COIN_TYPE)
      case NestedSegWit => HDCoin(HDPurpose.NestedSegWit, DEFAULT_HD_COIN_TYPE)
      case SegWit       => HDCoin(HDPurpose.SegWit, DEFAULT_HD_COIN_TYPE)
      case P2TR         => HDCoin(HDPurpose.Taproot, DEFAULT_HD_COIN_TYPE)
    }
    for {
      account <- accountDAO.read((hdCoin, 0))
    } yield getOrThrowAccount(account)
  }

  override def clearUtxos(account: HDAccount): Future[Unit] = {
    val aggregatedActions
        : DBIOAction[Unit, NoStream, Effect.Read & Effect.Write] = {
      for {
        accountUtxos <- spendingInfoDAO.findAllForAccountAction(account)
        _ <- spendingInfoDAO.deleteSpendingInfoDbAllAction(accountUtxos)
      } yield ()
    }

    safeDatabase.run(aggregatedActions)
  }

  override def generateScriptPubKeys(
      account: HDAccount,
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): Future[Vector[ScriptPubKey]] = {
    val action = generateScriptPubKeysAction(
      account = account,
      addressBatchSize = addressBatchSize,
      forceGenerateSpks = forceGenerateSpks
    )
    safeDatabase.run(action)
  }

  /** If forceGeneratSpks is true or addressCount == 0 we generate a new pool of
    * scriptpubkeys
    */
  private def generateScriptPubKeysAction(
      account: HDAccount,
      addressBatchSize: Int,
      forceGenerateSpks: Boolean
  ): DBIOAction[Vector[
                  ScriptPubKey
                ],
                NoStream,
                Effect.Read & Effect.Write & Effect.Transactional] = {
    val addressCountA = addressDAO.countAction
    for {
      addressCount <- addressCountA
      addresses <- {
        if (forceGenerateSpks || addressCount == 0) {
          logger.info(
            s"Generating $addressBatchSize fresh addresses for the rescan"
          )
          generateAddressesForRescanAction(account, addressBatchSize)
        } else {
          // we don't want to continously generate addresses
          // if our wallet already has them, so just use what is in the
          // database already
          addressDAO.findAllAddressesAction().map(_.map(_.address))
        }
      }
      spksDb <- scriptPubKeyDAO.findAllAction()
    } yield {
      val addrSpks =
        addresses.map(_.scriptPubKey)
      val otherSpks = spksDb.map(_.scriptPubKey)

      (addrSpks ++ otherSpks).distinct
    }
  }

  private def generateAddressesForRescanAction(
      account: HDAccount,
      addressBatchSize: Int
  ): DBIOAction[Vector[
                  BitcoinAddress
                ],
                NoStream,
                Effect.Read & Effect.Write & Effect.Transactional] = {
    val receiveAddressesA
        : DBIOAction[Vector[
                       BitcoinAddress
                     ],
                     NoStream,
                     Effect.Read & Effect.Write & Effect.Transactional] = {
      DBIOAction.sequence {
        1.to(addressBatchSize)
          .map(_ => getNewAddressAction(account))
      }
    }.map(_.toVector)

    val changeAddressesA
        : DBIOAction[Vector[
                       BitcoinAddress
                     ],
                     NoStream,
                     Effect.Read & Effect.Write & Effect.Transactional] = {
      DBIOAction.sequence {
        1.to(addressBatchSize)
          .map(_ => getNewChangeAddressAction(account))
      }
    }.map(_.toVector)

    for {
      receiveAddresses <- receiveAddressesA
      changeAddresses <- changeAddressesA
    } yield receiveAddresses ++ changeAddresses
  }

  override def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress] = {
    val action = getNewChangeAddressAction(account)
    safeDatabase.run(action)
  }

  /** Queues a request to generate an address and returns a Future that will be
    * completed when the request is processed in the queue. If the queue is full
    * it throws an exception.
    * @throws IllegalStateException
    */
  override def getNewAddress(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress] = {
    val action = getNewAddressHelperAction(account, chainType)
    safeDatabase.run(action)
  }

  def getNewAddressAction(account: HDAccount): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read & Effect.Write & Effect.Transactional
  ] = {
    val accountDbOptA = findAccountAction(account)
    accountDbOptA.flatMap {
      case Some(accountDb) => getNewAddressAction(accountDb)
      case None =>
        DBIOAction.failed(
          new RuntimeException(
            s"No account found for given hdaccount=${account}"
          )
        )
    }
  }

  def getNewChangeAddressAction(account: HDAccount): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read & Effect.Write & Effect.Transactional
  ] = {
    val accountDbOptA = findAccountAction(account)
    accountDbOptA.flatMap {
      case Some(accountDb) => getNewChangeAddressAction(accountDb)
      case None =>
        DBIOAction.failed(
          new RuntimeException(
            s"No account found for given hdaccount=${account}"
          )
        )
    }
  }

  override def getNewAddress(account: AccountDb): Future[BitcoinAddress] = {
    val action = getNewAddressAction(account)
    checkRootAccount.flatMap(_ => safeDatabase.run(action))
  }

  def getNewAddressAction(account: AccountDb): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read & Effect.Write & Effect.Transactional
  ] = {
    getNewAddressHelperAction(account, HDChainType.External)
  }

  def getNewChangeAddressAction(account: AccountDb): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read & Effect.Write & Effect.Transactional
  ] = {
    getNewAddressHelperAction(account, HDChainType.Change)
  }

  private def findAccountAction(
      account: HDAccount
  ): DBIOAction[Option[AccountDb], NoStream, Effect.Read] = {
    accountDAO.findByAccountAction(account)
  }

  override def findAccount(account: HDAccount): Future[Option[AccountDb]] = {
    safeDatabase.run(findAccountAction(account))
  }

  override def getUnusedAddresses(
      account: HDAccount): Future[Vector[AddressDb]] = {
    val unusedAddressesF = addressDAO.getUnusedAddresses
    unusedAddressesF.map { unusedAddresses =>
      unusedAddresses.filter(addr =>
        HDAccount.isSameAccount(addr.path, account))
    }
  }

  override def getAddresses(account: HDAccount): Future[Vector[AddressDb]] = {
    val allAddressesF: Future[Vector[AddressDb]] = addressDAO.findAllAddresses()

    val accountAddressesF = {
      allAddressesF.map { addresses =>
        addresses.filter { a =>
          logger.debug(s"a.path=${a.path} account=${account}")
          HDAccount.isSameAccount(a.path, account)
        }
      }
    }

    accountAddressesF
  }

  override def getSpentAddresses(
      account: HDAccount
  ): Future[Vector[AddressDb]] = {
    addressDAO.getSpentAddresses(account)
  }

  override def getFundedAddresses(
      account: HDAccount
  ): Future[Vector[(AddressDb, CurrencyUnit)]] = {
    addressDAO.getFundedAddresses(account)
  }

  private def getNewAddressHelperAction(
      account: AccountDb,
      chainType: HDChainType
  ): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read & Effect.Write & Effect.Transactional
  ] = {
    logger.debug(s"Processing $account $chainType in our address request queue")
    val resultA: DBIOAction[
      BitcoinAddress,
      NoStream,
      Effect.Read & Effect.Write & Effect.Transactional
    ] = for {
      addressDb <- getNewAddressDbAction(account, chainType)
      writtenAddressDb <- addressDAO.createAction(addressDb)
    } yield {
      logger.info(
        s"Generated new address=${addressDb.address} path=${addressDb.path} isChange=${addressDb.isChange}"
      )
      writtenAddressDb.address
    }

    val callbackExecuted = resultA.flatMap { address =>
      val executedF =
        walletCallbacks.executeOnNewAddressGenerated(address)
      DBIOAction
        .from(executedF)
        .map(_ => address)
    }

    callbackExecuted
  }

  private def getNewAddressDbAction(
      account: AccountDb,
      chainType: HDChainType
  ): DBIOAction[AddressDb, NoStream, Effect.Read] = {
    logger.debug(s"Getting new $chainType adddress for ${account.hdAccount}")

    val lastAddrOptA = chainType match {
      case HDChainType.External =>
        addressDAO.findMostRecentExternalAction(account.hdAccount)
      case HDChainType.Change =>
        addressDAO.findMostRecentChangeAction(account.hdAccount)
    }

    lastAddrOptA.map { lastAddrOpt =>
      val addrPath: HDPath = lastAddrOpt match {
        case Some(addr) =>
          val next = addr.path.next
          logger.debug(
            s"Found previous address at path=${addr.path}, next=$next"
          )
          next
        case None =>
          val address = account.hdAccount
            .toChain(chainType)
            .toHDAddress(0)

          val path = address.toPath
          logger.debug(s"Did not find previous address, next=$path")
          path
      }

      val pathDiff =
        account.hdAccount.diff(addrPath) match {
          case Some(value) => value
          case None =>
            throw new RuntimeException(
              s"Could not diff ${account.hdAccount} and $addrPath"
            )
        }

      val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
        case Failure(exception) => throw exception
        case Success(value)     => value.key
      }

      addrPath match {
        case segwitPath: SegWitHDPath =>
          AddressDbHelper
            .getSegwitAddress(pubkey, segwitPath, networkParameters)
        case legacyPath: LegacyHDPath =>
          AddressDbHelper.getLegacyAddress(
            pubkey,
            legacyPath,
            networkParameters
          )
        case nestedPath: NestedSegWitHDPath =>
          AddressDbHelper.getNestedSegwitAddress(
            pubkey,
            nestedPath,
            networkParameters
          )
        case taprootHDPath: TaprootHDPath =>
          AddressDbHelper.getTaprootAddress(pubkey,
                                            taprootHDPath,
                                            networkParameters)
        case h: HDPath =>
          sys.error(s"Unsupported HDPath type=$h for calculating addresses")
      }
    }
  }

  private def getLastAccountOpt(
      purpose: HDPurpose
  ): Future[Option[AccountDb]] = {
    accountDAO
      .findAll()
      .map(_.filter(_.hdAccount.purpose == purpose))
      .map(_.sortBy(_.hdAccount.index))
      // we want to the most recently created account,
      // to know what the index of our new account
      // should be.
      .map(_.lastOption)
  }

  private def checkRootAccount: Future[Unit] = {
    val coinType = HDUtil.getCoinType(keyManager.kmParams.network)
    val coin =
      HDCoin(purpose = keyManager.kmParams.purpose, coinType = coinType)
    val account = HDAccount(coin = coin, index = 0)
    // safe since we're deriving from a priv
    val xpub = keyManager.deriveXPub(account).get

    accountDAO.read((account.coin, account.index)).flatMap {
      case Some(account) =>
        if (account.xpub != xpub) {
          val errorMsg =
            s"Divergent xpubs for account=$account. Existing database xpub=${account.xpub}, key manager's xpub=$xpub. " +
              s"It is possible we have a different key manager being used than expected, key manager=${keyManager.kmParams.seedPath.toAbsolutePath.toString}"
          Future.failed(new RuntimeException(errorMsg))
        } else {
          Future.unit
        }
      case None =>
        val errorMsg = s"Missing root xpub for account $account in database"
        Future.failed(new RuntimeException(errorMsg))
    }
  }

  /** The default HD coin for this wallet, read from config */
  protected[wallet] lazy val DEFAULT_HD_COIN: HDCoin = {
    val coinType = DEFAULT_HD_COIN_TYPE
    HDCoin(walletConfig.defaultPurpose, coinType)
  }

  /** The default HD coin type for this wallet, derived from the network we're
    * on
    */
  protected[wallet] lazy val DEFAULT_HD_COIN_TYPE: HDCoinType = {
    chainParams match {
      case MainNetChainParams => HDCoinType.Bitcoin
      case RegTestNetChainParams | TestNetChainParams | SigNetChainParams(_) =>
        HDCoinType.Testnet

    }

  }

  /** The default HD purpose for this wallet, read from config */
  protected[wallet] lazy val DEFAULT_HD_PURPOSE: HDPurpose =
    walletConfig.defaultPurpose
}
