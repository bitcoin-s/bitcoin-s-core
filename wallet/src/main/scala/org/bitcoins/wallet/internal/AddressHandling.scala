package org.bitcoins.wallet.internal

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.hd._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.wallet._
import org.bitcoins.wallet.api.AddressInfo
import org.bitcoins.wallet.models.{AccountDb, AddressDb, AddressDbHelper}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Provides functionality related to addresses. This includes
  * enumeratng and creating them, primarily.
  */
private[wallet] trait AddressHandling extends WalletLogger {
  self: LockedWallet =>

  def contains(
      address: BitcoinAddress,
      accountOpt: Option[HDAccount]): Future[Boolean] = {
    val possibleAddressesF = accountOpt match {
      case Some(account) =>
        listAddresses(account)
      case None =>
        listAddresses()
    }

    possibleAddressesF.map { possibleAddresses =>
      possibleAddresses.exists(_.address == address)
    }
  }

  override def listAddresses(): Future[Vector[AddressDb]] =
    addressDAO.findAll()

  override def listAddresses(account: HDAccount): Future[Vector[AddressDb]] = {
    val allAddressesF: Future[Vector[AddressDb]] = listAddresses()

    val accountAddressesF = {
      allAddressesF.map { addresses =>
        addresses.filter { a =>
          logger.info(s"a.path=${a.path} account=${account}")
          HDAccount.isSameAccount(a.path, account)
        }
      }
    }

    accountAddressesF
  }

  /** Enumerates the public keys in this wallet */
  protected[wallet] def listPubkeys(): Future[Vector[ECPublicKey]] =
    addressDAO.findAllPubkeys()

  /** Enumerates the scriptPubKeys in this wallet */
  protected[wallet] def listSPKs(): Future[Vector[ScriptPubKey]] =
    addressDAO.findAllSPKs()

  /** Given a transaction, returns the outputs (with their corresponding outpoints)
    * that pay to this wallet */
  def findOurOuts(transaction: Transaction): Future[
    Vector[(TransactionOutput, TransactionOutPoint)]] =
    for {
      spks <- listSPKs()
    } yield transaction.outputs.zipWithIndex.collect {
      case (out, index) if spks.contains(out.scriptPubKey) =>
        (out, TransactionOutPoint(transaction.txId, UInt32(index)))
    }.toVector

  private val queue = mutable.ArrayBuffer.empty[(AccountDb,HDChainType, Promise[AddressDb])]

  /**
    * Derives a new address in the wallet for the
    * given account and chain type (change/external).
    * After deriving the address it inserts it into our
    * table of addresses.
    *
    * This method is called with the approriate params
    * from the public facing methods `getNewChangeAddress`
    * and `getNewAddress`.
    *
    * @param account Account to generate address from
    * @param chainType What chain do we generate from? Internal change vs. external
    */
  private def getNewAddressDb(
      account: AccountDb,
      chainType: HDChainType
  ): Future[AddressDb] = {
    logger.debug(s"Getting new $chainType adddress for ${account.hdAccount}")

    val lastAddrOptF = chainType match {
      case HDChainType.External =>
        addressDAO.findMostRecentExternal(account.hdAccount)
      case HDChainType.Change =>
        addressDAO.findMostRecentChange(account.hdAccount)
    }

    lastAddrOptF.map { lastAddrOpt =>
      val addrPath: HDPath = lastAddrOpt match {
        case Some(addr) =>
          val next = addr.path.next
          logger.debug(
            s"Found previous address at path=${addr.path}, next=$next")
          next
        case None =>
          val chain = account.hdAccount.toChain(chainType)
          val address = HDAddress(chain, 0)
          val path = address.toPath
          logger.debug(s"Did not find previous address, next=$path")
          path
      }

      val pathDiff =
        account.hdAccount.diff(addrPath) match {
          case Some(value) => value
          case None =>
            throw new RuntimeException(
              s"Could not diff ${account.hdAccount} and $addrPath")
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
          AddressDbHelper.getLegacyAddress(pubkey,
                                           legacyPath,
                                           networkParameters)
        case nestedPath: NestedSegWitHDPath =>
          AddressDbHelper.getNestedSegwitAddress(pubkey,
                                                 nestedPath,
                                                 networkParameters)
      }
    }
  }

  private def getNewAddressHelper(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress] = {
    val p = Promise[AddressDb]
    queue.append((account, chainType, p))
    for {
      addressDb <- p.future
    } yield {
      addressDb.address
    }
  }

  def getNextAvailableIndex(
      accountDb: AccountDb,
      chainType: HDChainType): Future[Int] = {
    getNewAddressDb(accountDb, chainType).map(_.path.path.last.index)
  }

  def getNewAddress(account: HDAccount): Future[BitcoinAddress] = {
    val accountDbOptF = findAccount(account)
    accountDbOptF.flatMap {
      case Some(accountDb) => getNewAddress(accountDb)
      case None =>
        Future.failed(
          new RuntimeException(
            s"No account found for given hdaccount=${account}"))
    }
  }

  def getNewAddress(account: AccountDb): Future[BitcoinAddress] = {
    val addrF =
      getNewAddressHelper(account, HDChainType.External)
    addrF
  }

  /** @inheritdoc */
  def getAddress(
      account: AccountDb,
      chainType: HDChainType,
      addressIndex: Int): Future[AddressDb] = {

    val coinType = account.hdAccount.coin.coinType
    val accountIndex = account.hdAccount.index

    val path = account.hdAccount.purpose match {
      case HDPurposes.Legacy =>
        LegacyHDPath(coinType, accountIndex, chainType, addressIndex)
      case HDPurposes.NestedSegWit =>
        NestedSegWitHDPath(coinType, accountIndex, chainType, addressIndex)
      case HDPurposes.SegWit =>
        SegWitHDPath(coinType, accountIndex, chainType, addressIndex)

      case invalid: HDPurpose =>
        throw new IllegalArgumentException(
          s"No HD Path type for HDPurpose of $invalid")
    }

    val pathDiff =
      account.hdAccount.diff(path) match {
        case Some(value) => value
        case None =>
          throw new IllegalArgumentException(
            s"Could not diff ${account.hdAccount} and $path")
      }

    val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
      case Failure(exception) => throw exception
      case Success(value)     => value.key
    }

    val addressDb = account.hdAccount.purpose match {
      case HDPurposes.SegWit =>
        AddressDbHelper.getSegwitAddress(
          pubkey,
          SegWitHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters)
      case HDPurposes.NestedSegWit =>
        AddressDbHelper.getNestedSegwitAddress(
          pubkey,
          NestedSegWitHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters)
      case HDPurposes.Legacy =>
        AddressDbHelper.getLegacyAddress(
          pubkey,
          LegacyHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters)

      case invalid: HDPurpose =>
        throw new IllegalArgumentException(
          s"No HD Path type for HDPurpose of $invalid")
    }

    logger.debug(s"Writing $addressDb to database")

    addressDAO.upsert(addressDb).map { written =>
      logger.debug(
        s"Got $chainType address ${written.address} at key path ${written.path} with pubkey ${written.ecPublicKey}")
      written
    }
  }

  /** @inheritdoc */
  def getUnusedAddress(addressType: AddressType): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccountForType(addressType)
      addresses <- addressDAO.getUnusedAddresses(account.hdAccount)
      address <- if (addresses.isEmpty) {
        getNewAddress(account.hdAccount)
      } else {
        Future.successful(addresses.head.address)
      }
    } yield address
  }

  /** @inheritdoc */
  def getUnusedAddress: Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccount()
      addresses <- addressDAO.getUnusedAddresses(account.hdAccount)
      address <- if (addresses.isEmpty) {
        getNewAddress(account.hdAccount)
      } else {
        Future.successful(addresses.head.address)
      }
    } yield address
  }

  def findAccount(account: HDAccount): Future[Option[AccountDb]] = {
    accountDAO.findByAccount(account)
  }

  /** @inheritdoc */
  override def getNewAddress(
      addressType: AddressType): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccountForType(addressType)
      address <- getNewAddressHelper(account, HDChainType.External)
    } yield address
  }

  /** Generates a new change address */
  override protected[wallet] def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress] = {
    getNewAddressHelper(account, HDChainType.Change)
  }

  /** @inheritdoc */
  override def getAddressInfo(
      address: BitcoinAddress): Future[Option[AddressInfo]] = {

    val addressOptF = addressDAO.findAddress(address)
    addressOptF.map { addressOpt =>
      addressOpt.map { address =>
        AddressInfo(pubkey = address.ecPublicKey,
                    network = address.address.networkParameters,
                    path = address.path)
      }
    }
  }

  /** Background thread meant to ensure safety when calling [[getNewAddress()]]
    * We to ensure independent calls to getNewAddress don't result in a race condition
    * to the database that would generate the same address and cause an error.
    * With this background thread, we poll the [[addressRequestQueue]] seeing if there
    * are any elements in it, if there are, we process them and complete the Promise in the queue. */
  lazy val walletThread = new Thread(AddressQueueRunnable)

  val addressRequestQueue =
    mutable.ArrayBuffer.empty[(AccountDb, HDChainType, Promise[BitcoinAddress])]

  /** A runnable that drains [[addressRequestQueue]]. Currently polls every 100ms
    * seeing if things are in the queue. This is needed because otherwise
    * wallet address generation is not async safe.
    * @see https://github.com/bitcoin-s/bitcoin-s/issues/1009
    * */
  private case object AddressQueueRunnable extends Runnable {
    override def run(): Unit = {
      while (true) {
        while (addressRequestQueue.nonEmpty) {
          try {
            val (account, chainType, promise) = addressRequestQueue.head
            logger.debug(
              s"Processing $account $chainType in our address request queue")
            addressRequestQueue.remove(0)

            val lastAddrOptF = chainType match {
              case HDChainType.External =>
                addressDAO.findMostRecentExternal(account.hdAccount)
              case HDChainType.Change =>
                addressDAO.findMostRecentChange(account.hdAccount)
            }

            val resultF: Future[BitcoinAddress] = lastAddrOptF.flatMap {
              lastAddrOpt =>
                val addrPath: HDPath = lastAddrOpt match {
                  case Some(addr) =>
                    val next = addr.path.next
                    logger.debug(
                      s"Found previous address at path=${addr.path}, next=$next")
                    next
                  case None =>
                    val chain = account.hdAccount.toChain(chainType)
                    val address = HDAddress(chain, 0)
                    val path = address.toPath
                    logger.debug(s"Did not find previous address, next=$path")
                    path
                }

                val addressDb = {
                  val pathDiff =
                    account.hdAccount.diff(addrPath) match {
                      case Some(value) => value
                      case None =>
                        throw new RuntimeException(
                          s"Could not diff ${account.hdAccount} and $addrPath")
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
                      AddressDbHelper.getLegacyAddress(pubkey,
                                                       legacyPath,
                                                       networkParameters)
                    case nestedPath: NestedSegWitHDPath =>
                      AddressDbHelper.getNestedSegwitAddress(pubkey,
                                                             nestedPath,
                                                             networkParameters)
                  }
                }
                logger.debug(s"Writing $addressDb to DB")
                val writeF = addressDAO.create(addressDb)
                writeF.foreach { written =>
                  logger.debug(
                    s"Got ${chainType} address ${written.address} at key path ${written.path} with pubkey ${written.ecPublicKey}")
                }

                val addrF = writeF.map { w =>
                  promise.success(w.address)
                  w.address
                }

                addrF.failed.foreach {
                  case err =>
                    logger.warn(
                      s"Failed to generate address for $account $chainType",
                      err)
                    promise.failure(err)
                }

                addrF
            }
            //make sure this is completed before we iterate to the next one
            //otherwise we will possibly have a race condition
            Await.result(resultF, 1.second)
          } catch {
            case NonFatal(exn) =>
              logger.error(s"Failed to generate address in queue", exn)
          }
        }
        //is this fair? sleep 100 milliseconds between polling for addresses
        Thread.sleep(100)
      }
    }
  }
}
