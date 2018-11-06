package org.bitcoins.rpc.common

import java.io.File
import java.nio.file.Files

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.typesafe.config.ConfigValueFactory
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.transaction.{
  TransactionInput,
  TransactionOutPoint
}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.{ExecutionContext, Future}

class MempoolRpcTest extends AsyncFlatSpec with BeforeAndAfterAll {
  implicit val system: ActorSystem =
    ActorSystem("MempoolRpcTest", BitcoindRpcTestUtil.AKKA_CONFIG)
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val networkParam: NetworkParameters = BitcoindRpcTestUtil.network

  private val clientAccum = Vector.newBuilder[BitcoindRpcClient]

  lazy val clientsF: Future[(BitcoindRpcClient, BitcoindRpcClient)] =
    BitcoindRpcTestUtil.createNodePair(clientAccum = clientAccum)

  lazy val clientWithoutBroadcastF: Future[BitcoindRpcClient] =
    clientsF.flatMap {
      case (client, otherClient) =>
        val defaultConfig = BitcoindRpcTestUtil.standardConfig

        val datadirValue = {
          val tempDirPrefix = null // because java APIs are bad
          val tempdirPath = Files.createTempDirectory(tempDirPrefix).toString
          ConfigValueFactory.fromAnyRef(tempdirPath)
        }

        // walletbroadcast must be turned off for a transaction to be abondonable
        val noBroadcastValue = ConfigValueFactory.fromAnyRef(0)

        // connecting clients once they are started takes forever for some reason
        val configNoBroadcast =
          defaultConfig
            .withValue("walletbroadcast", noBroadcastValue)
            .withValue("datadir", datadirValue)

        val _ = BitcoindRpcTestUtil.writeConfigToFile(configNoBroadcast)

        val instanceWithoutBroadcast =
          BitcoindInstance.fromConfig(configNoBroadcast)

        val clientWithoutBroadcast =
          new BitcoindRpcClient(instanceWithoutBroadcast)
        clientAccum += clientWithoutBroadcast

        val pairs = Vector(client -> clientWithoutBroadcast,
                           otherClient -> clientWithoutBroadcast)

        for {
          _ <- clientWithoutBroadcast.start()
          _ <- BitcoindRpcTestUtil.connectPairs(pairs)
          _ <- BitcoindRpcTestUtil.syncPairs(pairs)
          _ <- BitcoindRpcTestUtil.generateAndSync(
            Vector(clientWithoutBroadcast, client, otherClient),
            blocks = 200)
        } yield clientWithoutBroadcast
    }

  override protected def afterAll(): Unit = {
    BitcoindRpcTestUtil.stopServers(clientAccum.result)
    TestKit.shutdownActorSystem(system)
  }

  behavior of "MempoolRpc"

  it should "be able to find a transaction sent to the mem pool" in {
    for {
      (client, otherClient) <- clientsF
      transaction <- BitcoindRpcTestUtil.sendCoinbaseTransaction(client,
                                                                 otherClient)
      mempool <- client.getRawMemPool
    } yield {
      assert(mempool.length == 1)
      assert(mempool.head == transaction.txid)
    }
  }

  it should "be able to find a verbose transaction in the mem pool" in {
    for {
      (client, otherClient) <- clientsF
      transaction <- BitcoindRpcTestUtil.sendCoinbaseTransaction(client,
                                                                 otherClient)
      mempool <- client.getRawMemPoolWithTransactions
    } yield {
      val txid = mempool.keySet.head
      assert(txid == transaction.txid)
      assert(mempool(txid).size > 0)
    }
  }

  it should "be able to find a mem pool entry" in {
    for {
      (client, otherClient) <- clientsF
      transaction <- BitcoindRpcTestUtil.sendCoinbaseTransaction(client,
                                                                 otherClient)
      _ <- client.getMemPoolEntry(transaction.txid)
    } yield succeed
  }

  it should "be able to get mem pool info" in {
    for {
      (client, otherClient) <- clientsF
      _ <- client.generate(1)
      info <- client.getMemPoolInfo
      _ <- BitcoindRpcTestUtil
        .sendCoinbaseTransaction(client, otherClient)
      newInfo <- client.getMemPoolInfo
    } yield {
      assert(info.size == 0)
      assert(newInfo.size == 1)
    }
  }

  it should "be able to prioritise a mem pool transaction" in {
    for {
      (client, otherClient) <- clientsF
      address <- otherClient.getNewAddress
      txid <- BitcoindRpcTestUtil
        .fundMemPoolTransaction(client, address, Bitcoins(3.2))
      entry <- client.getMemPoolEntry(txid)
      tt <- client.prioritiseTransaction(txid, Bitcoins(1).satoshis)
      newEntry <- client.getMemPoolEntry(txid)
    } yield {
      assert(entry.fee == entry.modifiedfee)
      assert(tt)
      assert(newEntry.fee == entry.fee)
      assert(newEntry.modifiedfee == newEntry.fee + Bitcoins(1))
    }
  }

  it should "be able to find mem pool ancestors and descendants" in {
    for {
      (client, _) <- clientsF
      _ <- client.generate(1)
      address1 <- client.getNewAddress
      txid1 <- BitcoindRpcTestUtil.fundMemPoolTransaction(client,
                                                          address1,
                                                          Bitcoins(2))
      mempool <- client.getRawMemPool
      address2 <- client.getNewAddress

      createdTx <- {
        val input: TransactionInput =
          TransactionInput(TransactionOutPoint(txid1.flip, UInt32.zero),
                           ScriptSignature.empty,
                           UInt32.max - UInt32.one)
        client
          .createRawTransaction(Vector(input), Map(address2 -> Bitcoins.one))
      }
      signedTx <- BitcoindRpcTestUtil.signRawTransaction(client, createdTx)
      txid2 <- client.sendRawTransaction(signedTx.hex, allowHighFees = true)

      descendantsTxid1 <- client.getMemPoolDescendants(txid1)
      verboseDescendantsTxid1 <- client.getMemPoolDescendantsVerbose(txid1)
      _ = {
        assert(descendantsTxid1.head == txid2)
        val (txid, mempoolreults) = verboseDescendantsTxid1.head
        assert(txid == txid2)
        assert(mempoolreults.ancestorcount == 2)
      }

      ancestorsTxid2 <- client.getMemPoolAncestors(txid2)
      verboseAncestorsTxid2 <- client.getMemPoolAncestorsVerbose(txid2)
      _ = {
        assert(ancestorsTxid2.head == txid1)
        val (txid, mempoolreults) = verboseAncestorsTxid2.head
        assert(txid == txid1)
        assert(mempoolreults.descendantcount == 2)
      }

    } yield {
      assert(mempool.head == txid1)
      assert(signedTx.complete)
    }
  }

  it should "be able to abandon a transaction" in {
    for {
      (_, otherClient) <- clientsF
      clientWithoutBroadcast <- clientWithoutBroadcastF
      recipient <- otherClient.getNewAddress
      txid <- clientWithoutBroadcast.sendToAddress(recipient, Bitcoins(1))
      _ <- clientWithoutBroadcast.abandonTransaction(txid)
      maybeAbandoned <- clientWithoutBroadcast.getTransaction(txid)
    } yield assert(maybeAbandoned.details.head.abandoned.contains(true))
  }

  it should "be able to save the mem pool to disk" in {
    for {
      (client, _) <- clientsF
      regTest = {
        val regTest =
          new File(client.getDaemon.authCredentials.datadir + "/regtest")
        assert(regTest.isDirectory)
        assert(!regTest.list().contains("mempool.dat"))
        regTest
      }
      _ <- client.saveMemPool()
    } yield assert(regTest.list().contains("mempool.dat"))
  }
}
