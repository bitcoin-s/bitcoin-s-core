package org.bitcoins.rpc.v19
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.client.common.RpcOpts.WalletFlag
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.concurrent.Future

class BitcoindV19RpcClientTest extends BitcoindRpcTest {
  lazy val clientF: Future[BitcoindV19RpcClient] = {
    val client = new BitcoindV19RpcClient(BitcoindRpcTestUtil.v19Instance())
    val clientIsStartedF = BitcoindRpcTestUtil.startServers(Vector(client))
    clientIsStartedF.map(_ => client)
  }
  lazy val clientPairF: Future[(BitcoindV19RpcClient, BitcoindV19RpcClient)] =
    BitcoindRpcTestUtil.createNodePairV19(clientAccum)

  clientF.foreach(c => clientAccum.+=(c))

  behavior of "BitcoindV19RpcClient"

  it should "be able to start a V19 bitcoind instance" in {

    clientF.map { client =>
      assert(client.version == BitcoindVersion.V19)
    }

  }

  it should "get a block filter given a block hash" in {
    for {
      (client, _) <- clientPairF
      blocks <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      blockFilter <- client.getBlockFilter(blocks.head, FilterType.Basic)

      block <- client.getBlockRaw(blocks.head)
      txs <- Future.sequence(
        block.transactions
          .filter(!_.isCoinbase)
          .map(x => client.getTransaction(x.txIdBE)))

      prevFilter <- client.getBlockFilter(block.blockHeader.previousBlockHashBE, FilterType.Basic)
    } yield {
      val pubKeys = txs.flatMap(_.hex.outputs.map(_.scriptPubKey)).toVector
      assert(BlockFilter(block, pubKeys).hash == blockFilter.filter.hash)
      assert(blockFilter.header == BlockFilter(block, pubKeys).getHeader(prevFilter.header.flip).hash.flip)
    }
  }

  it should "be able to get the balances" in {
    for {
      (client, _) <- clientPairF
      immatureBalance <- client.getBalances
      _ <- client.getNewAddress.flatMap(client.generateToAddress(1, _))
      newImmatureBalance <- client.getBalances
    } yield {
      assert(immatureBalance.mine.immature.toBigDecimal >= 0)
      assert(
        immatureBalance.mine.immature.toBigDecimal + 12.5 == newImmatureBalance.mine.immature.toBigDecimal)
    }
  }

  it should "be able to set the wallet flag 'avoid_reuse'" in {
    for {
      (client, _) <- clientPairF
      result <- client.setWalletFlag(WalletFlag.AvoidReuse, value = true)
    } yield {
      assert(result.flag_name == "avoid_reuse")
      assert(result.flag_state)
      // TODO: Add test cases for when updated RPCs are added
    }
  }
}
