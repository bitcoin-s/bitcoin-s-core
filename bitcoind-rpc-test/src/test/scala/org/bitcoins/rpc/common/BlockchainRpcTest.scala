package org.bitcoins.rpc.common

import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockChainInfoResultPostV23}
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.AddressType
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.testkit.rpc.{
  BitcoindFixturesCachedPairNewest,
  BitcoindRpcTestUtil
}

class BlockchainRpcTest extends BitcoindFixturesCachedPairNewest {

  behavior of "BlockchainRpc"

  it should "be able to get the first block" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
    } yield {
      assert(block.tx.nonEmpty)
      assert(block.height == 1)
    }
  }

  it should "be able to get blockchain info" in { nodePair =>
    val client = nodePair.node1
    for {
      info <- client.getBlockChainInfo
      bestHash <- client.getBestBlockHash
    } yield {
      assert(info.isInstanceOf[GetBlockChainInfoResultPostV23])
      val postV23 = info.asInstanceOf[GetBlockChainInfoResultPostV23]
      assert(postV23.chain == RegTest)
      assert(postV23.bestblockhash == bestHash)
    }
  }

  it should "be able to invalidate a block" in { nodePair =>
    val client = nodePair.node1
    val otherClient = nodePair.node2
    for {
      address <- otherClient.getNewAddress(addressType = AddressType.P2SHSegwit)
      txid <-
        BitcoindRpcTestUtil
          .fundMemPoolTransaction(client, address, Bitcoins(1))
      blocks <- client.generate(1)
      mostRecentBlock <- client.getBlock(blocks.head)
      _ <- client.invalidateBlock(blocks.head)
      mempool <- client.getRawMemPool
      count1 <- client.getBlockCount
      count2 <- otherClient.getBlockCount

      _ <- client.generate(
        2
      ) // Ensure client and otherClient have the same blockchain
    } yield {
      assert(mostRecentBlock.tx.contains(txid))
      assert(mempool.contains(txid))
      assert(count1 == count2 - 1)
    }
  }

  it should "be able to get block hash by height" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(2)
      count <- client.getBlockCount
      hash <- client.getBlockHash(count)
      prevhash <- client.getBlockHash(count - 1)
    } yield {
      assert(blocks(1) == hash)
      assert(blocks(0) == prevhash)
    }
  }

  it should "be able to get tx out proof and verify it" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      merkle <- client.getTxOutProof(Vector(block.tx.head.txid))
      txids <- client.verifyTxOutProof(merkle)
    } yield {
      assert(merkle.transactionCount == UInt32(1))
      assert(merkle.hashes.length == 1)
      assert(merkle.hashes.head.flip == block.tx.head.txid)
      assert(block.tx.head.txid == txids.head)
    }
  }

  it should "be able to rescan the blockchain" in { nodePair =>
    val client = nodePair.node1
    for {
      result <- client.rescanBlockChain()
      count <- client.getBlockCount
    } yield {
      assert(result.start_height == 0)
      assert(count == result.stop_height)
    }
  }

  it should "be able to get the chain tx stats" in { nodePair =>
    val client = nodePair.node1
    for {
      stats <- client.getChainTxStats
    } yield {
      assert(stats.txcount > 0)
      assert(stats.window_block_count > 0)
    }
  }

  it should "be able to get a raw block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(1)
      block <- client.getBlockRaw(blocks.head)
      blockHeader <- client.getBlockHeaderRaw(blocks.head)
    } yield assert(block.blockHeader == blockHeader)
  }

  it should "be able to get a block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(1)
      block <- client.getBlock(blocks.head)
    } yield {
      assert(block.hash == blocks(0))
      assert(block.confirmations == 1)
      assert(block.size > 0)
      assert(block.weight > 0)
      assert(block.height > 0)
      assert(block.difficulty > 0)
    }
  }

  it should "be able to get a transaction" in { nodePair =>
    val client = nodePair.node1
    for {
      block <- BitcoindRpcTestUtil.getFirstBlock(client)
      tx <- client.getTransaction(block.tx.head.txid)
      count <- client.getBlockCount
    } yield {
      assert(tx.txid == block.tx.head.txid)
      assert(tx.amount == Bitcoins(50))
      assert(tx.blockindex.get == 0)
      assert(tx.details.head.category == "generate")
      assert(tx.generated.get)
      assert(tx.confirmations == count)
    }
  }

  it should "be able to get a block with verbose transactions" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(2)
      block <- client.getBlockWithTransactions(blocks(1))
    } yield {
      assert(block.hash == blocks(1))
      assert(block.tx.length == 1)
      val tx = block.tx.head
      assert(tx.vout.head.n == 0)
    }
  }

  it should "be able to get the chain tips" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.getChainTips
    } yield succeed
  }

  it should "be able to get the best block hash" in { nodePair =>
    val client = nodePair.node1
    for {
      _ <- client.getBestBlockHash
    } yield succeed
  }

  it should "be able to list all blocks since a given block" in { nodePair =>
    val client = nodePair.node1
    for {
      blocks <- client.generate(3)
      list <- client.listSinceBlock(blocks(0))
    } yield {
      assert(list.transactions.length >= 2)
      assert(list.transactions.exists(_.blockhash.contains(blocks(1))))
      assert(list.transactions.exists(_.blockhash.contains(blocks(2))))
    }
  }

  it should "be able to verify the chain" in { nodePair =>
    val client = nodePair.node1
    for {
      valid <- client.verifyChain(blocks = 0)
    } yield assert(valid)
  }

  it should "be able to get the tx outset info" in { nodePair =>
    val client = nodePair.node1
    for {
      info <- client.getTxOutSetInfo
      count <- client.getBlockCount
      hash <- client.getBestBlockHash
    } yield {
      assert(info.height == count)
      assert(info.bestblock == hash)
    }
  }

  it should "calculate median time past" in { nodePair =>
    val client = nodePair.node1
    for {
      medianTime <- client.getMedianTimePast()
    } yield {
      val oneHourAgo = (System.currentTimeMillis() / 1000) - 60 * 60
      assert(medianTime > oneHourAgo)
    }
  }
}