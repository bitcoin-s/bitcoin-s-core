---
title: Blockchain Verification
id: version-1.9.10-chain
original_id: chain
---

Bitcoin-S comes bundled with a rudimentary blockchain verification
module. This module is currently only released as a library, and not as a binary.
This is because it (nor the documentation) is not deemed production
ready. Use at your own risk, and without too much money depending on it.

## Syncing and verifying block headers

Using the `chain` module of Bitcoin-S it's possible to
sync and verify block headers from the Bitcoin blockchain. In this document
we demonstrate how to do this, while persisting it to disk. We should be
able to read this chain on subsequent runs, assuming we are connected
to the same `bitcoind` instance.


```scala
implicit val ec: ExecutionContext = ExecutionContext.global
implicit val system: ActorSystem = ActorSystem("System")
// We are assuming that a `bitcoind` regtest node is running in the background.
// You can see our `bitcoind` guides to see how to connect
// to a local or remote `bitcoind` node.

val bitcoindInstance = BitcoindInstanceLocal.fromDatadir()
val rpcCli = BitcoindRpcClient(bitcoindInstance)

// Next, we need to create a way to monitor the chain:

val getBestBlockHash = SyncUtil.getBestBlockHashFunc(rpcCli)

val getBlockHeader = SyncUtil.getBlockHeaderFunc(rpcCli)

// set a data directory
val datadir = Files.createTempDirectory("bitcoin-s-test")

// set the current network to regtest
import com.typesafe.config.ConfigFactory
val config = ConfigFactory.parseString {
    """
    | bitcoin-s {
    |   network = regtest
    | }
    |""".stripMargin
}

implicit val chainConfig: ChainAppConfig = ChainAppConfig(datadir, Vector(config))

// Initialize the needed database tables if they don't exist:
val chainProjectInitF = chainConfig.start()
val blockHeaderDAO = BlockHeaderDAO()
val compactFilterHeaderDAO = CompactFilterHeaderDAO()
val compactFilterDAO = CompactFilterDAO()
val stateDAO = ChainStateDescriptorDAO()


//initialize the chain handler from the database
val chainHandler = ChainHandler.fromDatabase(blockHeaderDAO, compactFilterHeaderDAO, compactFilterDAO, stateDAO)

// Now, do the actual syncing:
val syncedChainApiF = for {
    _ <- chainProjectInitF
    synced <- ChainSync.sync(chainHandler, getBlockHeader, getBestBlockHash)
} yield synced

val syncResultF = syncedChainApiF.flatMap { chainApi =>
  chainApi.getBlockCount().map(count => println(s"chain api blockcount=${count}"))

  rpcCli.getBlockCount().map(count => println(s"bitcoind blockcount=${count}"))
}

syncResultF.onComplete { case result =>
  println(s"Sync result=${result}")
}
```
