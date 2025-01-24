---
id: version-1.9.10-node-api title: Node API
title: node-api title: Node API
original_id: node-api title: Node API
---


### NodeAPI

The NodeApi is how the wallet project retrieves relevant node data like blocks. This allows the wallet for example to
retrieve blocks for finding its relevant transactions.

Since this is an API it can be hooked up to the `node` module of bitcoin-s but it can also be linked to any other
implementation of your choosing. This allows you to use the bitcoin-s wallet in any schema that you want.

The functions that the NodeApi supports are:

```scala
trait NodeApi {

  /** Request the underlying node to download the given blocks from its peers and feed the blocks to [[org.bitcoins.node.NodeCallbacks]] */
    def downloadBlocks(blockHashes: Vector[DoubleSha256DigestBE]): Future[Unit]
}
```

## Downloading blocks with bitcoind

As an example, we will show you how to use the `NodeApi` and bitcoind to download blocks for a wallet.

```scala
implicit val system: ActorSystem = ActorSystem(s"node-api-example")
implicit val ec: ExecutionContextExecutor = system.dispatcher
implicit val walletConf: WalletAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig().walletConf

// let's use a helper method to get a v19 bitcoind
// and a ChainApi
val instance = BitcoindInstanceLocal.fromConfigFile(BitcoindConfig.DEFAULT_CONF_FILE)
val bitcoind = BitcoindRpcClient(instance)
val chainApi = MockChainQueryApi.mock
val aesPasswordOpt = Some(AesPassword.fromString("password"))

// This function can be used to create a callback for when our node api calls downloadBlocks,
// more specifically it will call the function every time we receive a block, the returned
// NodeCallbacks will contain the necessary items to initialize the callbacks
def createCallback(processBlock: Block => Future[Unit]): NodeCallbacks = {
    lazy val onBlock: OnBlockReceived = { block =>
      processBlock(block)
    }
    NodeCallbacks(onBlockReceived = Vector(onBlock))
  }

// Here is a super simple example of a callback, this could be replaced with anything, from
// relaying the block on the network, finding relevant wallet transactions, verifying the block,
// or writing it to disk
val exampleProcessBlock = (block: Block) =>
    Future.successful(println(s"Received block: ${block.blockHeader.hashBE}"))
val exampleCallback = createCallback(exampleProcessBlock)

// Here is where we are defining our actual node api, Ideally this could be it's own class
// but for the examples sake we will keep it small.
  val nodeApi = new NodeApi {

    override def broadcastTransactions(transactions: Vector[Transaction]): Future[Unit] = {
            FutureUtil.sequentially(transactions)(bitcoind.sendRawTransaction(_)).map(_ => ())
    }

    override def downloadBlocks(
        blockHashes: Vector[DoubleSha256DigestBE]): Future[Unit] = {
      val blockFs = blockHashes.map(hash => bitcoind.getBlockRaw(hash))
      Future.sequence(blockFs).map(_ => ())
    }
  
    override def getConnectionCount: Future[Int] = bitcoind.getConnectionCount
  }

// Finally, we can initialize our wallet with our own node api
val wallet =
    Wallet(nodeApi = nodeApi, chainQueryApi = chainApi)

// Then to trigger the event we can run
val exampleBlock = DoubleSha256DigestBE(
    "000000000010dc23dc0d5acad64667a7a2b3010b6e02da4868bf392c90b6431d")
wallet.nodeApi.downloadBlocks(Vector(exampleBlock))
```
