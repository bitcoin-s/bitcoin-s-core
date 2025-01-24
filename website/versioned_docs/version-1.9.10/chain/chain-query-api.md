---
id: version-1.9.10-chain-query-api
title: Chain Query API
original_id: chain-query-api
---


### ChainQueryAPI

The ChainQueryApi is how the wallet project stays aware of the current best chain.
This allows the wallet for example to calculate the number of confirmations for a transaction,
get the current chain tip, or even retrieve block filters for a given set of blocks.

Since this is an API it can be hooked up to the `chain` module of bitcoin-s but it can also be linked to
any other implementation of your choosing. This allows you to use the bitcoin-s wallet in any schema that you
want.

The functions that the ChainQueryApi supports are:

```scala
trait ChainQueryApi {

  /** Gets the height of the given block */
  def getBlockHeight(blockHash: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the hash of the block that is what we consider "best" */
  def getBestBlockHash(): Future[DoubleSha256DigestBE]

  /** Gets number of confirmations for the given block hash*/
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the number of compact filters in the database */
  def getFilterCount: Future[Int]

  /** Returns the block height of the given block stamp */
  def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int]

  def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]]
      
  def getMedianTimePast(): Future[Long]
}
```

## Chain query with bitcoind

As an example, we will show you how to use the `ChainQueryApi` and bitcoind to query chain data.

```scala
implicit val system: ActorSystem = ActorSystem(s"node-api-example")
implicit val ec: ExecutionContextExecutor = system.dispatcher
implicit val walletConf: WalletAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig().walletConf

val instance = BitcoindInstanceLocal.fromConfigFile(BitcoindConfig.DEFAULT_CONF_FILE)
val bitcoind = BitcoindRpcClient(instance)
val nodeApi = MockNodeApi.mock

// This function can be used to create a callback for when our chain api receives a transaction, block, or
// a block filter, the returned NodeCallbacks will contain the necessary items to initialize the callbacks
def createCallbacks(
      processTransaction: Transaction => Future[Unit],
      processCompactFilters: (Vector[(DoubleSha256DigestBE, GolombFilter)]) => Future[Unit],
      processBlock: Block => Future[Unit]): NodeCallbacks = {
    lazy val onTx: OnTxReceived = { tx =>
      processTransaction(tx)
    }
    lazy val onCompactFilters: OnCompactFiltersReceived = {
      blockFilters =>
        processCompactFilters(blockFilters)
    }
    lazy val onBlock: OnBlockReceived = { block =>
      processBlock(block)
    }
    NodeCallbacks(onTxReceived = Vector(onTx),
                  onBlockReceived = Vector(onBlock),
                  onCompactFiltersReceived = Vector(onCompactFilters))
  }

// Here is a super simple example of a callback, this could be replaced with anything, from
// relaying the block on the network, finding relevant wallet transactions, verifying the block,
// or writing it to disk
val exampleProcessTx = (tx: Transaction) =>
    Future.successful(println(s"Received tx: ${tx.txIdBE}"))

val exampleProcessBlock = (block: Block) =>
    Future.successful(println(s"Received block: ${block.blockHeader.hashBE}"))

val exampleProcessFilters =
    (filters: Vector[(DoubleSha256DigestBE, GolombFilter)]) =>
      Future.successful(println(s"Received filter: ${filters.head._1.flip.hex} ${filters.head._2.hash.flip.hex}"))

val exampleCallbacks =
    createCallbacks(exampleProcessTx, exampleProcessFilters, exampleProcessBlock)

// Here is where we are defining our actual chain api, Ideally this could be it's own class
// but for the examples sake we will keep it small.
val chainApi = new ChainQueryApi {

    override def epochSecondToBlockHeight(time: Long): Future[Int] =
        Future.successful(0)

    /** Gets the height of the given block */
    override def getBlockHeight(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
      bitcoind.getBlock(blockHash).map(block => Some(block.height))
    }

    /** Gets the hash of the block that is what we consider "best" */
    override def getBestBlockHash(): Future[DoubleSha256DigestBE] = {
      bitcoind.getBestBlockHash()
    }

    /** Gets number of confirmations for the given block hash */
    override def getNumberOfConfirmations(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
      for {
        tip <- bitcoind.getBlockCount()
        block <- bitcoind.getBlock(blockHash)
      } yield {
        Some(tip - block.height + 1)
      }
    }

    /** Gets the number of compact filters in the database */
    override def getFilterCount(): Future[Int] = {
      // since bitcoind should have the filter for
      // every block we can just return the block height
      bitcoind.getBlockCount()
    }

    /** Returns the block height of the given block stamp */
    override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] = {
      blockStamp match {
        case blockHeight: BlockStamp.BlockHeight =>
          Future.successful(blockHeight.height)
        case blockHash: BlockStamp.BlockHash =>
          getBlockHeight(blockHash.hash).map(_.get)
        case blockTime: BlockStamp.BlockTime =>
          Future.failed(new RuntimeException(s"Not implemented: $blockTime"))
      }
    }

    override def getFiltersBetweenHeights(
        startHeight: Int,
        endHeight: Int): Future[Vector[FilterResponse]] = {
      val filterFs = startHeight
        .until(endHeight)
        .map { height =>
          for {
            hash <- bitcoind.getBlockHash(height)
            filter <- bitcoind.getBlockFilter(hash, FilterType.Basic)
          } yield {
            FilterResponse(filter.filter, hash, height)
          }
        }
        .toVector

      Future.sequence(filterFs)
    }
    
    override def getMedianTimePast(): Future[Long] = bitcoind.getMedianTimePast()
  }

// Finally, we can initialize our wallet with our own node api
val wallet =
    Wallet(nodeApi = nodeApi, chainQueryApi = chainApi)

// Then to trigger one of the events we can run
wallet.chainQueryApi.getFiltersBetweenHeights(100, 150)
```
