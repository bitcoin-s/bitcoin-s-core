package org.bitcoins.chain.blockchain.sync

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.{ExecutionContext, Future}

abstract class ChainSync extends ChainVerificationLogger {

  /** This method checks if our chain handler has the tip of the blockchain as an external source
    * If we do not have the same chain, we sync our chain handler until we are at the same best block hash
    * @param chainHandler our internal chain handler
    * @param getBlockHeaderFunc a function that we can call to retrieve a block
    * @param getBestBlockHashFunc a function that can call a third party source (bitcoind, block explorer etc)
    *                             to retrieve what the best block is on the blockchain
    * @param ec
    * @return
    */
  def sync(
      chainHandler: ChainHandler,
      getBlockHeaderFunc: DoubleSha256DigestBE => Future[BlockHeader],
      getBestBlockHashFunc: () => Future[DoubleSha256DigestBE])(implicit
      ec: ExecutionContext): Future[ChainApi] = {
    val currentTipsF: Future[Vector[BlockHeaderDb]] = {
      chainHandler.getBestChainTips()
    }

    //TODO: We are implicitly trusting whatever
    // getBestBlockHashFunc returns as the best chain
    // and we don't ever even have to have this connect
    // with our current best tips
    // do we some how want to mitigate against the divergence
    // in chains here?
    val bestBlockHashF = {
      getBestBlockHashFunc()
    }

    val updatedChainApi = bestBlockHashF.flatMap { bestBlockHash =>
      currentTipsF.flatMap { tips =>
        syncTips(chainApi = chainHandler,
                 tips = tips,
                 bestBlockHash = bestBlockHash,
                 getBlockHeaderFunc = getBlockHeaderFunc)
      }
    }

    updatedChainApi

  }

  /** Keeps walking backwards on the chain until we match one
    * of the tips we have in our chain
    * @param chainApi the chain api that represents our current chain state
    * @param tips the best block header we know about
    * @param bestBlockHash the best block header seen by our third party data source
    * @param getBlockHeaderFunc how we can retrieve block headers
    * @param ec
    * @return
    */
  private def syncTips(
      chainApi: ChainApi,
      tips: Vector[BlockHeaderDb],
      bestBlockHash: DoubleSha256DigestBE,
      getBlockHeaderFunc: DoubleSha256DigestBE => Future[BlockHeader])(implicit
      ec: ExecutionContext): Future[ChainApi] = {
    require(tips.nonEmpty, s"Cannot sync without the genesis block")
    //we need to walk backwards on the chain until we get to one of our tips
    val tipsBH = tips.map(_.blockHeader)

    def loop(
        lastHeaderF: Future[BlockHeader],
        accum: Vector[BlockHeader]): Future[Vector[BlockHeader]] = {
      lastHeaderF.flatMap { lastHeader =>
        if (tipsBH.contains(lastHeader)) {
          //means we have synced back to a block that we know
          Future.successful(accum)
        } else {

          logger.debug(s"Last header=${lastHeader.hashBE.hex}")
          //we don't know this block, so we need to keep walking backwards
          //to find a block a we know
          val newLastHeaderF =
            getBlockHeaderFunc(lastHeader.previousBlockHashBE)

          loop(newLastHeaderF, lastHeader +: accum)
        }
      }
    }

    val bestHeaderF = getBlockHeaderFunc(bestBlockHash)

    bestHeaderF.map { bestHeader =>
      logger.debug(
        s"Best tip from third party=${bestHeader.hashBE.hex} currentTips=${tips
          .map(_.hashBE.hex)}")
    }

    //one sanity check to make sure we aren't _ahead_ of our data source
    val hasBlockHashF = chainApi.getHeader(bestBlockHash)

    hasBlockHashF.flatMap { case hasBlockHashF: Option[BlockHeaderDb] =>
      if (hasBlockHashF.isDefined) {
        //if we have the best block hash in our
        //chainstate already, we don't need to search
        //for it again!
        Future.successful(chainApi)
      } else {
        //this represents all headers we have received from our external data source
        //and need to process with our chain handler
        val headersToSyncF = loop(bestHeaderF, Vector.empty)

        //now we are going to add them to our chain and return the chain api
        headersToSyncF.flatMap { headers =>
          logger.info(
            s"Attempting to sync ${headers.length} blockheader to our chainstate")
          chainApi.processHeaders(headers)
        }
      }

    }
  }
}

object ChainSync extends ChainSync
