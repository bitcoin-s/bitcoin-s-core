package org.bitcoins.rpc.client.v19

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.{BlockFilter, FilterType}
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.jsonmodels.GetBlockFilterResult
import org.bitcoins.rpc.serializers.JsonReaders.DoubleSha256DigestBEReads
import play.api.libs.json._

import scala.concurrent.Future

/**
  * Gets the BIP158 filter for the specified block.
  * This RPC is only enabled if block filters have been created using the -blockfilterindex configuration option
  * @see [[https://bitcoincore.org/en/doc/0.19.0/rpc/blockchain/getblockfilter]]
  */
trait V19BlockFilterRpc {
  self: Client =>

  private case class TempBlockFilterResult(
      filter: String,
      header: DoubleSha256DigestBE)
  implicit private val tempBlockFilterResultReads: Reads[
    TempBlockFilterResult] = Json.reads[TempBlockFilterResult]

  def getBlockFilter(
      blockhash: DoubleSha256DigestBE,
      filtertype: FilterType): Future[GetBlockFilterResult] = {
    bitcoindCall[TempBlockFilterResult](
      "getblockfilter",
      List(JsString(blockhash.hex), JsString(filtertype.toString.toLowerCase)))
      .map { tempBlockFilterResult =>
        GetBlockFilterResult(
          BlockFilter.fromHex(tempBlockFilterResult.filter, blockhash.flip),
          tempBlockFilterResult.header)
      }
  }

}
