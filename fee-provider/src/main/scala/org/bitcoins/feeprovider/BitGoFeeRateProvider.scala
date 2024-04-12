package org.bitcoins.feeprovider

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.Uri
import org.bitcoins.commons.jsonmodels.wallet.BitGoResult
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.api.tor.Socks5ProxyParams
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import play.api.libs.json.{JsError, JsSuccess, Json}
import org.bitcoins.core.satoshisPerKiloByteOrdering

import scala.util.{Failure, Success, Try}

/** Fetches fee rate from BitGo's API
  * @see [[https://www.bitgo.com/api/v2/#operation/v2.tx.getfeeestimate]]
  */
case class BitGoFeeRateProvider(
    blockTargetOpt: Option[Int],
    proxyParams: Option[Socks5ProxyParams])(implicit
    override val system: ActorSystem)
    extends CachedHttpFeeRateProvider[SatoshisPerKiloByte] {

  override val uri: Uri = Uri("https://www.bitgo.com/api/v2/btc/tx/fee")

  override def converter(str: String): Try[SatoshisPerKiloByte] = {
    val json = Json.parse(str)
    json.validate[BitGoResult] match {
      case JsSuccess(response, _) =>
        blockTargetOpt match {
          case None =>
            Success(response.feePerKb)
          case Some(blockTarget) =>
            val feeRate = extractFeerate(response.feeByBlockTarget, blockTarget)
            Success(feeRate)
        }
      case JsError(error) =>
        Failure(
          new RuntimeException(
            s"Unexpected error when parsing response $str: $error"))
    }
  }

  private def extractFeerate(
      feeRanges: Map[Int, SatoshisPerKiloByte],
      blockTarget: Int): SatoshisPerKiloByte = {
    // first we keep only fee ranges with a max block delay below the limit
    val belowLimit = feeRanges.filter(_._1 <= blockTarget)
    // out of all the remaining fee ranges, we select the one with the minimum higher bound
    belowLimit.values.min
  }
}

object BitGoFeeRateProvider extends FeeProviderFactory[BitGoFeeRateProvider] {

  override def fromBlockTarget(
      blocks: Int,
      proxyParams: Option[Socks5ProxyParams])(implicit
      system: ActorSystem): BitGoFeeRateProvider = {
    BitGoFeeRateProvider(Some(blocks), proxyParams)
  }
}
