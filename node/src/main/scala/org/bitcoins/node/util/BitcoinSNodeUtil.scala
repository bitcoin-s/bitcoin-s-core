package org.bitcoins.node.util

import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig

import scala.util.Random

object BitcoinSNodeUtil {

  /** Creates a unique actor name for a actor
    * @param className
    * @return
    */
  def createActorName(className: String): String = {
    s"$className-${System.currentTimeMillis()}-${Random.nextLong()}"
  }

  /** Creates a unique actor name for a given class
    * @param className
    * @return
    */
  def createActorName(className: Class[?]): String =
    createActorName(className.getSimpleName)

  def stringsToPeers(
      addresses: Vector[String]
  )(implicit nodeAppConfig: NodeAppConfig): Vector[Peer] = {
    val formatStrings = addresses.map { s =>
      // assumes strings are valid, todo: add util functions to check fully for different addresses
      val stripped = if (s.contains('#')) {
        // for cases like 83.150.2.128:8333 # AS8758
        s.split('#').head.trim
      } else {
        s
      }
      if (stripped.count(_ == ':') > 1 && stripped(0) != '[') // ipv6
        "[" + stripped + "]"
      else stripped
    }
    val inetSockets = formatStrings.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port)
    )
    val peers =
      inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
    peers
  }
}
