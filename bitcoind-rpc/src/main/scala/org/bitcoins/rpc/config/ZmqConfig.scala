package org.bitcoins.rpc.config

import org.bitcoins.commons.util.BitcoinSLogger

import java.net.InetSocketAddress

sealed trait ZmqConfig {
  def hashBlock: Option[InetSocketAddress]
  def rawBlock: Option[InetSocketAddress]
  def hashTx: Option[InetSocketAddress]
  def rawTx: Option[InetSocketAddress]
}

object ZmqConfig extends BitcoinSLogger {

  val empty: ZmqConfig = ZmqConfig()

  private case class ZmqConfigImpl(
      hashBlock: Option[InetSocketAddress],
      rawBlock: Option[InetSocketAddress],
      hashTx: Option[InetSocketAddress],
      rawTx: Option[InetSocketAddress]
  ) extends ZmqConfig

  def apply(
      hashBlock: Option[InetSocketAddress] = None,
      rawBlock: Option[InetSocketAddress] = None,
      hashTx: Option[InetSocketAddress] = None,
      rawTx: Option[InetSocketAddress] = None
  ): ZmqConfig =
    ZmqConfigImpl(
      hashBlock = hashBlock,
      rawBlock = rawBlock,
      hashTx = hashTx,
      rawTx = rawTx
    )

  /** Creates a `ZmqConfig` with all `URI` s set to `localhost` and the same
    * port
    */
  def fromPort(port: Int): ZmqConfig = {
    val uri = new InetSocketAddress("tcp://127.0.0.1", port)
    ZmqConfig(
      hashBlock = Some(uri),
      rawBlock = Some(uri),
      hashTx = Some(uri),
      rawTx = Some(uri)
    )
  }

  def fromConfig(config: BitcoindConfig): ZmqConfig =
    ZmqConfig(
      hashBlock = config.zmqpubhashblock,
      hashTx = config.zmqpubhashtx,
      rawBlock = config.zmqpubrawblock,
      rawTx = config.zmqpubrawtx
    )

}
