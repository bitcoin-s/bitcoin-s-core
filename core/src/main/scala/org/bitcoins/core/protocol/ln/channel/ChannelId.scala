package org.bitcoins.core.protocol.ln.channel

import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/** There are two types of ChannelIds in the lightning protocol There is a
  * 'temporary' channel id used for the hand shake when initially establishing a
  * channel and then a FundedChannelId indicating a channel that has a validly
  * signed tx For more information on the distinction between these two types
  * please read about
  * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#channel-establishment establishing a channel]]
  */
sealed abstract class ChannelId extends NetworkElement

/** Represents the temporary channelId created in the `open_channel` msg of
  * the
  * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#the-open_channel-message LN p2p protocol]]
  */
case class TempChannelId(bytes: ByteVector) extends ChannelId {
  require(bytes.length == 32,
          s"ChannelId must be 32 bytes in size, got ${bytes.length}")
}

/** Represents the stable ChannelId that represents a channel that has been
  * signed by both parties This is created in the `funding_signed` msg on the
  * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#the-funding_signed-message LN p2p protocol]].
  *
  * This channelId is derived It's derived from the funding transaction by
  * combining the `funding_txid` and the `funding_output_index` using big-endian
  * exclusive-OR (i.e. `funding_output_index` alters the last 2 bytes).
  */
case class FundedChannelId(bytes: ByteVector) extends ChannelId {
  require(bytes.length == 32,
          s"ChannelId must be 32 bytes in size, got ${bytes.length}")
}

object FundedChannelId extends Factory[FundedChannelId] {

  override def fromBytes(bytes: ByteVector): FundedChannelId = {
    new FundedChannelId(bytes)
  }
}
