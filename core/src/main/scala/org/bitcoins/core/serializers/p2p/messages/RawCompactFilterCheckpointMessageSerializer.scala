package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.p2p.CompactFilterCheckPointMessage
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import org.bitcoins.crypto.DoubleSha256Digest
import scodec.bits.ByteVector

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfcheckpt BIP-157 ]]
  */
object RawCompactFilterCheckpointMessageSerializer
    extends RawBitcoinSerializer[CompactFilterCheckPointMessage] {

  def read(bytes: ByteVector): CompactFilterCheckPointMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))

    val (stopHashBytes, afterStopHash) = bytes.drop(1).splitAt(32)
    val stopHash = DoubleSha256Digest.fromBytes(stopHashBytes)

    val filterHeadersLength = CompactSizeUInt.parse(afterStopHash)

    val (headers, _) =
      RawSerializerHelper.parseCmpctSizeUIntSeq(
        afterStopHash,
        { bytes =>
          DoubleSha256Digest.fromBytes(bytes.take(32))
        })

    require(
      headers.length == filterHeadersLength.toInt,
      s"Invalid compact filter checkpoint message: expected number of headers ${filterHeadersLength.toInt}, but got ${headers.length}"
    )

    CompactFilterCheckPointMessage(filterType, stopHash, headers.toVector)
  }

  def write(message: CompactFilterCheckPointMessage): ByteVector = {
    val filterType = message.filterType.bytes
    val stopHash = message.stopHash.bytes
    val filterHeaders =
      RawSerializerHelper.writeCmpctSizeUInt(message.filterHeaders,
                                             { (fh: DoubleSha256Digest) =>
                                               fh.bytes
                                             })
    filterType ++ stopHash ++ filterHeaders
  }
}
