package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.p2p.CompactFilterHeadersMessage
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import org.bitcoins.crypto.DoubleSha256Digest
import scodec.bits.ByteVector

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfheaders BIP157]]
  */
object RawCompactFilterHeadersMessageSerializer
    extends RawBitcoinSerializer[CompactFilterHeadersMessage] {

  def read(bytes: ByteVector): CompactFilterHeadersMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))

    val (stopHashBytes, afterStopHash) = bytes.drop(1).splitAt(32)
    val stopHash = DoubleSha256Digest.fromBytes(stopHashBytes)

    val (previousFilterHeaderBytes, afterPreviousFilterHeader) =
      afterStopHash.splitAt(32)
    val previousFilterHeaderHash =
      DoubleSha256Digest.fromBytes(previousFilterHeaderBytes)

    val (hashes, _) =
      RawSerializerHelper.parseCmpctSizeUIntSeq(
        afterPreviousFilterHeader,
        { bytes =>
          DoubleSha256Digest.fromBytes(bytes.take(32))
        })

    val message = CompactFilterHeadersMessage(filterType,
                                              stopHash,
                                              previousFilterHeaderHash,
                                              hashes.toVector)

    message
  }

  def write(message: CompactFilterHeadersMessage): ByteVector = {
    val filterType = message.filterType.bytes
    val stopHash = message.stopHash.bytes
    val previousFilterHeader = message.previousFilterHeader.bytes
    val filterHashes =
      RawSerializerHelper.writeCmpctSizeUInt(message.filterHashes,
                                             { (fh: DoubleSha256Digest) =>
                                               fh.bytes
                                             })

    filterType ++ stopHash ++ previousFilterHeader ++ filterHashes
  }

}
