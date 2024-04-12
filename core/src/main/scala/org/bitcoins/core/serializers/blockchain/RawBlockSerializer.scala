package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import scodec.bits.ByteVector

/** Created by chris on 5/20/16.
  * Responsible for serializing blocks in our blockchain
  * https://bitcoin.org/en/developer-reference#serialized-blocks
  */
sealed abstract class RawBlockSerializer extends RawBitcoinSerializer[Block] {

  /** Takes a list of bytes and converts it into a Block */
  def read(bytes: ByteVector): Block = {
    val blockHeader: BlockHeader = BlockHeader(bytes.take(80))
    val txBytes: ByteVector = bytes.splitAt(80)._2
    val (transactions, _) = RawSerializerHelper
      .parseCmpctSizeUIntSeq[Transaction](txBytes, Transaction(_: ByteVector))
    Block(blockHeader, transactions)
  }

  /** Takes in a block and converts it to a byte array */
  def write(block: Block): ByteVector = {
    val writtenHeader = block.blockHeader.bytes
    val txBytes = RawSerializerHelper.writeCmpctSizeUInt(block.transactions,
                                                         { (tx: Transaction) =>
                                                           tx.bytes
                                                         })
    writtenHeader ++ txBytes
  }

}

object RawBlockSerializer extends RawBlockSerializer
