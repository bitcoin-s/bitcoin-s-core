package org.bitcoins.core.p2p

import org.bitcoins.core.bloom.{BloomFilter, BloomFlag}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.gcs.{FilterHeader, FilterType, GolombFilter}
import org.bitcoins.core.number.{Int32, Int64, UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.p2p.messages._
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerKiloByte}
import org.bitcoins.crypto._

import scodec.bits.ByteVector

/** Trait that represents a payload for a message on the Bitcoin p2p network
  * @see [[https://bitcoin.org/en/developer-reference#p2p-network]]
  */
sealed trait NetworkPayload extends NetworkElement {

  /** ASCII string which identifies what message type is contained in the payload.
    * Followed by nulls (0x00) to pad out byte count; for example: version\0\0\0\0\0.
    * Command names need to be 12 bytes long
    * This is generally used to build a [[org.bitcoins.core.p2p.NetworkHeader]]
    */
  def commandName: String
}

/** Represents a data message inside of bitcoin core
  *
  * @see [[https://bitcoin.org/en/developer-reference#data-messages]]
  */
sealed trait DataPayload extends NetworkPayload

/** The block message transmits a single serialized block
  *
  * @param block The block being transmitted inside of this message
  *
  * @see [[https://bitcoin.org/en/developer-reference#block]]
  */
case class BlockMessage(block: Block) extends DataPayload {
  override val commandName = NetworkPayload.blockCommandName

  override def bytes: ByteVector = RawBlockMessageSerializer.write(this)
}

object BlockMessage extends Factory[BlockMessage] {

  def fromBytes(bytes: ByteVector): BlockMessage =
    RawBlockMessageSerializer.read(bytes)

}

/** The `getblocks` message requests an inv message that provides block header hashes
  * starting from a particular point in the block chain.
  * It allows a peer which has been disconnected or started for the first time to get the data
  * it needs to request the blocks it hasn’t seen.
  * @see  [https://bitcoin.org/en/developer-reference#getblocks]]
  */
trait GetBlocksMessage extends DataPayload with ExpectsResponse {

  /** The protocol version number; the same as sent in the version message.
    */
  def protocolVersion: ProtocolVersion

  /** The number of header hashes provided not including the stop hash.
    * There is no limit except that the byte size of the entire message
    * must be below the MAX_SIZE limit; typically from 1 to 200 hashes are sent.
    */
  def hashCount: CompactSizeUInt

  /** One or more block header hashes (32 bytes each) in internal byte order.
    * Hashes should be provided in reverse order of block height,
    * so highest-height hashes are listed first and lowest-height hashes are listed last.
    */
  def blockHeaderHashes: Seq[DoubleSha256Digest]

  /** The header hash of the last header hash being requested;
    * set to all zeroes to request an inv message with all subsequent
    * header hashes (a maximum of 500 will be sent as a reply to this message;
    * if you need more than 500, you will need to send another getblocks message
    * with a higher-height header hash as the first entry in block header hash field).
    */
  def stopHash: DoubleSha256Digest

  override def commandName = NetworkPayload.getBlocksCommandName

  override def bytes: ByteVector = RawGetBlocksMessageSerializer.write(this)
}

/** This is the companion object for the GetBlocks network message on the p2p network
  * @see https://bitcoin.org/en/developer-reference#getblocks
  */
object GetBlocksMessage extends Factory[GetBlocksMessage] {

  private case class GetBlocksMessageImpl(
      protocolVersion: ProtocolVersion,
      hashCount: CompactSizeUInt,
      blockHeaderHashes: Seq[DoubleSha256Digest],
      stopHash: DoubleSha256Digest)
      extends GetBlocksMessage

  def apply(
      version: ProtocolVersion,
      hashCount: CompactSizeUInt,
      blockHeaderHashes: Seq[DoubleSha256Digest],
      stopHash: DoubleSha256Digest): GetBlocksMessage = {
    GetBlocksMessageImpl(version, hashCount, blockHeaderHashes, stopHash)
  }

  def apply(
      version: ProtocolVersion,
      blockHeaderHashes: Seq[DoubleSha256Digest],
      stopHash: DoubleSha256Digest): GetBlocksMessage = {
    val hashCount = CompactSizeUInt(UInt64(blockHeaderHashes.length))
    GetBlocksMessage(version, hashCount, blockHeaderHashes, stopHash)
  }

  def fromBytes(bytes: ByteVector): GetBlocksMessage =
    RawGetBlocksMessageSerializer.read(bytes)
}

/** The getdata message requests one or more data objects from another node.
  * The objects are requested by an inventory,
  * which the requesting node typically previously received by way of an inv message.
  *
  * @param inventoryCount The number of inventory enteries
  * @param inventories One or more inventory entries up to a maximum of 50,000 entries.
  *
  * @see [[https://bitcoin.org/en/developer-reference#getdata]]
  */
case class GetDataMessage(
    inventoryCount: CompactSizeUInt,
    inventories: Seq[Inventory])
    extends DataPayload {
  override def commandName = NetworkPayload.getDataCommandName

  override def bytes: ByteVector = RawGetDataMessageSerializer.write(this)

  override def toString(): String = {

    val count = s"inventoryCount=${inventoryCount.toInt}"
    val invs = s"inventories=${
      val base = inventories.toString
      val cutoff = 100
      if (base.length() > cutoff) base.take(cutoff) + "..."
      else base
    }"
    s"GetDataMessage($count, $invs)"
  }
}

object GetDataMessage extends Factory[GetDataMessage] {

  override def fromBytes(bytes: ByteVector): GetDataMessage = {
    RawGetDataMessageSerializer.read(bytes)
  }

  def apply(inventories: Seq[Inventory]): GetDataMessage = {
    val inventoryCount = CompactSizeUInt(UInt64(inventories.length))
    GetDataMessage(inventoryCount, inventories)
  }

  def apply(inventory: Inventory): GetDataMessage =
    GetDataMessage(Seq(inventory))
}

sealed trait ExpectsResponse {

  def isPayloadExpectedResponse(payload: NetworkPayload): Boolean = {
    this match {
      case _: GetBlocksMessage  => payload.isInstanceOf[BlockMessage]
      case _: GetHeadersMessage => payload.isInstanceOf[HeadersMessage]
      case _: GetCompactFiltersMessage =>
        payload.isInstanceOf[CompactFilterMessage]
      case _: GetCompactFilterHeadersMessage =>
        payload.isInstanceOf[CompactFilterHeadersMessage]
    }
  }
}

/** The getheaders message requests a headers message that provides block headers starting
  * from a particular point in the block chain.
  * It allows a peer which has been disconnected or started for the first time to get the
  * headers it hasn’t seen yet.
  * @see [[https://bitcoin.org/en/developer-reference#getheaders]]
  */
trait GetHeadersMessage extends DataPayload with ExpectsResponse {
  def version: ProtocolVersion
  def hashCount: CompactSizeUInt
  def hashes: Seq[DoubleSha256Digest]
  def hashStop: DoubleSha256Digest

  override def commandName = NetworkPayload.getHeadersCommandName
  override def bytes: ByteVector = RawGetHeadersMessageSerializer.write(this)

  override def toString(): String = {
    val count = hashCount.toInt
    // only display first hash, otherwise this gets really long
    val hashesStr = hashes match {
      case Nil         => "empty"
      case head +: Nil => head.toString
      case head +: _   => s"$head, ..."
    }
    s"GetHeadersMessage($version, hashCount=$count, hashes=$hashesStr, stop=$hashStop)"
  }

}

object GetHeadersMessage extends Factory[GetHeadersMessage] {

  private case class GetHeadersMessageImpl(
      version: ProtocolVersion,
      hashCount: CompactSizeUInt,
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest)
      extends GetHeadersMessage

  override def fromBytes(bytes: ByteVector): GetHeadersMessage =
    RawGetHeadersMessageSerializer.read(bytes)

  def apply(
      version: ProtocolVersion,
      hashCount: CompactSizeUInt,
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest): GetHeadersMessage = {
    GetHeadersMessageImpl(version, hashCount, hashes, hashStop)
  }

  def apply(
      version: ProtocolVersion,
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest): GetHeadersMessage = {
    val hashCount = CompactSizeUInt(UInt64(hashes.length))
    GetHeadersMessage(version, hashCount, hashes, hashStop)
  }

  /** Creates a [[GetHeadersMessage]] with the default protocol version */
  def apply(
      hashes: Seq[DoubleSha256Digest],
      hashStop: DoubleSha256Digest): GetHeadersMessage = {
    GetHeadersMessage(ProtocolVersion.default, hashes, hashStop)
  }

  /** Creates a [[GetHeadersMessage]] with no hash stop set, this requests all possible blocks
    * if we need more than 2000 block headers, we will have to send another [[GetHeadersMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#getheaders]]
    */
  def apply(hashes: Seq[DoubleSha256Digest]): GetHeadersMessage = {
    //The header hash of the last header hash being requested; set to all zeroes to request an inv message with all
    //subsequent header hashes (a maximum of 2000 will be sent as a reply to this message
    val hashStop = DoubleSha256Digest.empty
    GetHeadersMessage(hashes, hashStop)
  }

  def apply(hash: DoubleSha256Digest): GetHeadersMessage = {
    GetHeadersMessage(Vector(hash))
  }
}

/** The headers message sends one or more block headers to a node
  * which previously requested certain headers with a getheaders message.
  * @see [[https://bitcoin.org/en/developer-reference#headers]]
  *
  * @param count Number of block headers up to a maximum of 2,000.
  *              Note: headers-first sync assumes the sending node
  *              will send the maximum number of headers whenever possible.
  *
  * @param headers Block headers: each 80-byte block header is in the format described in the
  *                block headers section with an additional 0x00 suffixed.
  *                This 0x00 is called the transaction count, but because the headers message
  *                doesn’t include any transactions, the transaction count is always zero.
  */
case class HeadersMessage(count: CompactSizeUInt, headers: Vector[BlockHeader])
    extends DataPayload {

  override def commandName = NetworkPayload.headersCommandName

  override def bytes: ByteVector = RawHeadersMessageSerializer.write(this)

  override def toString(): String = {
    if (headers.nonEmpty) {
      s"HeadersMessage(${count},${headers.head.hashBE.hex}..${headers.last.hashBE.hex}"
    } else {
      super.toString
    }

  }
}

object HeadersMessage extends Factory[HeadersMessage] {

  /** The maximum amount of headers sent in one `headers` message
    *
    * @see [[https://bitcoin.org/en/developer-reference#getheaders bitcoin.org]]
    *      developer reference
    */
  val MaxHeadersCount: Int = 2000

  def fromBytes(bytes: ByteVector): HeadersMessage =
    RawHeadersMessageSerializer.read(bytes)

  /** Constructs a `headers` message from the given headers */
  def apply(headers: Vector[BlockHeader]): HeadersMessage = {
    val count = CompactSizeUInt(UInt64(headers.length))
    HeadersMessage(count, headers)
  }
}

/** The inv message (inventory message) transmits one or more inventories of objects known to the transmitting peer.
  * It can be sent unsolicited to announce new transactions or blocks,
  * or it can be sent in reply to a getblocks message or mempool message.
  * @see [[https://bitcoin.org/en/developer-reference#inv]]
  */
trait InventoryMessage extends DataPayload {

  /** The number of inventory enteries
    */
  def inventoryCount: CompactSizeUInt

  /** One or more inventory entries up to a maximum of 50,000 entries.
    */
  def inventories: Seq[Inventory]

  override def commandName = NetworkPayload.invCommandName

  override def bytes: ByteVector = RawInventoryMessageSerializer.write(this)

  override def toString(): String = {
    val invCount = inventoryCount.toInt
    val invList =
      if (invCount > 1) {
        val txCount = inventories.count { t =>
          t.typeIdentifier == TypeIdentifier.MsgTx ||
          t.typeIdentifier == TypeIdentifier.MsgWitnessTx
        }

        val blockCount = inventories.count { t =>
          t.typeIdentifier == TypeIdentifier.MsgBlock ||
          t.typeIdentifier == TypeIdentifier.MsgWitnessBlock
        }
        val otherCount = inventories.length - blockCount - txCount

        s"InventoryMessage(txCount=$txCount, blockCount=$blockCount, other=$otherCount)"
      } else {
        inventories.mkString
      }
    s"InventoryMessage($invCount inv(s)${if (invList.nonEmpty) ", " + invList
    else ""})"
  }
}

/** Creates an scala object that represents the inventory type on the p2p network
  * @see https://bitcoin.org/en/developer-reference#inv
  */
object InventoryMessage extends Factory[InventoryMessage] {

  private case class InventoryMessageImpl(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory])
      extends InventoryMessage

  override def fromBytes(bytes: ByteVector): InventoryMessage =
    RawInventoryMessageSerializer.read(bytes)

  def apply(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory]): InventoryMessage = {
    InventoryMessageImpl(inventoryCount, inventories)
  }

  def apply(inventories: Seq[Inventory]): InventoryMessage = {
    val count = CompactSizeUInt(UInt64(inventories.length))
    InventoryMessage(count, inventories)
  }
}

/** The mempool message requests the TXIDs of transactions that the receiving node has verified
  * as valid but which have not yet appeared in a block.
  * That is, transactions which are in the receiving node’s memory pool.
  * The response to the mempool message is one or more inv messages containing the TXIDs in the usual inventory format.
  *
  * @see [[https://bitcoin.org/en/developer-reference#mempool]]
  */
case object MemPoolMessage extends DataPayload {
  override val commandName = NetworkPayload.memPoolCommandName
  override val bytes: ByteVector = ByteVector.empty
}

/** The merkleblock message is a reply to a getdata message which requested a
  * block using the inventory type MSG_MERKLEBLOCK.
  * It is only part of the reply: if any matching transactions are found,
  * they will be sent separately as tx messages.
  *
  * @see [[https://bitcoin.org/en/developer-reference#merkleblock]]
  *
  * @param merkleBlock The actual [[org.bitcoins.core.protocol.blockchain.MerkleBlock MerkleBlock]] that this message represents
  */
case class MerkleBlockMessage(merkleBlock: MerkleBlock) extends DataPayload {

  override val commandName = NetworkPayload.merkleBlockCommandName

  def bytes: ByteVector = RawMerkleBlockMessageSerializer.write(this)

}

/** @see https://bitcoin.org/en/developer-reference#merkleblock
  */
object MerkleBlockMessage extends Factory[MerkleBlockMessage] {

  def fromBytes(bytes: ByteVector): MerkleBlockMessage =
    RawMerkleBlockMessageSerializer.read(bytes)

}

/** The notfound message is a reply to a getdata message which requested an object the receiving
  * node does not have available for relay. (Nodes are not expected to relay historic transactions
  * which are no longer in the memory pool or relay set.
  * Nodes may also have pruned spent transactions from older blocks, making them unable to send those blocks.)
  *
  * @see [[https://bitcoin.org/en/developer-reference#notfound]]
  */
trait NotFoundMessage extends DataPayload with InventoryMessage {
  override def commandName = NetworkPayload.notFoundCommandName
  override def bytes: ByteVector = RawNotFoundMessageSerializer.write(this)
}

/** The companion object factory used to create NotFoundMessages on the p2p network
  *
  * @see https://bitcoin.org/en/developer-reference#notfound
  */
object NotFoundMessage extends Factory[NotFoundMessage] {

  private case class NotFoundMessageImpl(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory])
      extends NotFoundMessage

  def fromBytes(bytes: ByteVector): NotFoundMessage =
    RawNotFoundMessageSerializer.read(bytes)

  def apply(inventories: Seq[Inventory]): NotFoundMessage = {
    val count = CompactSizeUInt(UInt64(inventories.length))
    apply(count, inventories)
  }

  def apply(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory]): NotFoundMessage = {
    NotFoundMessageImpl(inventoryCount, inventories)
  }
}

/** The tx message transmits a single transaction in the raw transaction format.
  * It can be sent in a variety of situations;
  * @param transaction The transaction being sent over the wire
  * @see [[https://bitcoin.org/en/developer-reference#tx]]
  */
case class TransactionMessage(transaction: Transaction) extends DataPayload {

  override val commandName = NetworkPayload.transactionCommandName
  override def bytes: ByteVector = RawTransactionMessageSerializer.write(this)

  override def toString(): String = s"TransactionMessage(${transaction.txIdBE})"
}

/** Companion factory object for the TransactionMessage on the p2p network
  * @see https://bitcoin.org/en/developer-reference#tx
  */
object TransactionMessage extends Factory[TransactionMessage] {

  def fromBytes(bytes: ByteVector): TransactionMessage =
    RawTransactionMessageSerializer.read(bytes)
}

/** Represents a control message on this network
  * [[https://bitcoin.org/en/developer-reference#control-messages]]
  */
sealed trait ControlPayload extends NetworkPayload

sealed trait GossipAddrMessage extends ControlPayload

/** The addr (IP address) message relays connection information for peers on the network.
  * Each peer which wants to accept incoming connections creates an addr message providing its
  * connection information and then sends that message to its peers unsolicited.
  * Some of its peers send that information to their peers (also unsolicited),
  * some of which further distribute it, allowing decentralized peer discovery for
  * any program already on the network.
  * @see [[https://bitcoin.org/en/developer-reference#addr]]
  */
trait AddrMessage extends GossipAddrMessage {
  def ipCount: CompactSizeUInt
  def addresses: Seq[NetworkIpAddress]
  override def commandName = NetworkPayload.addrCommandName
  override def bytes: ByteVector = RawAddrMessageSerializer.write(this)
}

/** The companion object for an AddrMessage
  * @see https://bitcoin.org/en/developer-reference#addr
  */
object AddrMessage extends Factory[AddrMessage] {

  private case class AddrMessageImpl(
      ipCount: CompactSizeUInt,
      addresses: Seq[NetworkIpAddress])
      extends AddrMessage

  def fromBytes(bytes: ByteVector): AddrMessage =
    RawAddrMessageSerializer.read(bytes)

  def apply(addresses: Seq[NetworkIpAddress]): AddrMessage = {
    val count = CompactSizeUInt(UInt64(addresses.length))
    apply(count, addresses)
  }

  def apply(
      ipCount: CompactSizeUInt,
      addresses: Seq[NetworkIpAddress]): AddrMessage =
    AddrMessageImpl(ipCount, addresses)

}

/** addrV2 relays information about a peer. It supports many different
  * address types and networks.
  *
  * @see https://github.com/bitcoin/bips/blob/master/bip-0155.mediawiki
  */
sealed trait AddrV2Message extends GossipAddrMessage {
  def time: UInt32
  def services: CompactSizeUInt
  def networkId: Byte
  def addrBytes: ByteVector
  def port: UInt16

  override def commandName: String = NetworkPayload.addrV2CommandName

  override def bytes: ByteVector =
    time.bytes ++ services.bytes ++ ByteVector.fromByte(
      networkId) ++ addrBytes ++ port.bytes
}

/** addrv2 message that contains an IPv4 address */
case class IPv4AddrV2Message(
    time: UInt32,
    services: CompactSizeUInt,
    addr: InetAddress,
    port: UInt16)
    extends AddrV2Message {
  override val networkId: Byte = AddrV2Message.IPV4_NETWORK_BYTE

  override val addrBytes: ByteVector = addr.ipv4Bytes

  require(addrBytes.size == AddrV2Message.IPV4_ADDR_LENGTH,
          "Incorrect size of IPv4 message, consider using IPv6AddrV2Message")
}

/** addrv2 message that contains an IPv6 address */
case class IPv6AddrV2Message(
    time: UInt32,
    services: CompactSizeUInt,
    addr: InetAddress,
    port: UInt16)
    extends AddrV2Message {
  override val networkId: Byte = AddrV2Message.IPV6_NETWORK_BYTE

  override val addrBytes: ByteVector = ByteVector(addr.getAddress)

  require(addrBytes.size == AddrV2Message.IPV6_ADDR_LENGTH,
          "Incorrect size of IPv6 message, consider using IPv4AddrV2Message")
}

/** addrv2 message that contains a TorV2 address */
case class TorV2AddrV2Message(
    time: UInt32,
    services: CompactSizeUInt,
    addrBytes: ByteVector,
    port: UInt16)
    extends AddrV2Message {
  require(
    addrBytes.size == AddrV2Message.TOR_V2_ADDR_LENGTH,
    s"TorV2 addresses are ${AddrV2Message.TOR_V2_ADDR_LENGTH} bytes, got ${addrBytes.size}")
  override val networkId: Byte = AddrV2Message.TOR_V2_NETWORK_BYTE
}

/** addrv2 message that contains a TorV3 address */
case class TorV3AddrV2Message(
    time: UInt32,
    services: CompactSizeUInt,
    addrBytes: ByteVector,
    port: UInt16)
    extends AddrV2Message {
  require(
    addrBytes.size == AddrV2Message.TOR_V3_ADDR_LENGTH,
    s"TorV3 addresses are ${AddrV2Message.TOR_V3_ADDR_LENGTH} bytes, got ${addrBytes.size}")
  override val networkId: Byte = AddrV2Message.TOR_V3_NETWORK_BYTE
}

/** addrv2 message that contains an I2P address */
case class I2PAddrV2Message(
    time: UInt32,
    services: CompactSizeUInt,
    addrBytes: ByteVector,
    port: UInt16)
    extends AddrV2Message {
  require(
    addrBytes.size == AddrV2Message.I2P_ADDR_LENGTH,
    s"I2P addresses are ${AddrV2Message.I2P_ADDR_LENGTH} bytes, got ${addrBytes.size}")
  override val networkId: Byte = AddrV2Message.I2P_NETWORK_BYTE
}

/** addrv2 message that contains an CJDNS address */
case class CJDNSAddrV2Message(
    time: UInt32,
    services: CompactSizeUInt,
    addrBytes: ByteVector,
    port: UInt16)
    extends AddrV2Message {
  require(addrBytes.head == 0xfc.toByte,
          s"CJDNS addresses start with 0xFC, got $addrBytes")
  require(
    addrBytes.size == AddrV2Message.CJDNS_ADDR_LENGTH,
    s"CJDNS addresses are ${AddrV2Message.CJDNS_ADDR_LENGTH} bytes, got ${addrBytes.size}")
  override val networkId: Byte = AddrV2Message.CJDNS_NETWORK_BYTE
}

/** addrv2 message that contains an address from a network we do not understand */
case class UnknownNetworkAddrV2Message(
    time: UInt32,
    services: CompactSizeUInt,
    networkId: Byte,
    addrBytes: ByteVector,
    port: UInt16)
    extends AddrV2Message {
  require(!AddrV2Message.knownNetworkBytes.contains(networkId),
          s"$networkId is a known network byte")
}

/** The companion object for an AddrV2Message
  * @see https://developer.bitcoin.org/reference/p2p_networking.html#addrv2
  * @see https://github.com/bitcoin/bips/blob/master/bip-0155.mediawiki
  */
object AddrV2Message extends Factory[AddrV2Message] {

  final val IPV4_NETWORK_BYTE: Byte = 0x01
  final val IPV4_ADDR_LENGTH: Int = 4

  final val IPV6_NETWORK_BYTE: Byte = 0x02
  final val IPV6_ADDR_LENGTH: Int = 16

  final val TOR_V2_NETWORK_BYTE: Byte = 0x03
  final val TOR_V2_ADDR_LENGTH: Int = TorAddress.TOR_V2_ADDR_LENGTH

  final val TOR_V3_NETWORK_BYTE: Byte = 0x04
  final val TOR_V3_ADDR_LENGTH: Int = TorAddress.TOR_V3_ADDR_LENGTH

  final val I2P_NETWORK_BYTE: Byte = 0x05
  final val I2P_ADDR_LENGTH: Int = 32

  final val CJDNS_NETWORK_BYTE: Byte = 0x06
  final val CJDNS_ADDR_LENGTH: Int = 16

  val knownNetworkBytes: Vector[Byte] = Vector(IPV4_NETWORK_BYTE,
                                               IPV6_NETWORK_BYTE,
                                               TOR_V2_NETWORK_BYTE,
                                               TOR_V3_NETWORK_BYTE,
                                               I2P_NETWORK_BYTE,
                                               CJDNS_NETWORK_BYTE)

  override def fromBytes(bytes: ByteVector): AddrV2Message = {
    val timeBytes = bytes.take(4)
    val time = UInt32(timeBytes)

    val services = CompactSizeUInt(bytes.drop(4))

    val remainingBytes = bytes.drop(4 + services.bytes.size)

    val networkId = remainingBytes.head

    val addrPortBytes = remainingBytes.tail

    networkId match {
      case IPV4_NETWORK_BYTE =>
        val addrBytes = addrPortBytes.take(IPV4_ADDR_LENGTH)
        val address = InetAddress.getByAddress(addrBytes.toArray)
        val port = UInt16(addrPortBytes.drop(IPV4_ADDR_LENGTH).take(2))
        IPv4AddrV2Message(time, services, address, port)
      case IPV6_NETWORK_BYTE =>
        val addrBytes = addrPortBytes.take(IPV6_ADDR_LENGTH)
        val address = InetAddress.getByAddress(addrBytes.toArray)
        val port = UInt16(addrPortBytes.drop(IPV6_ADDR_LENGTH).take(2))
        IPv6AddrV2Message(time, services, address, port)
      case TOR_V2_NETWORK_BYTE =>
        val addrBytes = addrPortBytes.take(TOR_V2_ADDR_LENGTH)
        val port = UInt16(addrPortBytes.drop(TOR_V2_ADDR_LENGTH).take(2))
        TorV2AddrV2Message(time, services, addrBytes, port)
      case TOR_V3_NETWORK_BYTE =>
        val addrBytes = addrPortBytes.take(TOR_V3_ADDR_LENGTH)
        val port = UInt16(addrPortBytes.drop(TOR_V3_ADDR_LENGTH).take(2))
        TorV3AddrV2Message(time, services, addrBytes, port)
      case I2P_NETWORK_BYTE =>
        val addrBytes = addrPortBytes.take(I2P_ADDR_LENGTH)
        val port = UInt16(addrPortBytes.drop(I2P_ADDR_LENGTH).take(2))
        I2PAddrV2Message(time, services, addrBytes, port)
      case CJDNS_NETWORK_BYTE =>
        val addrBytes = addrPortBytes.take(CJDNS_ADDR_LENGTH)
        val port = UInt16(addrPortBytes.drop(CJDNS_ADDR_LENGTH).take(2))
        CJDNSAddrV2Message(time, services, addrBytes, port)
      case unknown: Byte =>
        val addrBytes = addrPortBytes.dropRight(2)
        val port = UInt16(addrPortBytes.drop(addrBytes.size))
        UnknownNetworkAddrV2Message(time, services, unknown, addrBytes, port)

    }
  }
}

/** Sending such a message indicates that a node can understand and
  * prefers to receive addrv2 messages instead of addr messages.
  * I.e. "Send me addrv2".sendaddrv2 SHOULD be sent after receiving the verack message from the peer.
  * For older peers, that did not emit sendaddrv2, keep sending the legacy
  * addr message, ignoring addresses with the newly introduced address types.
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0155.mediawiki#signaling-support-and-compatibility]]
  */
case object SendAddrV2Message extends ControlPayload {
  override val commandName: String = NetworkPayload.sendAddrV2CommandName
  override val bytes: ByteVector = ByteVector.empty
}

/** The feefilter message is a request to the receiving peer to not relay any transaction inv messages
  * to the sending peer where the fee rate for the transaction is below the fee rate specified in the
  * feefilter message.
  *
  * feefilter was introduced in Bitcoin Core 0.13.0 following the introduction of mempool limiting in
  * Bitcoin Core 0.12.0. Mempool limiting provides protection against attacks and spam transactions
  * that have low fee rates and are unlikely to be included in mined blocks. The feefilter messages
  * allows a node to inform its peers that it will not accept transactions below a specified fee rate
  * into its mempool, and therefore that the peers can skip relaying inv messages for transactions below
  * that fee rate to that node.
  */
trait FeeFilterMessage extends ControlPayload {

  /** The raw fee rate, in satoshis per kb. This is what is defined in the p2p message */
  def feeRate: SatoshisPerKiloByte

  def satPerByte: SatoshisPerByte = {
    feeRate.toSatPerByte
  }

  override def commandName: String = NetworkPayload.feeFilterCommandName

  override def bytes: ByteVector = {
    RawFeeFilterMessageSerializer.write(this)
  }
}

object FeeFilterMessage extends Factory[FeeFilterMessage] {

  private case class FeeFilterMessageImpl(feeRate: SatoshisPerKiloByte)
      extends FeeFilterMessage

  override def fromBytes(bytes: ByteVector): FeeFilterMessage = {
    RawFeeFilterMessageSerializer.read(bytes)
  }

  def apply(satoshisPerKiloByte: SatoshisPerKiloByte): FeeFilterMessage = {
    FeeFilterMessageImpl(satoshisPerKiloByte)
  }

  def apply(satPerByte: SatoshisPerByte): FeeFilterMessage = {
    FeeFilterMessage(satPerByte.toSatPerKb)
  }
}

/** The filteradd message tells the receiving peer to add a single element to a
  * previously-set bloom filter, such as a new public key.
  * The element is sent directly to the receiving peer; the peer then uses the parameters
  * set in the filterload message to add the element to the bloom filter.
  * @see [[https://bitcoin.org/en/developer-reference#filteradd]]
  */
trait FilterAddMessage extends ControlPayload {

  /** The number of bytes in the following element field.
    */
  def elementSize: CompactSizeUInt

  /** The element to add to the current filter.
    * Maximum of 520 bytes, which is the maximum size of an element which can be pushed
    * onto the stack in a pubkey or signature script.
    * Elements must be sent in the byte order they would use when appearing in a raw transaction;
    * for example, hashes should be sent in internal byte order.
    */
  def element: ByteVector

  override def commandName = NetworkPayload.filterAddCommandName

  override def bytes: ByteVector = RawFilterAddMessageSerializer.write(this)
}

/** @see [[https://bitcoin.org/en/developer-reference#filteradd]]
  */
object FilterAddMessage extends Factory[FilterAddMessage] {

  private case class FilterAddMessageImpl(
      elementSize: CompactSizeUInt,
      element: ByteVector)
      extends FilterAddMessage

  override def fromBytes(bytes: ByteVector): FilterAddMessage =
    RawFilterAddMessageSerializer.read(bytes)

  def apply(
      elementSize: CompactSizeUInt,
      element: ByteVector): FilterAddMessage = {
    FilterAddMessageImpl(elementSize, element)
  }

  /** Constructs a `FilterAddMessage` from the given hash digest */
  def fromHash(hash: HashDigest): FilterAddMessage = {
    FilterAddMessageImpl(CompactSizeUInt(UInt64(hash.bytes.length)), hash.bytes)
  }

}

/** The filterclear message tells the receiving peer to remove a previously-set bloom filter.
  * This also undoes the effect of setting the relay field in the version message to 0,
  * allowing unfiltered access to inv messages announcing new transactions.
  * @see [[https://bitcoin.org/en/developer-reference#filterclear]]
  */
case object FilterClearMessage extends ControlPayload {
  override val commandName = NetworkPayload.filterClearCommandName
  override val bytes: ByteVector = ByteVector.empty
}

/** The filterload message tells the receiving peer to filter all relayed transactions and
  * requested merkle blocks through the provided filter.
  * This allows clients to receive transactions relevant to their wallet plus a configurable
  * rate of false positive transactions which can provide plausible-deniability privacy.
  * @see [[https://bitcoin.org/en/developer-reference#filterload]]
  */
trait FilterLoadMessage extends ControlPayload {

  /** The underlying bloom filter inside of the FilterLoadMessage */
  def bloomFilter: BloomFilter

  override def commandName = NetworkPayload.filterLoadCommandName

  override def bytes: ByteVector = RawFilterLoadMessageSerializer.write(this)
}

/** @see [[https://bitcoin.org/en/developer-reference#filterload]]
  */
object FilterLoadMessage extends Factory[FilterLoadMessage] {

  private case class FilterLoadMessageImpl(bloomFilter: BloomFilter)
      extends FilterLoadMessage {
    require(
      bloomFilter.filterSize.num.toLong <= BloomFilter.maxSize.toLong,
      "Can only have a maximum of 36,000 bytes in our filter, got: " + bloomFilter.data.size)
    require(
      bloomFilter.hashFuncs <= BloomFilter.maxHashFuncs,
      "Can only have a maximum of 50 hashFuncs inside FilterLoadMessage, got: " + bloomFilter.hashFuncs)
    require(
      bloomFilter.filterSize.num.toLong == bloomFilter.data.size,
      "Filter Size compactSizeUInt and actual filter size were different, " +
        "filterSize: " + bloomFilter.filterSize.num + " actual filter size: " + bloomFilter.data.length
    )
  }

  override def fromBytes(bytes: ByteVector): FilterLoadMessage =
    RawFilterLoadMessageSerializer.read(bytes)

  def apply(
      filterSize: CompactSizeUInt,
      filter: ByteVector,
      hashFuncs: UInt32,
      tweak: UInt32,
      flags: BloomFlag): FilterLoadMessage = {
    val bloomFilter = BloomFilter(filterSize, filter, hashFuncs, tweak, flags)
    FilterLoadMessage(bloomFilter)
  }

  def apply(
      filter: ByteVector,
      hashFuncs: UInt32,
      tweak: UInt32,
      flags: BloomFlag): FilterLoadMessage = {
    val filterSize = CompactSizeUInt(UInt64(filter.length))
    FilterLoadMessage(filterSize, filter, hashFuncs, tweak, flags)
  }

  def apply(bloomFilter: BloomFilter): FilterLoadMessage = {
    FilterLoadMessageImpl(bloomFilter)
  }
}

/** The getaddr message requests an addr message from the receiving node,
  * preferably one with lots of IP addresses of other receiving nodes.
  * The transmitting node can use those IP addresses to quickly update its
  * database of available nodes rather than waiting for unsolicited addr messages to arrive over time.
  * @see [[https://bitcoin.org/en/developer-reference#getaddr]]
  */
case object GetAddrMessage extends ControlPayload {
  override val commandName = NetworkPayload.getAddrCommandName
  override val bytes: ByteVector = ByteVector.empty
}

/** The ping message helps confirm that the receiving peer is still connected.
  * If a TCP/IP error is encountered when sending the ping message (such as a connection timeout),
  * the transmitting node can assume that the receiving node is disconnected.
  * The response to a ping message is the pong message.
  * @see [[https://bitcoin.org/en/developer-reference#ping]]
  */
trait PingMessage extends ControlPayload {

  /** Random nonce assigned to this ping message.
    * The responding pong message will include this nonce
    * to identify the ping message to which it is replying.
    */
  def nonce: UInt64

  override def commandName = NetworkPayload.pingCommandName

  override def bytes: ByteVector = RawPingMessageSerializer.write(this)
}

object PingMessage extends Factory[PingMessage] {
  private case class PingMessageImpl(nonce: UInt64) extends PingMessage

  override def fromBytes(bytes: ByteVector): PingMessage = {
    val pingMsg = RawPingMessageSerializer.read(bytes)
    pingMsg
  }

  def apply(nonce: UInt64): PingMessage = PingMessageImpl(nonce)
}

/** The pong message replies to a ping message, proving to the pinging node that the ponging node is still alive.
  * Bitcoin Core will, by default, disconnect from any clients which have not responded
  * to a ping message within 20 minutes.
  * @see [[https://bitcoin.org/en/developer-reference#pong]]
  */
trait PongMessage extends ControlPayload {

  /** The nonce which is the nonce in the ping message the peer is responding too
    */
  def nonce: UInt64

  override def commandName = NetworkPayload.pongCommandName

  override def bytes: ByteVector = RawPongMessageSerializer.write(this)

  override def toString(): String = s"PongMessage(${nonce.toBigInt})"

}

object PongMessage extends Factory[PongMessage] {
  private case class PongMessageImpl(nonce: UInt64) extends PongMessage

  def fromBytes(bytes: ByteVector): PongMessage = {
    val pongMsg = RawPongMessageSerializer.read(bytes)
    pongMsg
  }

  def apply(nonce: UInt64): PongMessage = PongMessageImpl(nonce)
}

/** The reject message informs the receiving node that one of its previous messages has been rejected.
  * @see [[https://bitcoin.org/en/developer-reference#reject]]
  */
trait RejectMessage extends ControlPayload {

  /** The number of bytes in the following message field.
    */
  def messageSize: CompactSizeUInt

  /** The type of message rejected as ASCII text without null padding.
    * For example: “tx”, “block”, or “version”.
    */
  def message: String

  /** The reject message code.
    */
  def code: Char

  /** The number of bytes in the following reason field.
    * May be 0x00 if a text reason isn’t provided.
    */
  def reasonSize: CompactSizeUInt

  /** The reason for the rejection in ASCII text.
    * This should not be displayed to the user; it is only for debugging purposes.
    */
  def reason: String

  /** Optional additional data provided with the rejection.
    * For example, most rejections of tx messages or block messages include
    * the hash of the rejected transaction or block header. See the code table below.
    */
  def extra: ByteVector

  override def commandName = NetworkPayload.rejectCommandName

  override def bytes: ByteVector = RawRejectMessageSerializer.write(this)
}

/** @see [[https://bitcoin.org/en/developer-reference#reject]]
  */
object RejectMessage extends Factory[RejectMessage] {

  private case class RejectMessageImpl(
      messageSize: CompactSizeUInt,
      message: String,
      code: Char,
      reasonSize: CompactSizeUInt,
      reason: String,
      extra: ByteVector)
      extends RejectMessage

  def apply(
      messageSize: CompactSizeUInt,
      message: String,
      code: Char,
      reasonSize: CompactSizeUInt,
      reason: String,
      extra: ByteVector): RejectMessage = {
    RejectMessageImpl(messageSize, message, code, reasonSize, reason, extra)
  }

  def fromBytes(bytes: ByteVector): RejectMessage =
    RawRejectMessageSerializer.read(bytes)

  def apply(
      message: String,
      code: Char,
      reason: String,
      extra: ByteVector): RejectMessage = {
    val messageSize: CompactSizeUInt = CompactSizeUInt(UInt64(message.size))
    val reasonSize: CompactSizeUInt = CompactSizeUInt(UInt64(reason.size))
    RejectMessage(messageSize, message, code, reasonSize, reason, extra)
  }
}

/** The sendheaders message tells the receiving peer to send new block announcements
  * using a headers message rather than an inv message.
  * There is no payload in a sendheaders message. See the message header section for an example
  * of a message without a payload.
  * @see [[https://bitcoin.org/en/developer-reference#sendheaders]]
  */
case object SendHeadersMessage extends ControlPayload {
  override def commandName = NetworkPayload.sendHeadersCommandName
  override def bytes: ByteVector = ByteVector.empty
}

/** The verack message acknowledges a previously-received version message,
  * informing the connecting node that it can begin to send other messages.
  * The verack message has no payload; for an example of a message with no payload,
  * see the message headers section.
  * @see [[https://bitcoin.org/en/developer-reference#verack]]
  */
case object VerAckMessage extends ControlPayload {
  override val commandName = NetworkPayload.verAckCommandName
  override val bytes: ByteVector = ByteVector.empty
}

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#getcfilters BIP157]]
  */
case class GetCompactFiltersMessage(
    filterType: FilterType,
    startHeight: UInt32,
    stopHash: DoubleSha256Digest)
    extends DataPayload
    with ExpectsResponse {
  val commandName: String = NetworkPayload.getCompactFiltersCommandName

  def bytes: ByteVector = RawGetCompactFiltersMessageSerializer.write(this)

  override def toString: String = {
    s"GetCompactFiltersMessage(filterType=$filterType,startHeight=$startHeight,stopHash=${stopHash.flip})"
  }
}

object GetCompactFiltersMessage extends Factory[GetCompactFiltersMessage] {

  def fromBytes(bytes: ByteVector): GetCompactFiltersMessage =
    RawGetCompactFiltersMessageSerializer.read(bytes)

  /** Constructs a message with the default basic filter type */
  def apply(startHeight: Int, stopHash: DoubleSha256Digest) =
    new GetCompactFiltersMessage(FilterType.Basic,
                                 UInt32(startHeight),
                                 stopHash)
}

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfilter BIP157]]
  */
case class CompactFilterMessage(
    filterType: FilterType,
    blockHash: DoubleSha256Digest,
    filterBytes: ByteVector
) extends DataPayload {

  val blockHashBE: DoubleSha256DigestBE = blockHash.flip

  /** The number of filter bytes in this message */
  val numFilterBytes: CompactSizeUInt = CompactSizeUInt(
    UInt64(filterBytes.length))

  val commandName: String = NetworkPayload.compactFilterCommandName
  def bytes: ByteVector = RawCompactFilterMessageSerializer.write(this)

  override def toString: String = {
    s"CompactFilterMessage($filterType, blockHashBE=${blockHash.flip}, $bytes)"
  }
}

object CompactFilterMessage extends Factory[CompactFilterMessage] {

  /** Constructs a message from the tiven blockhash and filter */
  def apply(
      blockHash: DoubleSha256Digest,
      filter: GolombFilter): CompactFilterMessage = {
    val filterBytes = filter.bytes
    new CompactFilterMessage(FilterType.Basic, blockHash, filterBytes)
  }

  def fromBytes(bytes: ByteVector): CompactFilterMessage =
    RawCompactFilterMessageSerializer.read(bytes)

}

/** `getcfheaders` is used to request verifiable filter headers for a range of blocks
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#getcfheaders BIP157]]
  */
case class GetCompactFilterHeadersMessage(
    filterType: FilterType,
    startHeight: UInt32,
    stopHash: DoubleSha256Digest
) extends DataPayload
    with ExpectsResponse {
  val commandName: String = NetworkPayload.getCompactFilterHeadersCommandName

  def bytes: ByteVector =
    RawGetCompactFilterHeadersMessageSerializer.write(this)
}

object GetCompactFilterHeadersMessage
    extends Factory[GetCompactFilterHeadersMessage] {

  /** Constructs a message from the given startheight and stophash */
  def apply(
      startHeight: Int,
      stopHash: DoubleSha256Digest,
      filterType: FilterType =
        FilterType.Basic): GetCompactFilterHeadersMessage = {
    new GetCompactFilterHeadersMessage(filterType,
                                       UInt32(startHeight),
                                       stopHash)
  }

  def fromBytes(bytes: ByteVector): GetCompactFilterHeadersMessage =
    RawGetCompactFilterHeadersMessageSerializer.read(bytes)
}

/** `cfheaders` is sent in response to `getcfheaders`. Instead of including
  * the filter headers themselves, the response includes one filter header
  * and a sequence of filter hashes, from which the headers can be derived.
  * This has the benefit that the client can verify the binding links
  * between the headers.
  *
  * TODO: doc on params
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfheaders BIP157]]
  */
case class CompactFilterHeadersMessage(
    filterType: FilterType,
    stopHash: DoubleSha256Digest,
    previousFilterHeader: DoubleSha256Digest,
    filterHashes: Vector[DoubleSha256Digest])
    extends DataPayload {

  val stopHashBE: DoubleSha256DigestBE = stopHash.flip

  /** The number of hashes in this message */
  val filterHashesLength: CompactSizeUInt = CompactSizeUInt(
    UInt64(filterHashes.length))

  val commandName: String = NetworkPayload.compactFilterHeadersCommandName
  def bytes: ByteVector = RawCompactFilterHeadersMessageSerializer.write(this)

  def filterHeaders: Vector[FilterHeader] = {
    val z = FilterHeader(filterHashes.head, previousFilterHeader)
    filterHashes.tail.foldLeft(Vector(z)) { (acc, nextFilterHash) =>
      acc :+ acc.last.nextHeader(nextFilterHash)
    }
  }

  override def toString: String = {
    val hashesString = {
      if (filterHashes.isEmpty) {
        "empty"
      } else {
        s"${filterHashes.head.flip.hex}...${filterHashes.last.flip.hex}"
      }
    }
    s"CompactFilterHeadersMessage(filterType=${filterType}, " +
      s"previousFilterHeader=${previousFilterHeader.flip.hex}, " +
      s"stopHash=${stopHash.flip.hex} " +
      s"filterHeaders=${hashesString})"
  }
}

object CompactFilterHeadersMessage
    extends Factory[CompactFilterHeadersMessage] {

  def fromBytes(bytes: ByteVector): CompactFilterHeadersMessage =
    RawCompactFilterHeadersMessageSerializer.read(bytes)
}

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#getcfcheckpt BIP157]]
  */
case class GetCompactFilterCheckPointMessage(
    filterType: FilterType,
    stopHash: DoubleSha256Digest)
    extends DataPayload {
  val commandName: String = NetworkPayload.getCompactFilterCheckpointCommandName

  def bytes: ByteVector =
    RawGetCompactFilterCheckpointMessageSerializer.write(this)
}

object GetCompactFilterCheckPointMessage
    extends Factory[GetCompactFilterCheckPointMessage] {

  def apply(stopHash: DoubleSha256Digest): GetCompactFilterCheckPointMessage = {
    new GetCompactFilterCheckPointMessage(FilterType.Basic, stopHash)
  }

  def fromBytes(bytes: ByteVector): GetCompactFilterCheckPointMessage =
    RawGetCompactFilterCheckpointMessageSerializer.read(bytes)
}

/** @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#cfcheckpt BIP-157 ]]
  */
case class CompactFilterCheckPointMessage(
    filterType: FilterType,
    stopHash: DoubleSha256Digest,
    filterHeaders: Vector[DoubleSha256Digest])
    extends DataPayload {

  /** The amount of filter headers in this message */
  val filterHeadersLength: CompactSizeUInt = CompactSizeUInt(
    UInt64(filterHeaders.length))

  val commandName: String = NetworkPayload.compactFilterCheckpointCommandName

  def bytes: ByteVector =
    RawCompactFilterCheckpointMessageSerializer.write(this)

  override def toString: String = {
    val headersString = {
      if (filterHeaders.isEmpty) {
        "empty"
      } else {
        s"${filterHeaders.head.flip.hex}...${filterHeaders.last.flip.hex}"
      }
    }
    s"CompactFilterCheckPointMessage(filterType=${filterType}, stopHash=${stopHash.flip.hex}, filterHeaders=${headersString})"
  }
}

object CompactFilterCheckPointMessage
    extends Factory[CompactFilterCheckPointMessage] {

  def fromBytes(bytes: ByteVector): CompactFilterCheckPointMessage =
    RawCompactFilterCheckpointMessageSerializer.read(bytes)
}

/** The version message provides information about the transmitting node to the
  * receiving node at the beginning of a connection.
  * Until both peers have exchanged version messages, no other messages will be accepted.
  * If a version message is accepted, the receiving node should send a verack message—but
  * no node should send a verack message before initializing its half of the connection
  * by first sending a version message.
  * [[https://bitcoin.org/en/developer-reference#version]]
  */
trait VersionMessage extends ControlPayload {

  /** The highest protocol version understood by the transmitting node. See the protocol version section.
    */
  def version: ProtocolVersion

  /** The services supported by the transmitting node encoded as a bitfield. See the list of service codes below.
    */
  def services: ServiceIdentifier

  /** The current Unix epoch time according to the transmitting node’s clock.
    * Because nodes will reject blocks with timestamps more than two hours in the future,
    * this field can help other nodes to determine that their clock is wrong.
    */
  def timestamp: Int64

  /** The services supported by the receiving node as perceived by the transmitting node.
    * Same format as the ‘services’ field above.
    * Bitcoin Core will attempt to provide accurate information. BitcoinJ will, by default, always send 0.
    */
  def addressReceiveServices: ServiceIdentifier

  /** The IPv6 address of the receiving node as perceived by the transmitting node in big endian byte order.
    * IPv4 addresses can be provided as IPv4-mapped IPv6 addresses.
    * Bitcoin Core will attempt to provide accurate information
    * BitcoinJ will, by default, always return ::ffff:127.0.0.1
    * This is the network address of the node receiving this message
    */
  def addressReceiveIpAddress: InetAddress

  /** The port number of the receiving node as perceived by the transmitting node in big endian byte order.
    */
  def addressReceivePort: Int

  /** The services supported by the transmitting node. Should be identical to the ‘services’ field above.
    */
  def addressTransServices: ServiceIdentifier

  /** The IPv6 address of the transmitting node in big endian byte order.
    * IPv4 addresses can be provided as IPv4-mapped IPv6 addresses.
    * Set to ::ffff:127.0.0.1 if unknown.
    * This is the network address of the node emitting this message
    */
  def addressTransIpAddress: InetAddress

  /** The port number of the transmitting node in big endian byte order.
    */
  def addressTransPort: Int

  /** A random nonce which can help a node detect a connection to itself.
    * If the nonce is 0, the nonce field is ignored.
    * If the nonce is anything else, a node should terminate the connection on receipt
    * of a version message with a nonce it previously sent.
    */
  def nonce: UInt64

  /** Number of bytes in following user_agent field. If 0x00, no user agent field is sent.
    */
  def userAgentSize: CompactSizeUInt

  /** User agent as defined by BIP14. Previously called subVer.
    */
  def userAgent: String

  /** The height of the transmitting node’s best block chain or,
    * in the case of an SPV client, best block header chain.
    */
  def startHeight: Int32

  /** Transaction relay flag. If 0x00, no inv messages or tx messages announcing new transactions
    * should be sent to this client until it sends a filterload message or filterclear message.
    * If 0x01, this node wants inv messages and tx messages announcing new transactions.
    */
  def relay: Boolean

  override def commandName = NetworkPayload.versionCommandName

  override def bytes: ByteVector = RawVersionMessageSerializer.write(this)

  // TODO addressTransServices,  addressTransIpAddress and addressTransPort
  // what do these fields mean?
  override def toString(): String =
    s"VersionMessage($version, $services, epoch=${timestamp.toLong}, receiverServices=${addressReceiveIpAddress.bytes.toHex}, receiverAddress=${addressReceiveIpAddress.bytes.toHex}, receiverPort=$addressReceivePort), userAgent=$userAgent, startHeight=${startHeight.toInt}, relay=$relay)"

}

/** @see https://bitcoin.org/en/developer-reference#version
  */
object VersionMessage extends Factory[VersionMessage] {

  private case class VersionMessageImpl(
      version: ProtocolVersion,
      services: ServiceIdentifier,
      timestamp: Int64,
      addressReceiveServices: ServiceIdentifier,
      addressReceiveIpAddress: InetAddress,
      addressReceivePort: Int,
      addressTransServices: ServiceIdentifier,
      addressTransIpAddress: InetAddress,
      addressTransPort: Int,
      nonce: UInt64,
      userAgentSize: CompactSizeUInt,
      userAgent: String,
      startHeight: Int32,
      relay: Boolean)
      extends VersionMessage

  override def fromBytes(bytes: ByteVector): VersionMessage =
    RawVersionMessageSerializer.read(bytes)

  def apply(
      version: ProtocolVersion,
      services: ServiceIdentifier,
      timestamp: Int64,
      addressReceiveServices: ServiceIdentifier,
      addressReceiveIpAddress: InetAddress,
      addressReceivePort: Int,
      addressTransServices: ServiceIdentifier,
      addressTransIpAddress: InetAddress,
      addressTransPort: Int,
      nonce: UInt64,
      userAgent: String,
      startHeight: Int32,
      relay: Boolean): VersionMessage = {
    val userAgentSize: CompactSizeUInt =
      CompactSizeUInt.calculateCompactSizeUInt(ByteVector(userAgent.getBytes))
    VersionMessageImpl(
      version = version,
      services = services,
      timestamp = timestamp,
      addressReceiveServices = addressReceiveServices,
      addressReceiveIpAddress = addressReceiveIpAddress,
      addressReceivePort = addressReceivePort,
      addressTransServices = addressTransServices,
      addressTransIpAddress = addressTransIpAddress,
      addressTransPort = addressTransPort,
      nonce = nonce,
      userAgentSize = userAgentSize,
      userAgent = userAgent,
      startHeight = startHeight,
      relay = relay
    )
  }

  def apply(
      network: NetworkParameters,
      receivingIpAddress: InetAddress,
      transmittingIpAddress: InetAddress,
      relay: Boolean): VersionMessage = {
    VersionMessage(network,
                   ProtocolVersion.userAgent,
                   Int32.zero,
                   receivingIpAddress,
                   transmittingIpAddress,
                   relay)
  }

  def apply(
      network: NetworkParameters,
      userAgent: String,
      startHeight: Int32,
      receivingIpAddress: InetAddress,
      transmittingIpAddress: InetAddress,
      relay: Boolean): VersionMessage = {
    val nonce = UInt64.zero
    VersionMessage(
      version = ProtocolVersion.default,
      services = ServiceIdentifier.NODE_WITNESS,
      timestamp = Int64(java.time.Instant.now.getEpochSecond),
      addressReceiveServices = ServiceIdentifier.NODE_WITNESS,
      addressReceiveIpAddress = receivingIpAddress,
      addressReceivePort = network.port,
      addressTransServices = ServiceIdentifier.NODE_NETWORK,
      addressTransIpAddress = transmittingIpAddress,
      addressTransPort = network.port,
      nonce = nonce,
      userAgent = userAgent,
      startHeight = startHeight,
      relay = relay
    )
  }
}

object NetworkPayload {
  private[core] val alertCommandName = "alert"
  private[core] val blockCommandName = "block"
  private[core] val getBlocksCommandName = "getblocks"
  private[core] val getHeadersCommandName = "getheaders"
  private[core] val headersCommandName = "headers"
  private[core] val invCommandName = "inv"
  private[core] val getDataCommandName = "getdata"
  private[core] val memPoolCommandName = "mempool"
  private[core] val merkleBlockCommandName = "merkleblock"
  private[core] val notFoundCommandName = "notfound"
  private[core] val transactionCommandName = "tx"
  private[core] val addrCommandName = "addr"
  private[core] val addrV2CommandName = "addrv2"
  private[core] val sendAddrV2CommandName = "sendaddrv2"
  private[core] val feeFilterCommandName = "feefilter"
  private[core] val filterAddCommandName = "filteradd"
  private[core] val filterClearCommandName = "filterclear"
  private[core] val filterLoadCommandName = "filterload"
  private[core] val getAddrCommandName = "getaddr"
  private[core] val pingCommandName = "ping"
  private[core] val pongCommandName = "pong"
  private[core] val rejectCommandName = "reject"
  private[core] val sendHeadersCommandName = "sendheaders"
  private[core] val verAckCommandName = "verack"
  private[core] val versionCommandName = "version"
  private[core] val getCompactFiltersCommandName = "getcfilters"
  private[core] val compactFilterCommandName = "cfilter"
  private[core] val getCompactFilterHeadersCommandName = "getcfheaders"
  private[core] val compactFilterHeadersCommandName = "cfheaders"
  private[core] val getCompactFilterCheckpointCommandName = "getcfcheckpt"
  private[core] val compactFilterCheckpointCommandName = "cfcheckpt"

  /** Contains all the valid command names with their deserializer on the p2p protocol.
    * These commands all have the null bytes appended to the end of the string as
    * required by the network header specification.
    *
    * @see [[https://bitcoin.org/en/developer-reference#message-headers]]
    */
  val readers: Map[String, ByteVector => NetworkPayload] = Map(
    blockCommandName -> RawBlockMessageSerializer.read,
    getBlocksCommandName -> RawGetBlocksMessageSerializer.read,
    getHeadersCommandName -> RawGetHeadersMessageSerializer.read,
    getDataCommandName -> RawGetDataMessageSerializer.read,
    headersCommandName -> RawHeadersMessageSerializer.read,
    invCommandName -> RawInventoryMessageSerializer.read,
    memPoolCommandName -> { (_: ByteVector) =>
      MemPoolMessage
    },
    merkleBlockCommandName -> RawMerkleBlockMessageSerializer.read,
    notFoundCommandName -> RawNotFoundMessageSerializer.read,
    transactionCommandName -> RawTransactionMessageSerializer.read,
    addrCommandName -> RawAddrMessageSerializer.read,
    addrV2CommandName -> AddrV2Message.fromBytes,
    sendAddrV2CommandName -> { (_: ByteVector) =>
      SendAddrV2Message
    },
    feeFilterCommandName -> RawFeeFilterMessageSerializer.read,
    filterAddCommandName -> RawFilterAddMessageSerializer.read,
    filterClearCommandName -> { (_: ByteVector) =>
      FilterClearMessage
    },
    filterLoadCommandName -> RawFilterLoadMessageSerializer.read,
    getAddrCommandName -> { (_: ByteVector) =>
      GetAddrMessage
    },
    pingCommandName -> RawPingMessageSerializer.read,
    pongCommandName -> RawPongMessageSerializer.read,
    rejectCommandName -> RawRejectMessageSerializer.read,
    sendHeadersCommandName -> { (_: ByteVector) =>
      SendHeadersMessage
    },
    verAckCommandName -> { (_: ByteVector) =>
      VerAckMessage
    },
    versionCommandName -> RawVersionMessageSerializer.read,
    getCompactFiltersCommandName -> RawGetCompactFiltersMessageSerializer.read,
    compactFilterCommandName -> RawCompactFilterMessageSerializer.read,
    getCompactFilterHeadersCommandName -> RawGetCompactFilterHeadersMessageSerializer.read,
    compactFilterHeadersCommandName -> RawCompactFilterHeadersMessageSerializer.read,
    getCompactFilterCheckpointCommandName -> RawGetCompactFilterCheckpointMessageSerializer.read,
    compactFilterCheckpointCommandName -> RawCompactFilterCheckpointMessageSerializer.read
  )

  /** All command names for P2P messages */
  val commandNames: Vector[String] = readers.keys.toVector

  /** Parses a [[NetworkPayload]] from the given bytes using the [[NetworkHeader]]
    * to determine what type of [[NetworkPayload]] this is
    * @param networkHeader the header for the message on the p2p network
    * @param payloadBytes the payload corresponding to the header on the p2p network
    */
  def apply(
      networkHeader: NetworkHeader,
      payloadBytes: ByteVector): NetworkPayload = {
    //the commandName in the network header tells us what payload type this is
    val deserializer: ByteVector => NetworkPayload = readers(
      networkHeader.commandName)
    deserializer(payloadBytes)
  }

  /** Parses a [[NetworkPayload]] from the given hex using the [[NetworkHeader]]
    * to determine what type of [[NetworkPayload]] this is
    * @param networkHeader the header for the message on the p2p network
    * @param payloadHex the hexadecimal representation of the payload
    */
  def apply(
      networkHeader: NetworkHeader,
      payloadHex: String): NetworkPayload = {
    NetworkPayload(networkHeader, BytesUtil.decodeHex(payloadHex))
  }
}
