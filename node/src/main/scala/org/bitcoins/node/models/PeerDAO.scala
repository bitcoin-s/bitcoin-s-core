package org.bitcoins.node.models

import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.api.tor.Socks5ProxyParams
import org.bitcoins.core.p2p.{AddrV2Message, ServiceIdentifier}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.node.config.NodeAppConfig
import scodec.bits.ByteVector
import slick.dbio.DBIOAction
import slick.lifted.ProvenShape

import java.net.{InetAddress, InetSocketAddress}
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class PeerDb(
    address: ByteVector,
    port: Int,
    lastSeen: Instant,
    firstSeen: Instant,
    networkId: Byte,
    serviceBytes: ByteVector
) {

  def inetSocketAddress: InetSocketAddress = {
    NetworkUtil.parseInetSocketAddress(address, port)
  }

  def peer(socks5ProxyParamsOpt: Option[Socks5ProxyParams]): Peer = {
    Peer.fromSocket(inetSocketAddress, socks5ProxyParamsOpt)
  }
}

case class PeerDAO()(implicit appConfig: NodeAppConfig, ec: ExecutionContext)
    extends CRUD[PeerDb, (ByteVector, Int)]
    with SlickUtil[PeerDb, (ByteVector, Int)] {

  import profile.api._
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: TableQuery[PeerTable] =
    TableQuery[PeerTable]

  override def createAll(ts: Vector[PeerDb]): Future[Vector[PeerDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[(ByteVector, Int)]
  ): Query[PeerTable, PeerDb, Seq] = {
    // from: https://stackoverflow.com/questions/26815913/how-to-do-or-filter-in-slick
    table.filter(r =>
      ids.map(i => r.address === i._1 && r.port === i._2).reduceLeft(_ || _))
  }

  override protected def findAll(
      ts: Vector[PeerDb]
  ): Query[Table[PeerDb], PeerDb, Seq] =
    findByPrimaryKeys(ts.map(t => (t.address, t.port)))

  def deleteByKey(address: String): Future[Int] = {
    val bytes = ByteVector(address.getBytes)
    val q = table.filter(_.address === bytes)
    safeDatabase.run(q.delete)
  }

  /** returns only non onion addresses if tor is disabled, all otherwise */
  def findAllWithTorFilter(torEnabled: Boolean): Future[Vector[PeerDb]] = {
    val q = table.filterIf(!torEnabled)(
      _.networkId =!= AddrV2Message.TOR_V3_NETWORK_BYTE
    )
    safeDatabase.run(q.result).map(_.toVector)
  }

  def upsertPeer(
      address: ByteVector,
      port: Int,
      networkId: Byte,
      serviceIdentifier: ServiceIdentifier
  ): Future[PeerDb] = {
    val lastSeen: Instant = Instant.now
    val existingF = read((address, port))
    existingF.flatMap {
      case Some(value) =>
        upsert(
          PeerDb(
            address,
            port,
            firstSeen = value.firstSeen,
            lastSeen = lastSeen,
            networkId = networkId,
            serviceBytes = serviceIdentifier.bytes
          )
        )
      case None =>
        upsert(
          PeerDb(
            address,
            port,
            firstSeen = Instant.now,
            lastSeen = lastSeen,
            networkId = networkId,
            serviceBytes = serviceIdentifier.bytes
          )
        )
    }
  }

  def updateLastSeenTime(peer: Peer): Future[Option[PeerDb]] = {
    val address = PeerDAOHelper.getAddrBytes(peer)
    val port = peer.port
    val action = findByPrimaryKey((address, port)).result.headOption
    val updatedLastSeenA = action.flatMap {
      case Some(peerDb) =>
        val now = Instant.now()
        updateAction(peerDb.copy(lastSeen = now))
          .map(Some(_))
      case None => DBIOAction.successful(None)
    }

    for {
      result <- safeDatabase.run(updatedLastSeenA)
    } yield result
  }

  class PeerTable(tag: Tag) extends Table[PeerDb](tag, schemaName, "peers") {

    def address: Rep[ByteVector] = column("address")

    def port: Rep[Int] = column("port")

    def lastSeen: Rep[Instant] = column("last_seen")

    def firstSeen: Rep[Instant] = column("first_seen")

    def networkId: Rep[Byte] = column("network_id")

    def serviceBytes: Rep[ByteVector] = column("service_bytes")

    def pkPeers = primaryKey("pk_peers", (address, port))

    def * : ProvenShape[PeerDb] =
      (address, port, lastSeen, firstSeen, networkId, serviceBytes).<>(
        PeerDb.apply,
        PeerDb.unapply
      )
  }
}

object PeerDAOHelper {

  def getAddrBytes(peer: Peer): ByteVector = {
    val addrBytes =
      if (peer.socket.getHostString.contains(".onion"))
        NetworkUtil.torV3AddressToBytes(peer.socket.getHostString)
      else
        InetAddress.getByName(peer.socket.getHostString).getAddress
    ByteVector(addrBytes)
  }
}
