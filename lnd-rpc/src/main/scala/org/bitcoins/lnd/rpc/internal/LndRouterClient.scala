package org.bitcoins.lnd.rpc.internal

import lnrpc.Failure.FailureCode.INCORRECT_OR_UNKNOWN_PAYMENT_DETAILS
import lnrpc.{
  HTLCAttempt,
  MPPRecord,
  QueryRoutesRequest,
  QueryRoutesResponse,
  Route
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto._
import org.bitcoins.lnd.rpc.LndRpcClient
import routerrpc._

import scala.concurrent.Future

trait LndRouterClient { self: LndRpcClient =>

  def queryRoutes(
      amount: Satoshis,
      node: NodeId): Future[QueryRoutesResponse] = {
    val request =
      QueryRoutesRequest(pubKey = node.pubKey.hex,
                         amt = amount.toLong,
                         finalCltvDelta = 40,
                         useMissionControl = true)

    queryRoutes(request)
  }

  def queryRoutes(request: QueryRoutesRequest): Future[QueryRoutesResponse] = {
    logger.trace("lnd calling queryroutes")

    lnd.queryRoutes(request)
  }

  def probe(invoice: LnInvoice): Future[Vector[Route]] = {
    val amount = invoice.amount.map(_.toSatoshis).getOrElse(Satoshis.zero)
    probe(amount, invoice.nodeId)
  }

  def sendToRoute(hash: Sha256Digest, route: Route): Future[HTLCAttempt] = {
    val request = SendToRouteRequest(paymentHash = hash.bytes, Some(route))

    sendToRoute(request)
  }

  def sendToRoute(request: SendToRouteRequest): Future[HTLCAttempt] = {
    logger.trace("lnd calling sendtoroute")

    router.sendToRouteV2(request)
  }

  def probe(amount: Satoshis, node: NodeId): Future[Vector[Route]] = {
    queryRoutes(amount, node).map(_.routes).flatMap { routes =>
      val fs = routes.toVector.map { route =>
        val fakeHash = CryptoUtil.sha256(ECPrivateKey.freshPrivateKey.bytes)
        sendToRoute(fakeHash, route).map(t => (route, t))
      }

      Future.sequence(fs).map { results =>
        results
          .filter(
            _._2.failure.exists(_.code == INCORRECT_OR_UNKNOWN_PAYMENT_DETAILS))
          .map(_._1)
      }
    }
  }

  def sendToRoute(invoice: LnInvoice, route: Route): Future[HTLCAttempt] = {
    val milliSatoshis =
      invoice.amount.map(MilliSatoshis(_)).getOrElse(MilliSatoshis.zero)
    val last = route.hops.last
    val secret = invoice.lnTags.secret.get.secret.bytes
    val mpp =
      MPPRecord(paymentAddr = secret, totalAmtMsat = milliSatoshis.toLong)
    val update = last.copy(mppRecord = Some(mpp), tlvPayload = true)
    val updatedHops = route.hops.init :+ update
    val updatedRoute = route.copy(hops = updatedHops)

    val request =
      SendToRouteRequest(paymentHash = invoice.lnTags.paymentHash.bytes,
                         route = Some(updatedRoute))

    sendToRoute(request)
  }

  def probeAndPay(invoice: LnInvoice): Future[Option[HTLCAttempt]] = {
    probe(invoice).flatMap { routes =>
      val sorted = routes.sortBy(_.totalFeesMsat)
      attemptToPayRoutes(invoice, sorted)
    }
  }

  def attemptToPayRoutes(
      invoice: LnInvoice,
      routes: Vector[Route]): Future[Option[HTLCAttempt]] = {
    val init: Option[HTLCAttempt] = None
    FutureUtil.foldLeftAsync(init, routes) { case (ret, route) =>
      ret match {
        case Some(value) =>
          value.failure match {
            case Some(_) => sendToRoute(invoice, route).map(Some(_))
            case None    => Future.successful(Some(value))
          }
        case None => sendToRoute(invoice, route).map(Some(_))
      }
    }
  }
}
