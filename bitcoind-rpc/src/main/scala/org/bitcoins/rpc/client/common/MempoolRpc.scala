package org.bitcoins.rpc.client.common

import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.jsonmodels.{
  GetMemPoolEntryResult,
  GetMemPoolInfoResult,
  GetMemPoolResult
}
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsBoolean, JsString}

import scala.concurrent.Future

trait MempoolRpc extends Client {

  def getMemPoolAncestors(
      txid: DoubleSha256DigestBE): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolAncestors(
      txid: DoubleSha256Digest): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolAncestors(txid.flip)
  }

  def getMemPoolAncestorsVerbose(txid: DoubleSha256DigestBE): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResult]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getMemPoolAncestorsVerbose(txid: DoubleSha256Digest): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolAncestorsVerbose(txid.flip)
  }

  def getMemPoolDescendants(
      txid: DoubleSha256DigestBE): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolDescendants(
      txid: DoubleSha256Digest): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolDescendants(txid.flip)
  }

  def getMemPoolDescendantsVerbose(txid: DoubleSha256DigestBE): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResult]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getMemPoolDescendantsVerbose(txid: DoubleSha256Digest): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolDescendantsVerbose(txid.flip)
  }

  def getMemPoolEntry(
      txid: DoubleSha256DigestBE): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResult]("getmempoolentry",
                                        List(JsString(txid.hex)))
  }

  def getMemPoolEntry(
      txid: DoubleSha256Digest): Future[GetMemPoolEntryResult] = {
    getMemPoolEntry(txid.flip)
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getRawMemPool: Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]]("getrawmempool",
                                               List(JsBoolean(false)))
  }

  def getRawMemPoolWithTransactions: Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResult]](
      "getrawmempool",
      List(JsBoolean(true)))
  }

  def saveMemPool(): Future[Unit] = {
    bitcoindCall[Unit]("savemempool")
  }

}
