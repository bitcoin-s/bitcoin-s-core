package org.bitcoins.db.util

import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.db.models.MasterXPubDAO

import scala.concurrent.{ExecutionContext, Future}

object MasterXPubUtil {

  /** Checks our database xpub is the same as our key manager xpub
    * @throws RuntimeExceptions
    *   if the xpubs do not match
    */
  def checkMasterXPub(xpub: ExtPublicKey, masterXPubDAO: MasterXPubDAO)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    val validateF = masterXPubDAO.validate(xpub)
    validateF.map(_ => true)
  }
}
