package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.CRUD

import scala.concurrent.Future

/** Helper methods for querying by dlcId whne the dlcId is the primary key on
  * the table
  */
trait DLCIdDaoUtil[T, PrimaryKeyType] { self: CRUD[T, PrimaryKeyType] =>

  def findByDLCIdAction(
      dlcId: Sha256Digest): profile.api.DBIOAction[Option[
                                                     T
                                                   ],
                                                   profile.api.NoStream,
                                                   profile.api.Effect.Read] = {
    findByDLCIdsAction(Vector(dlcId))
      .map(_.headOption)
  }

  def findByDLCIdsAction(
      dlcId: Vector[Sha256Digest]
  ): profile.api.DBIOAction[Vector[
                              T
                            ],
                            profile.api.NoStream,
                            profile.api.Effect.Read]

  def findByDLCId(dlcId: Sha256Digest): Future[Option[T]] = {
    safeDatabase.run(findByDLCIdAction(dlcId))
  }

  def deleteByDLCIdAction(
      dlcId: Sha256Digest
  ): profile.api.DBIOAction[Int, profile.api.NoStream, profile.api.Effect.Write]

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    safeDatabase.run(deleteByDLCIdAction(dlcId))
  }
}

/** Helper methods for querying by dlcId when the dlcId is not a primary key on
  * the table
  */
trait DLCIdDaoUtilNoPK[T] { self: CRUD[T, _] =>

  def findByDLCIdAction(
      dlcId: Sha256Digest): profile.api.DBIOAction[Vector[
                                                     T
                                                   ],
                                                   profile.api.NoStream,
                                                   profile.api.Effect.Read]

  def findByDLCId(dlcId: Sha256Digest): Future[Vector[T]] = {
    safeDatabase.runVec(findByDLCIdAction(dlcId))
  }

  def deleteByDLCIdAction(
      dlcId: Sha256Digest
  ): profile.api.DBIOAction[Int, profile.api.NoStream, profile.api.Effect.Write]

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    safeDatabase.run(deleteByDLCIdAction(dlcId))
  }
}
