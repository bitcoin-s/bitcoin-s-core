package org.bitcoins.db

import org.bitcoins.crypto.StringFactory

sealed abstract class DatabaseDriver {
  def shortName: String
}

object DatabaseDriver extends StringFactory[DatabaseDriver] {

  case object SQLite extends DatabaseDriver {
    override def shortName: String = "sqlite"
  }

  case object PostgreSQL extends DatabaseDriver {
    override def shortName: String = "postgres"
  }

  val all: Vector[DatabaseDriver] = Vector(SQLite, PostgreSQL)

  override def fromStringOpt(str: String): Option[DatabaseDriver] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase) match {
      case Some(value) => Some(value)
      case None =>
        all.find(state => str.toLowerCase() == state.shortName.toLowerCase)
    }
  }

  override def fromString(string: String): DatabaseDriver = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find a DatabaseDriver for string=$string")
    }
  }
}
