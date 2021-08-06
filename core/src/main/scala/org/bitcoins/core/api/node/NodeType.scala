package org.bitcoins.core.api.node

import org.bitcoins.crypto.StringFactory

sealed abstract class NodeType {
  def shortName: String
}

sealed abstract class InternalImplementationNodeType extends NodeType
sealed abstract class ExternalImplementationNodeType extends NodeType

object NodeType extends StringFactory[NodeType] {

  case object FullNode extends InternalImplementationNodeType {
    override def shortName: String = "full"
  }

  case object NeutrinoNode extends InternalImplementationNodeType {
    override def shortName: String = "neutrino"
  }

  case object SpvNode extends InternalImplementationNodeType {
    override def shortName: String = "spv"
  }

  case object BitcoindBackend extends ExternalImplementationNodeType {
    override def shortName: String = "bitcoind"
  }

  val all: Vector[NodeType] =
    Vector(FullNode, NeutrinoNode, BitcoindBackend)

  override def fromStringOpt(str: String): Option[NodeType] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase) match {
      case Some(value) => Some(value)
      case None =>
        all.find(state => str.toLowerCase() == state.shortName.toLowerCase)
    }
  }

  override def fromString(string: String): NodeType = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find a NodeType for string=$string")
    }
  }
}
