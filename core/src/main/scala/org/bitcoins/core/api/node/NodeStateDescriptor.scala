package org.bitcoins.core.api.node

import org.bitcoins.crypto.StringFactory

sealed abstract class NodeStateDescriptorType

object NodeStateDescriptorType extends StringFactory[NodeStateDescriptorType] {

  case object WalletName extends NodeStateDescriptorType

  val all: Vector[NodeStateDescriptorType] = Vector(WalletName)

  override def fromStringOpt(str: String): Option[NodeStateDescriptorType] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }

  override def fromString(string: String): NodeStateDescriptorType = {
    fromStringOpt(string) match {
      case Some(state) => state
      case None =>
        sys.error(s"Could not find NodeStateDescriptorType for string=$string")
    }
  }
}

sealed abstract class NodeStateDescriptor {
  def descriptorType: NodeStateDescriptorType
}

sealed trait NodeStateDescriptorFactory[T <: NodeStateDescriptor]
    extends StringFactory[T] {
  def tpe: NodeStateDescriptorType
}

object NodeStateDescriptor extends StringFactory[NodeStateDescriptor] {

  val all: Vector[StringFactory[NodeStateDescriptor]] =
    Vector(WalletNameDescriptor)

  override def fromString(string: String): NodeStateDescriptor = {
    all.find(f => f.fromStringT(string).isSuccess) match {
      case Some(factory) => factory.fromString(string)
      case None =>
        sys.error(s"Could not find NodeStateDescriptor for string=$string")
    }
  }
}

case class WalletNameDescriptor(walletName: String)
    extends NodeStateDescriptor {

  override val descriptorType: NodeStateDescriptorType =
    NodeStateDescriptorType.WalletName
  override val toString: String = walletName
}

object WalletNameDescriptor
    extends NodeStateDescriptorFactory[WalletNameDescriptor] {

  override val tpe: NodeStateDescriptorType =
    NodeStateDescriptorType.WalletName

  override def fromString(string: String): WalletNameDescriptor = {
    WalletNameDescriptor(string)
  }
}
