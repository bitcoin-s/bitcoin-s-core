package org.bitcoins.core.wallet.utxo

import org.bitcoins.crypto.StringFactory

/** An AddressTagNames that is native to Bitcoin-S.
  * InternalAddressTagNames are still usable when using Bitcoin-S
  * as a dependency
  */
sealed trait InternalAddressTagName extends AddressTagName

/** An AddressTagType that is native to Bitcoin-S.
  * InternalAddressTagTypes are still usable when using Bitcoin-S
  * as a dependency
  */
sealed trait InternalAddressTagType extends AddressTagType

/** An AddressTag that is native to Bitcoin-S.
  * InternalAddressTags are still usable when using Bitcoin-S
  * as a dependency
  */
sealed trait InternalAddressTag extends AddressTag

/** An unknown address tag name, most likely an internal representation of an [[ExternalAddressTagName]] */
case class UnknownAddressTagName(name: String) extends InternalAddressTagName {
  require(InternalAddressTagName.fromStringOpt(name).isEmpty,
          s"This tag name is already defined, got $name")
}

/** An unknown address tag type, most likely an internal representation of an [[ExternalAddressTagType]] */
case class UnknownAddressTagType(typeName: String)
    extends InternalAddressTagType {
  require(InternalAddressTagType.fromStringOpt(typeName).isEmpty,
          s"This tag type is already defined, got $typeName")
}

/** An address tag without an unknown type, most likely an internal representation of an [[ExternalAddressTag]] */
case class UnknownAddressTag(tagName: AddressTagName, tagType: AddressTagType)
    extends InternalAddressTag

object UnknownAddressTag {

  def apply(tagName: String, tagType: String): UnknownAddressTag =
    UnknownAddressTag(UnknownAddressTagName(tagName),
                      UnknownAddressTagType(tagType))

  def apply(tagName: String, tagType: AddressTagType): UnknownAddressTag =
    UnknownAddressTag(UnknownAddressTagName(tagName), tagType)

  def apply(tagName: AddressTagName, tagType: String): UnknownAddressTag =
    UnknownAddressTag(tagName, UnknownAddressTagType(tagType))
}

object InternalAddressTagName extends StringFactory[InternalAddressTagName] {

  val all: Vector[InternalAddressTagName] = StorageLocationTag.tagNames

  override def fromStringOpt(string: String): Option[InternalAddressTagName] =
    all.find(_.name.toLowerCase == string.toLowerCase)

  override def fromString(string: String): InternalAddressTagName =
    fromStringOpt(string).getOrElse(UnknownAddressTagName(string))
}

object InternalAddressTagType extends StringFactory[InternalAddressTagType] {

  val all: Seq[InternalAddressTagType] =
    Vector(StorageLocationTagType, AddressLabelTagType)

  override def fromStringOpt(string: String): Option[InternalAddressTagType] =
    all.find(_.typeName.toLowerCase == string.toLowerCase)

  override def fromString(string: String): InternalAddressTagType =
    fromStringOpt(string).getOrElse(UnknownAddressTagType(string))
}

object InternalAddressTag {

  def apply(
      tagName: AddressTagName,
      tagType: AddressTagType): InternalAddressTag = {
    tagType match {
      case StorageLocationTagType =>
        tagName match {
          case StorageLocationTag.HotStorageName =>
            StorageLocationTag.HotStorage
          case StorageLocationTag.ColdStorageName =>
            StorageLocationTag.ColdStorage
          case StorageLocationTag.DeepColdStorageName =>
            StorageLocationTag.DeepColdStorage
          case unknownName: UnknownAddressTagName =>
            UnknownAddressTag(unknownName, StorageLocationTagType)
        }
      case AddressLabelTagType =>
        AddressLabelTag(tagName.name)
      case unknownType: UnknownAddressTagType =>
        UnknownAddressTag(tagName, unknownType)
    }
  }
}

object StorageLocationTagType extends InternalAddressTagType {
  override val typeName: String = "StorageLocationTag"
}

/** Storage Location of the private keys associated with the address */
sealed trait StorageLocationTag extends InternalAddressTag {
  override val tagType: AddressTagType = StorageLocationTagType
}

object StorageLocationTag extends AddressTagFactory[StorageLocationTag] {

  override val tagType: InternalAddressTagType = StorageLocationTagType

  override val tagNames: Vector[InternalAddressTagName] =
    Vector(HotStorageName, ColdStorageName, DeepColdStorageName)

  // Tag Names
  case object HotStorageName extends InternalAddressTagName {
    override def name: String = "HotStorage"
  }

  case object ColdStorageName extends InternalAddressTagName {
    override def name: String = "ColdStorage"
  }

  case object DeepColdStorageName extends InternalAddressTagName {
    override def name: String = "DeepColdStorage"
  }

  /** Keys stored on a computer connected to the internet */
  case object HotStorage extends StorageLocationTag {
    override val tagName: AddressTagName = HotStorageName
  }

  /** Keys stored on a hardware wallet or other offline device */
  case object ColdStorage extends StorageLocationTag {
    override val tagName: AddressTagName = ColdStorageName
  }

  /** Keys stored on a hardware wallet or other offline device locked in a safe in a distant location */
  case object DeepColdStorage extends StorageLocationTag {
    override val tagName: AddressTagName = DeepColdStorageName
  }

  override val all: Vector[StorageLocationTag] =
    Vector(HotStorage, ColdStorage, DeepColdStorage)
}

object AddressLabelTagType extends InternalAddressTagType {
  override val typeName: String = "Label"
}

case class AddressLabelTagName(name: String) extends InternalAddressTagName

/** Used for generic address labeling, generally labels should be
  * provided by the user so they keep track which parties are aware
  * of which addresses
  */
case class AddressLabelTag(name: String) extends InternalAddressTag {
  override val tagType: AddressTagType = AddressLabelTagType

  override val tagName: AddressTagName = AddressLabelTagName(name)
}
