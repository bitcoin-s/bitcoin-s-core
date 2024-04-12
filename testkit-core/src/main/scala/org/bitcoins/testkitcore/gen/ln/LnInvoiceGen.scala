package org.bitcoins.testkitcore.gen.ln

import org.bitcoins.core.number.{UInt64, UInt8}
import org.bitcoins.core.protocol.ln.LnTag.NodeIdTag
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.testkitcore.gen._
import org.scalacheck.Gen

sealed abstract class LnInvoiceGen {

  /** Generates a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart LnHumanReadablePart]]
    * that does not contain a amount
    * @return
    */
  def lnHrpNoAmt: Gen[LnHumanReadablePart] = {
    ChainParamsGenerator.lnNetworkParams.flatMap { lnParam =>
      LnHumanReadablePart.fromLnParams(lnParam)
    }
  }

  /** Generates a [[org.bitcoins.core.protocol.ln.LnHumanReadablePart LnHumanReadablePart]]
    * with an amount encoded
    */
  def lnHrpAmt: Gen[LnHumanReadablePart] = {
    ChainParamsGenerator.lnNetworkParams.flatMap { lnParam =>
      LnCurrencyUnitGen.realisticLnInvoice.map { lcu =>
        LnHumanReadablePart.fromParamsAmount(network = lnParam,
                                             amount = Some(lcu))
      }
    }
  }

  def lnHrp: Gen[LnHumanReadablePart] = {
    Gen.oneOf(lnHrpAmt, lnHrpNoAmt)
  }

  def nodeId: Gen[NodeId] = {
    CryptoGenerators.publicKey.map(NodeId(_))
  }

  def paymentHashTag: Gen[LnTag.PaymentHashTag] = {
    CryptoGenerators.sha256Digest.map { hash =>
      LnTag.PaymentHashTag(hash)
    }
  }

  def secret: Gen[LnTag.SecretTag] = {
    CryptoGenerators.sha256Digest.map { hash =>
      LnTag.SecretTag(PaymentSecret.fromBytes(hash.bytes))
    }
  }

  def descriptionTag: Gen[LnTag.DescriptionTag] = {
    StringGenerators.genString.map { description =>
      LnTag.DescriptionTag(description)
    }
  }

  def descriptionHashTag: Gen[LnTag.DescriptionHashTag] = {
    descriptionTag.map { desc =>
      desc.descriptionHashTag
    }
  }

  def descriptionOrDescriptionHashTag: Gen[
    Either[LnTag.DescriptionTag, LnTag.DescriptionHashTag]] = {
    if (scala.util.Random.nextBoolean()) {
      descriptionTag.map(Left(_))
    } else {
      descriptionHashTag.map(Right(_))
    }
  }

  def expiryTime: Gen[LnTag.ExpiryTimeTag] = {
    NumberGenerator.uInt32s.map { u32 =>
      LnTag.ExpiryTimeTag(u32)
    }
  }

  def cltvExpiry: Gen[LnTag.MinFinalCltvExpiry] = {
    NumberGenerator.uInt32s.map { u32 =>
      LnTag.MinFinalCltvExpiry(u32)

    }
  }

  def fallbackAddress: Gen[LnTag.FallbackAddressTag] = {
    AddressGenerator.address.map { addr =>
      LnTag.FallbackAddressTag(addr)
    }
  }

  def nodeIdTag(nodeId: NodeId): Gen[LnTag.NodeIdTag] = {
    LnTag.NodeIdTag(nodeId)
  }

  def routingInfo: Gen[LnTag.RoutingInfo] = {
    LnRouteGen.routes
      .map(rs => LnTag.RoutingInfo(rs))
  }

  def mandatoryTags: Gen[LnTaggedFields] = {
    for {
      paymentHash <- paymentHashTag
      descOrHashTag <- descriptionOrDescriptionHashTag
    } yield LnTaggedFields(
      paymentHash = paymentHash,
      secret = None,
      descriptionOrHash = descOrHashTag,
      expiryTime = None,
      cltvExpiry = None,
      fallbackAddress = None,
      nodeId = None,
      routingInfo = None,
      features = None
    )
  }

  def optionalTags(nodeIdOpt: Option[NodeId]): Gen[LnTaggedFields] = {
    for {
      paymentHash <- paymentHashTag
      descOrHashTag <- descriptionOrDescriptionHashTag
      secret <- Gen.option(secret)
      //optional fields
      expiryTime <- Gen.option(expiryTime)
      cltvExpiry <- Gen.option(cltvExpiry)
      fallbackAddress <- Gen.option(fallbackAddress)
      routes <- Gen.option(routingInfo)
    } yield LnTaggedFields(
      paymentHash = paymentHash,
      secret = secret,
      descriptionOrHash = descOrHashTag,
      expiryTime = expiryTime,
      cltvExpiry = cltvExpiry,
      fallbackAddress = fallbackAddress,
      nodeId = nodeIdOpt.map(NodeIdTag(_)),
      routingInfo = routes,
      features = None
    )
  }

  def allTags(nodeIdOpt: Option[NodeId]): Gen[LnTaggedFields] = {
    for {
      paymentHash <- paymentHashTag
      descOrHashTag <- descriptionOrDescriptionHashTag
      secret <- secret
      //optional fields
      expiryTime <- expiryTime
      cltvExpiry <- cltvExpiry
      fallbackAddress <- fallbackAddress
      routes <- routingInfo
    } yield LnTaggedFields(
      paymentHash = paymentHash,
      secret = Some(secret),
      descriptionOrHash = descOrHashTag,
      expiryTime = Some(expiryTime),
      cltvExpiry = Some(cltvExpiry),
      fallbackAddress = Some(fallbackAddress),
      nodeId = nodeIdOpt.map(NodeIdTag(_)),
      routingInfo = Some(routes),
      features = None
    )
  }

  /** Generated a tagged fields with an explicit
    * [[org.bitcoins.core.protocol.ln.LnTag.NodeIdTag LnTag.NodeIdTag]]
    */
  def taggedFields(nodeIdOpt: Option[NodeId]): Gen[LnTaggedFields] = {
    Gen.oneOf(allTags(nodeIdOpt), mandatoryTags, optionalTags(nodeIdOpt))
  }

  def signatureVersion: Gen[UInt8] = {
    Gen.choose(0, 3).map(UInt8(_))
  }

  def lnInvoiceSignature: Gen[LnInvoiceSignature] =
    for {
      sig <- CryptoGenerators.digitalSignature
      version <- signatureVersion
    } yield LnInvoiceSignature(version, sig)

  def invoiceTimestamp: Gen[UInt64] = {
    Gen.choose(0L, LnInvoice.MAX_TIMESTAMP_U64.toLong).map(UInt64(_))
  }

  def lnInvoice(privateKey: ECPrivateKey): Gen[LnInvoice] = {
    for {
      hrp <- lnHrp
      //timestamp is 35 bits according to BOLT11
      timestamp <- invoiceTimestamp
      nodeIdOpt <- Gen.option(NodeId(privateKey.publicKey))
      tags <- taggedFields(nodeIdOpt)
    } yield {
      val signature = LnInvoice.buildLnInvoiceSignature(
        hrp = hrp,
        timestamp = timestamp,
        lnTags = tags,
        privateKey = privateKey
      )

      LnInvoice(hrp = hrp,
                timestamp = timestamp,
                lnTags = tags,
                signature = signature)
    }
  }

  def lnInvoice(tags: LnTaggedFields): Gen[LnInvoice] = {
    for {
      privateKey <- CryptoGenerators.privateKey
      hrp <- lnHrp
      //timestamp is 35 bits according to BOLT11
      timestamp <- invoiceTimestamp
    } yield {
      val signature = LnInvoice.buildLnInvoiceSignature(
        hrp = hrp,
        timestamp = timestamp,
        lnTags = tags,
        privateKey = privateKey
      )

      LnInvoice(hrp = hrp,
                timestamp = timestamp,
                lnTags = tags,
                signature = signature)
    }
  }

  def lnInvoice: Gen[LnInvoice] = {
    CryptoGenerators.privateKey.flatMap { p =>
      val i = lnInvoice(p)
      i
    }
  }
}

object LnInvoiceGen extends LnInvoiceGen
