package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels._
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{
  AddressType,
  LabelPurpose
}
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.jsonmodels.clightning.CLightningJsonModels._
import org.bitcoins.commons.jsonmodels.eclair._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.config._
import org.bitcoins.core.currency._
import org.bitcoins.core.number._
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.channel._
import org.bitcoins.core.protocol.ln.currency._
import org.bitcoins.core.protocol.ln.fee.FeeProportionalMillionths
import org.bitcoins.core.protocol.ln.node.{Feature, FeatureSupport, NodeId}
import org.bitcoins.core.protocol.ln.routing.{ChannelRoute, NodeRoute, Route}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementV0TLV,
  OracleAttestmentV0TLV
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{
  Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.wallet.fee._
import org.bitcoins.crypto._
import play.api.libs.json._
import ujson.{Num, Str, Value}
import scodec.bits.ByteVector

import java.io.File
import java.net._
import java.nio.file.Path
import java.time._
import java.util.UUID
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object JsonReaders {

  /** Tries to prase the provided JSON into a map with keys of
    * type `K` and values of type `V`
    */
  def mapReads[K, V](js: JsValue)(implicit
      readsK: Reads[K],
      readsV: Reads[V]): JsResult[Map[K, V]] = {
    js.validate[JsObject].flatMap { jsObj =>
      val jsResults: scala.collection.Seq[(JsResult[K], JsResult[V])] =
        jsObj.fields.map { case (key, value) =>
          JsString(key).validate[K] -> value.validate[V]
        }

      val allErrors: scala.collection.Seq[(
          JsPath,
          scala.collection.Seq[JsonValidationError])] =
        jsResults.collect {
          case (JsError(keyErrors), _)   => keyErrors
          case (_, JsError(valueErrors)) => valueErrors
        }.flatten

      if (allErrors.nonEmpty) {
        JsError(allErrors)
      } else {
        JsSuccess(jsResults.collect { case (JsSuccess(k, _), JsSuccess(v, _)) =>
          k -> v
        }.toMap)
      }
    }
  }

  implicit object URLReads extends Reads[URL] {

    override def reads(json: JsValue): JsResult[URL] =
      SerializerUtil.processJsString[URL](str => new URI(str).toURL)(json)
  }

  implicit object ZonedDateTimeReads extends Reads[ZonedDateTime] {

    override def reads(json: JsValue): JsResult[ZonedDateTime] =
      SerializerUtil.processJsNumberBigInt[ZonedDateTime](bigInt =>
        ZonedDateTime.ofInstant(Instant.ofEpochSecond(bigInt.toLong),
                                ZoneOffset.UTC))(json)
  }

  implicit object LocalDateTimeReads extends Reads[LocalDateTime] {

    override def reads(json: JsValue): JsResult[LocalDateTime] =
      SerializerUtil.processJsNumberBigInt[LocalDateTime](bigInt =>
        LocalDateTime.ofInstant(Instant.ofEpochSecond(bigInt.toLong),
                                ZoneId.systemDefault()))(json)
  }

  implicit object BigIntReads extends Reads[BigInt] {

    override def reads(json: JsValue): JsResult[BigInt] =
      SerializerUtil.processJsNumber[BigInt](_.toBigInt)(json)
  }

  implicit object Sha256DigestReads extends Reads[Sha256Digest] {

    override def reads(json: JsValue): JsResult[Sha256Digest] =
      SerializerUtil.processJsString[Sha256Digest](Sha256Digest.fromHex)(json)
  }

  implicit object RipeMd160DigestReads extends Reads[RipeMd160Digest] {

    override def reads(json: JsValue): JsResult[RipeMd160Digest] =
      SerializerUtil.processJsString[RipeMd160Digest](RipeMd160Digest.fromHex)(
        json)
  }

  implicit object RipeMd160DigestBEReads extends Reads[RipeMd160DigestBE] {

    override def reads(json: JsValue): JsResult[RipeMd160DigestBE] =
      SerializerUtil.processJsString[RipeMd160DigestBE](
        RipeMd160DigestBE.fromHex)(json)
  }

  implicit object DoubleSha256DigestReads extends Reads[DoubleSha256Digest] {

    override def reads(json: JsValue): JsResult[DoubleSha256Digest] =
      SerializerUtil.processJsString[DoubleSha256Digest](
        DoubleSha256Digest.fromHex)(json)
  }

  implicit object DoubleSha256DigestBEReads
      extends Reads[DoubleSha256DigestBE] {

    override def reads(json: JsValue): JsResult[DoubleSha256DigestBE] =
      SerializerUtil.processJsString[DoubleSha256DigestBE](
        DoubleSha256DigestBE.fromHex)(json)
  }

  implicit object BitcoinsReads extends Reads[Bitcoins] {

    override def reads(json: JsValue): JsResult[Bitcoins] =
      SerializerUtil.processJsNumber[Bitcoins](Bitcoins(_))(json)
  }

  implicit object CurrencyUnitReads extends Reads[CurrencyUnit] {

    override def reads(json: JsValue): JsResult[CurrencyUnit] =
      SerializerUtil.processJsNumber[CurrencyUnit](num =>
        Satoshis(num.toBigInt))(json)
  }

  implicit object SatoshisReads extends Reads[Satoshis] {

    override def reads(json: JsValue): JsResult[Satoshis] =
      SerializerUtil.processJsNumber[Satoshis](num => Satoshis(num.toBigInt))(
        json)
  }

  implicit object BlockHeaderReads extends Reads[BlockHeader] {

    override def reads(json: JsValue): JsResult[BlockHeader] =
      SerializerUtil.processJsString[BlockHeader](BlockHeader.fromHex)(json)
  }

  implicit object UInt16Reads extends Reads[UInt16] {

    override def reads(json: JsValue): JsResult[UInt16] =
      json match {
        case JsNumber(n) =>
          n.toBigIntExact match {
            case Some(num) => JsSuccess(UInt16(num))
            case None      => SerializerUtil.buildErrorMsg("UInt16", n)
          }
        case JsString(s) => JsSuccess(UInt16.fromHex(s))
        case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsnumber", err)
      }
  }

  implicit object Int32Reads extends Reads[Int32] {

    override def reads(json: JsValue): JsResult[Int32] =
      json match {
        case JsNumber(n) =>
          n.toBigIntExact match {
            case Some(num) => JsSuccess(Int32(num))
            case None      => SerializerUtil.buildErrorMsg("Int32", n)
          }
        case JsString(s) => JsSuccess(Int32.fromHex(s))
        case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsnumber", err)
      }
  }

  implicit object UInt32Reads extends Reads[UInt32] {

    override def reads(json: JsValue): JsResult[UInt32] =
      json match {
        case JsNumber(n) =>
          n.toBigIntExact match {
            case Some(num) =>
              if (num >= 0) {
                JsSuccess(UInt32(num))
              } else {
                SerializerUtil.buildErrorMsg("positive_value", num)
              }
            case None => SerializerUtil.buildErrorMsg("UInt32", n)
          }
        case JsString(s) => JsSuccess(UInt32.fromHex(s))
        case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsnumber", err)
      }
  }

  implicit object UInt64Reads extends Reads[UInt64] {

    override def reads(json: JsValue): JsResult[UInt64] =
      json match {
        case JsNumber(n) =>
          n.toBigIntExact match {
            case Some(num) =>
              if (num >= 0) {
                JsSuccess(UInt64(num))
              } else {
                SerializerUtil.buildErrorMsg("positive_value", num)
              }
            case None => SerializerUtil.buildErrorMsg("UInt32", n)
          }
        case JsString(s) => JsSuccess(UInt64.fromHex(s))
        case err @ (JsNull | _: JsBoolean | _: JsArray | _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsnumber", err)
      }
  }

  implicit object AddressTypeReads extends Reads[AddressType] {

    override def reads(json: JsValue): JsResult[AddressType] =
      SerializerUtil.processJsStringOpt(AddressType.fromStringOpt)(json)
  }

  implicit object LabelPurposeReads extends Reads[LabelPurpose] {

    override def reads(json: JsValue): JsResult[LabelPurpose] =
      json match {
        case JsString("send")    => JsSuccess(LabelPurpose.Send)
        case JsString("receive") => JsSuccess(LabelPurpose.Receive)
        // TODO better error message?
        case err =>
          SerializerUtil.buildErrorMsg(expected = "send or receive", err)
      }
  }

  implicit object WitnessVersionReads extends Reads[WitnessVersion] {

    override def reads(json: JsValue): JsResult[WitnessVersion] =
      json match {
        case JsNumber(num) if num == 0 => JsSuccess(WitnessVersion0)
        case JsNumber(num) if num != 0 =>
          SerializerUtil.buildErrorMsg("Expected witness_version 0", num)
        case err =>
          SerializerUtil.buildErrorMsg("Expected numerical witness_version",
                                       err)
      }
  }

  implicit object AddressReads extends Reads[Address] {

    override def reads(json: JsValue): JsResult[Address] =
      json match {
        case JsString(s) =>
          Address.fromStringT(s) match {
            case Success(address) => JsSuccess(address)
            case Failure(err) =>
              SerializerUtil.buildErrorMsg("address", err)
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsstring", err)
      }
  }

  implicit object BitcoinNetworkReads extends Reads[BitcoinNetwork] {

    override def reads(json: JsValue): JsResult[BitcoinNetwork] =
      SerializerUtil.processJsString(BitcoinNetworks.fromString)(json)

  }

  // Errors for Unit return types are caught in RpcClient::checkUnit
  implicit object UnitReads extends Reads[Unit] {
    override def reads(json: JsValue): JsResult[Unit] = JsSuccess(())
  }

  implicit object InetAddressReads extends Reads[InetAddress] {

    override def reads(json: JsValue): JsResult[InetAddress] =
      SerializerUtil.processJsString[InetAddress](InetAddress.getByName)(json)
  }

  implicit object ECDigitalSignatureReads extends Reads[ECDigitalSignature] {

    override def reads(json: JsValue): JsResult[ECDigitalSignature] = {
      SerializerUtil.processJsString(ECDigitalSignature.fromHex)(json)
    }
  }

  implicit object ScriptPubKeyReads extends Reads[ScriptPubKey] {

    override def reads(json: JsValue): JsResult[ScriptPubKey] =
      SerializerUtil.processJsString[ScriptPubKey](ScriptPubKey.fromAsmHex)(
        json)
  }

  implicit object ScriptWitnessReads extends Reads[ScriptWitness] {

    override def reads(json: JsValue): JsResult[ScriptWitness] =
      SerializerUtil.processJsStringOpt[ScriptWitness](
        ScriptWitness.fromHexOpt)(json)
  }

  implicit object BlockReads extends Reads[Block] {

    override def reads(json: JsValue): JsResult[Block] =
      SerializerUtil.processJsString[Block](Block.fromHex)(json)
  }

  implicit object Sha256Hash160DigestReads extends Reads[Sha256Hash160Digest] {

    override def reads(json: JsValue): JsResult[Sha256Hash160Digest] =
      SerializerUtil.processJsString[Sha256Hash160Digest](
        Sha256Hash160Digest.fromHex)(json)
  }

  implicit object ECPublicKeyReads extends Reads[ECPublicKey] {

    override def reads(json: JsValue): JsResult[ECPublicKey] =
      SerializerUtil.processJsString[ECPublicKey](ECPublicKey.fromHex)(json)
  }

  implicit object ECPublicKeyBytesReads extends Reads[ECPublicKeyBytes] {

    override def reads(json: JsValue): JsResult[ECPublicKeyBytes] =
      SerializerUtil.processJsString[ECPublicKeyBytes](
        ECPublicKeyBytes.fromHex)(json)
  }

  implicit object SchnorrPublicKeyReads extends Reads[SchnorrPublicKey] {

    override def reads(json: JsValue): JsResult[SchnorrPublicKey] =
      SerializerUtil.processJsString[SchnorrPublicKey](
        SchnorrPublicKey.fromHex)(json)
  }

  implicit object SchnorrNonceReads extends Reads[SchnorrNonce] {

    override def reads(json: JsValue): JsResult[SchnorrNonce] =
      SerializerUtil.processJsString[SchnorrNonce](SchnorrNonce.fromHex)(json)
  }

  implicit object SchnorrDigitalSignatureReads
      extends Reads[SchnorrDigitalSignature] {

    override def reads(json: JsValue): JsResult[SchnorrDigitalSignature] =
      SerializerUtil.processJsString[SchnorrDigitalSignature](
        SchnorrDigitalSignature.fromHex)(json)
  }

  implicit object FieldElementReads extends Reads[FieldElement] {

    override def reads(json: JsValue): JsResult[FieldElement] =
      SerializerUtil.processJsString[FieldElement](FieldElement.fromHex)(json)
  }

  implicit object P2PKHAddressReads extends Reads[P2PKHAddress] {

    override def reads(json: JsValue): JsResult[P2PKHAddress] =
      json match {
        case JsString(s) =>
          P2PKHAddress.fromStringT(s) match {
            case Success(address) => JsSuccess(address)
            case Failure(err) =>
              SerializerUtil.buildErrorMsg("p2pkhaddress", err)
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsstring", err)
      }
  }

  implicit object P2SHAddressReads extends Reads[P2SHAddress] {

    override def reads(json: JsValue): JsResult[P2SHAddress] =
      json match {
        case JsString(s) =>
          P2SHAddress.fromStringT(s) match {
            case Success(address) => JsSuccess(address)
            case Failure(err) =>
              SerializerUtil.buildErrorMsg("p2shaddress", err)
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsstring", err)
      }
  }

  implicit object ScriptSignatureReads extends Reads[ScriptSignature] {

    override def reads(json: JsValue): JsResult[ScriptSignature] =
      SerializerUtil.processJsString[ScriptSignature](
        ScriptSignature.fromAsmHex)(json)
  }

  implicit object TransactionInputReads extends Reads[TransactionInput] {

    override def reads(json: JsValue): JsResult[TransactionInput] = {
      (json \ "sequence").validate[UInt32].flatMap { sequence =>
        (json \ "coinbase").validate[String] match {
          case s: JsSuccess[String] =>
            JsSuccess(
              CoinbaseInput(ScriptSignature.fromAsmHex(s.value), sequence))
          case _ =>
            (json \ "txid").validate[DoubleSha256DigestBE].flatMap { txid =>
              (json \ "vout").validate[UInt32].flatMap { vout =>
                (json \ "scriptSig" \ "hex")
                  .validate[ScriptSignature]
                  .flatMap { scriptSig =>
                    JsSuccess(
                      TransactionInput(TransactionOutPoint(txid.flip, vout),
                                       scriptSig,
                                       sequence))
                  }
              }
            }
        }
      }
    }
  }

  implicit object BitcoinAddressReads extends Reads[BitcoinAddress] {

    override def reads(json: JsValue): JsResult[BitcoinAddress] =
      json match {
        case JsString(s) =>
          BitcoinAddress.fromStringT(s) match {
            case Success(address) =>
              JsSuccess(address)
            case Failure(err) =>
              SerializerUtil.buildErrorMsg("address", err)
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsstring", err)
      }
  }

  implicit object MerkleBlockReads extends Reads[MerkleBlock] {

    override def reads(json: JsValue): JsResult[MerkleBlock] =
      SerializerUtil.processJsString[MerkleBlock](MerkleBlock.fromHex)(json)
  }

  implicit object TransactionReads extends Reads[Transaction] {

    override def reads(json: JsValue): JsResult[Transaction] =
      SerializerUtil.processJsString[Transaction](Transaction.fromHex)(json)
  }

  implicit object PSBTReads extends Reads[PSBT] {

    override def reads(json: JsValue): JsResult[PSBT] =
      SerializerUtil.processJsString[PSBT](PSBT.fromString)(json)
  }

  implicit object TransactionOutPointReads extends Reads[TransactionOutPoint] {
    private case class OutPoint(txid: DoubleSha256DigestBE, vout: UInt32)

    override def reads(json: JsValue): JsResult[TransactionOutPoint] = {
      implicit val outPointReads: Reads[OutPoint] = Json.reads[OutPoint]
      json.validate[OutPoint] match {
        case JsSuccess(op, _) =>
          JsSuccess(TransactionOutPoint(op.txid.flip, op.vout))
        case JsError(err) =>
          JsError(s"Could not parse TransactionOutPoint, got ${err.toString()}")
      }
    }
  }

  implicit object RpcAddressReads extends Reads[RpcAddress] {

    def reads(json: JsValue): JsResult[RpcAddress] =
      json match {
        case array: JsArray =>
          val bitcoinResult = array.value.find(_.isInstanceOf[JsNumber]) match {
            case Some(JsNumber(n)) => JsSuccess(Bitcoins(n))
            case Some(
                  err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
                  _: JsObject)) =>
              SerializerUtil.buildJsErrorMsg("jsnumber", err)
            case None => JsError("error.expected.balance")
          }
          bitcoinResult.flatMap { bitcoins =>
            val jsStrings =
              array.value
                .filter(_.isInstanceOf[JsString])
                .map(_.asInstanceOf[JsString].value)
            val addressResult = jsStrings.find(BitcoinAddress.isValid) match {
              case Some(s) =>
                BitcoinAddress.fromStringT(s) match {
                  case Success(a) => JsSuccess(a)
                  case Failure(err) =>
                    SerializerUtil.buildErrorMsg("address", err)
                }
              case None => JsError("error.expected.address")
            }
            addressResult.flatMap { address =>
              val account = jsStrings.find(s => !BitcoinAddress.isValid(s))
              JsSuccess(RpcAddress(address, bitcoins, account))
            }
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsString |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsarray", err)
      }
  }

  implicit object HashTypeReads extends Reads[HashType] {

    override def reads(json: JsValue): JsResult[HashType] =
      SerializerUtil.processJsString {
        case "ALL"                 => HashType.sigHashAll
        case "NONE"                => HashType.sigHashNone
        case "SINGLE"              => HashType.sigHashSingle
        case "ALL|ANYONECANPAY"    => HashType.sigHashAllAnyoneCanPay
        case "NONE|ANYONECANPAY"   => HashType.sigHashNoneAnyoneCanPay
        case "SINGLE|ANYONECANPAY" => HashType.sigHashSingleAnyoneCanPay
      }(json)
  }

  implicit object FinalizedPsbtReads extends Reads[FinalizedPsbt] {

    override def reads(json: JsValue): JsResult[FinalizedPsbt] =
      (json \ "complete").validate[Boolean].flatMap { completed =>
        if (completed) {
          (json \ "hex").validate[Transaction].flatMap { tx =>
            JsSuccess(FinalizedPsbt(tx))
          }
        } else {
          JsError("PSBT was not completed!")
        }
      }
  }

  implicit object NonFinalizedPsbtReads extends Reads[NonFinalizedPsbt] {

    override def reads(json: JsValue): JsResult[NonFinalizedPsbt] =
      if ((json \ "hex").isDefined) {
        JsError("PSBT was submitted as a serialized hex transaction!")
      } else {
        (json \ "psbt").validate[PSBT].map(NonFinalizedPsbt.apply)
      }
  }

  implicit object FinalizePsbtResultReads extends Reads[FinalizePsbtResult] {

    override def reads(json: JsValue): JsResult[FinalizePsbtResult] =
      if ((json \ "hex").isDefined) {
        json.validate[FinalizedPsbt]
      } else {
        json.validate[NonFinalizedPsbt]
      }
  }

  implicit object RpcPsbtOutputReads extends Reads[RpcPsbtOutput] {

    override def reads(json: JsValue): JsResult[RpcPsbtOutput] =
      for {
        redeemScript <- (json \ "redeem_script").validateOpt[RpcPsbtScript]
        witnessScript <- (json \ "witness_script").validateOpt[RpcPsbtScript]
        unknown <- (json \ "unknown").validateOpt[Map[String, String]]
        bip32Derivs <- (json \ "bip32_derivs")
          .validateOpt[Vector[PsbtBIP32Deriv]]
      } yield RpcPsbtOutput(redeemScript, witnessScript, bip32Derivs, unknown)
  }

  implicit object PsbtBIP32DerivsReads extends Reads[PsbtBIP32Deriv] {

    override def reads(json: JsValue): JsResult[PsbtBIP32Deriv] =
      for {
        pubkey <- (json \ "pubkey").validate[ECPublicKey]
        masterFingerprint <- (json \ "master_fingerprint").validate[String]
        path <- (json \ "path").validate[String]
      } yield PsbtBIP32Deriv(pubkey = pubkey,
                             masterFingerprint = masterFingerprint,
                             path = path)
  }

  implicit object RpcPsbtScriptReads extends Reads[RpcPsbtScript] {

    override def reads(json: JsValue): JsResult[RpcPsbtScript] =
      for {
        asm <- (json \ "asm").validate[String]
        hex <- (json \ "hex").validate[ScriptPubKey]
        scriptType <- (json \ "type").validateOpt[ScriptType]
        address <- (json \ "address").validateOpt[BitcoinAddress]
      } yield RpcPsbtScript(asm = asm,
                            hex = hex,
                            scriptType = scriptType,
                            address = address)
  }

  implicit object MapPubKeySignatureReads
      extends Reads[Map[ECPublicKey, ECDigitalSignature]] {

    override def reads(
        json: JsValue): JsResult[Map[ECPublicKey, ECDigitalSignature]] =
      JsonReaders.mapReads(json)(implicitly[Reads[ECPublicKey]],
                                 implicitly[Reads[ECDigitalSignature]])
  }

  implicit object RpcPsbtInputV22Reads extends Reads[RpcPsbtInputV22] {

    override def reads(json: JsValue): JsResult[RpcPsbtInputV22] =
      for {
        nonWitnessUtxo <- (json \ "non_witness_utxo")
          .validateOpt[RpcTransactionV22]
        witnessUtxo <- (json \ "witness_utxo").validateOpt[PsbtWitnessUtxoInput]
        finalScriptSig <- (json \ "final_scriptSig").validateOpt[RpcPsbtScript]
        redeemScript <- (json \ "redeem_script").validateOpt[RpcPsbtScript]
        sighash <- (json \ "sighash").validateOpt[HashType]
        partialSignatures <- (json \ "partial_signatures")
          .validateOpt[Map[ECPublicKey, ECDigitalSignature]]
        witnessScript <- (json \ "witness_script").validateOpt[RpcPsbtScript]
        bip32Derivs <- (json \ "bi32_derivs")
          .validateOpt[Vector[PsbtBIP32Deriv]]
        finalScriptWitness <-
          JsSuccess(None) // todo(torkelrogstad) find an example of this
        unknown <- (json \ "unknown").validateOpt[Map[String, String]]
      } yield {
        bitcoind.RpcPsbtInputV22(
          nonWitnessUtxo = nonWitnessUtxo,
          witnessUtxo = witnessUtxo,
          partialSignatures = partialSignatures,
          sighash = sighash,
          redeemScript = redeemScript,
          witnessScript = witnessScript,
          bip32Derivs = bip32Derivs,
          finalScriptSig = finalScriptSig,
          finalScriptwitness = finalScriptWitness,
          unknown = unknown
        )
      }

  }

  implicit object RpcPsbtInputPreV22Reads extends Reads[RpcPsbtInputPreV22] {

    override def reads(json: JsValue): JsResult[RpcPsbtInputPreV22] =
      for {
        nonWitnessUtxo <- (json \ "non_witness_utxo")
          .validateOpt[RpcTransactionPreV22]
        witnessUtxo <- (json \ "witness_utxo").validateOpt[PsbtWitnessUtxoInput]
        finalScriptSig <- (json \ "final_scriptSig").validateOpt[RpcPsbtScript]
        redeemScript <- (json \ "redeem_script").validateOpt[RpcPsbtScript]
        sighash <- (json \ "sighash").validateOpt[HashType]
        partialSignatures <- (json \ "partial_signatures")
          .validateOpt[Map[ECPublicKey, ECDigitalSignature]]
        witnessScript <- (json \ "witness_script").validateOpt[RpcPsbtScript]
        bip32Derivs <- (json \ "bi32_derivs")
          .validateOpt[Vector[PsbtBIP32Deriv]]
        finalScriptWitness <-
          JsSuccess(None) // todo(torkelrogstad) find an example of this
        unknown <- (json \ "unknown").validateOpt[Map[String, String]]
      } yield {
        bitcoind.RpcPsbtInputPreV22(
          nonWitnessUtxo = nonWitnessUtxo,
          witnessUtxo = witnessUtxo,
          partialSignatures = partialSignatures,
          sighash = sighash,
          redeemScript = redeemScript,
          witnessScript = witnessScript,
          bip32Derivs = bip32Derivs,
          finalScriptSig = finalScriptSig,
          finalScriptwitness = finalScriptWitness,
          unknown = unknown
        )
      }

  }

  implicit object ScriptTypeReads extends Reads[ScriptType] {

    override def reads(json: JsValue): JsResult[ScriptType] =
      json
        .validate[String]
        .map(ScriptType.fromString)
  }

  implicit object TestMempoolAcceptResultReads
      extends Reads[TestMempoolAcceptResult] {

    override def reads(json: JsValue): JsResult[TestMempoolAcceptResult] =
      for {
        txid <- (json \ "txid").validate[DoubleSha256DigestBE]
        allowed <- (json \ "allowed").validate[Boolean]
        rejectReason <- (json \ "reject-reason").validateOpt[String]
      } yield TestMempoolAcceptResult(txid, allowed, rejectReason)
  }

  // Currently takes in BTC/kB
  implicit object BitcoinFeeUnitReads extends Reads[BitcoinFeeUnit] {

    override def reads(json: JsValue): JsResult[BitcoinFeeUnit] =
      SerializerUtil.processJsNumber[BitcoinFeeUnit](num =>
        SatoshisPerByte(Satoshis((num * 100000).toBigInt)))(json)
  }

  implicit object SatoshisPerVByteReads extends Reads[SatoshisPerVirtualByte] {

    override def reads(json: JsValue): JsResult[SatoshisPerVirtualByte] =
      SerializerUtil.processJsNumberBigInt[SatoshisPerVirtualByte](num =>
        SatoshisPerVirtualByte.fromLong(num.toLong))(json)
  }

  implicit object FileReads extends Reads[File] {

    override def reads(json: JsValue): JsResult[File] =
      SerializerUtil.processJsString[File](new File(_))(json)
  }

  implicit object PathReads extends Reads[Path] {

    override def reads(json: JsValue): JsResult[Path] =
      SerializerUtil.processJsString[Path](new File(_).toPath)(json)
  }

  implicit object URIReads extends Reads[URI] {

    override def reads(json: JsValue): JsResult[URI] =
      SerializerUtil.processJsString[URI](str => new URI("http://" + str))(json)
  }

  implicit object NetworkParamsReads extends Reads[NetworkParameters] {

    def reads(json: JsValue): JsResult[NetworkParameters] =
      json.validate[String].map(_.toLowerCase()).map {
        case "regtest" => RegTest
        case "main"    => MainNet
        case "test"    => TestNet3
      }
  }

  implicit object ServiceIdentifierReads extends Reads[ServiceIdentifier] {

    override def reads(json: JsValue): JsResult[ServiceIdentifier] =
      json match {
        case JsString(s) =>
          Try(ServiceIdentifier.fromString(s)) match {
            case Success(serviceIdentifier) => JsSuccess(serviceIdentifier)
            case Failure(err) =>
              SerializerUtil.buildJsErrorMsg(
                s"Unexpected Service Identifier: $err",
                json)
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsstring", err)
      }
  }

  implicit object OracleAnnouncementV0TLVReads
      extends Reads[OracleAnnouncementV0TLV] {

    override def reads(json: JsValue): JsResult[OracleAnnouncementV0TLV] =
      json match {
        case JsString(s) =>
          OracleAnnouncementV0TLV.fromHexT(s) match {
            case Success(ann) => JsSuccess(ann)
            case Failure(err) =>
              SerializerUtil.buildJsErrorMsg(
                s"Unexpected Service Identifier: $err",
                json)
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsstring", err)
      }
  }

  implicit object OracleAttestmentV0TLVReads
      extends Reads[OracleAttestmentV0TLV] {

    override def reads(json: JsValue): JsResult[OracleAttestmentV0TLV] =
      json match {
        case JsString(s) =>
          OracleAttestmentV0TLV.fromHexT(s) match {
            case Success(att) => JsSuccess(att)
            case Failure(err) =>
              SerializerUtil.buildJsErrorMsg(
                s"Unexpected Service Identifier: $err",
                json)
          }
        case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
            _: JsObject) =>
          SerializerUtil.buildJsErrorMsg("jsstring", err)
      }
  }

  implicit val feeProportionalMillionthsReads: Reads[
    FeeProportionalMillionths] = Reads { case js =>
    SerializerUtil.processJsNumberBigInt(FeeProportionalMillionths.fromBigInt)(
      js)
  }

  implicit val channelStateReads: Reads[ChannelState] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil.processJsStringOpt(ChannelState.fromStringOpt)(jsValue)
    }
  }

  implicit val normalChannelStateReads: Reads[ChannelState.NORMAL.type] =
    Reads { case jsValue =>
      jsValue
        .validate[ChannelState]
        .flatMap {
          case ChannelState.NORMAL => JsSuccess(ChannelState.NORMAL)
          case state: ChannelState =>
            JsError(s"$state is not ChannelState.NORMAL")
        }
    }

  implicit val peerStateReads: Reads[PeerState] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil.processJsStringOpt(PeerState.fromString)(jsValue)
    }
  }

  implicit val picoBitcoinsReads: Reads[PicoBitcoins] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil.processJsNumberBigInt(PicoBitcoins.apply)(jsValue)
    }
  }

  implicit val msatReads: Reads[MilliSatoshis] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil
        .processJsNumberBigInt(MilliSatoshis.apply)(jsValue)
        .orElse {
          SerializerUtil.processJsString { str =>
            require(str.endsWith("msat"))
            val withoutUnit = str.dropRight(4)
            val num = BigInt(withoutUnit)
            MilliSatoshis(num)
          }(jsValue)
        }
    }
  }

  implicit val nodeIdReads: Reads[NodeId] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil.processJsString(NodeId.fromHex)(jsValue)
    }
  }

  implicit val lnHrpReads: Reads[LnHumanReadablePart] = {
    Reads { case jsValue =>
      SerializerUtil.processJsStringOpt(LnHumanReadablePart.fromStringOpt(_))(
        jsValue)
    }
  }

  implicit val lnInvoiceSignatureReads: Reads[LnInvoiceSignature] = {
    Reads { case jsValue =>
      SerializerUtil.processJsString(LnInvoiceSignature.fromHex)(jsValue)
    }
  }

  implicit val inetSocketAddressReads: Reads[InetSocketAddress] = {
    Reads { case jsValue =>
      SerializerUtil.processJsString { addr =>
        addr.split(":") match {
          case Array(host, portStr) =>
            val port = Try(portStr.toInt).getOrElse(
              throw new RuntimeException(s"Invalid port number `$portStr`"))
            InetSocketAddress.createUnresolved(host, port.toInt)
          case _ => throw new RuntimeException(s"Invalid inet address `$addr`")
        }
      }(jsValue)
    }
  }

  implicit val featureSupportReads: Reads[FeatureSupport] =
    Reads { case jsValue =>
      SerializerUtil.processJsString {
        case "mandatory" => FeatureSupport.Mandatory
        case "optional"  => FeatureSupport.Optional
        case err: String =>
          throw new RuntimeException(s"Invalid feature support value: `$err`")
      }(jsValue)
    }

  lazy val featuresByName: Map[String, Feature] =
    org.bitcoins.core.protocol.ln.node.Features.knownFeatures
      .map(f => (f.rfcName, f))
      .toMap

  implicit val featureReads: Reads[Feature] =
    Reads { case jsValue =>
      SerializerUtil.processJsString(featuresByName)(jsValue)
    }

  implicit val unknownFeatureReads: Reads[UnknownFeature] =
    Reads { case jsValue =>
      SerializerUtil.processJsString(s => UnknownFeature(s.toInt))(jsValue)
    }

  implicit val featuresReads: Reads[Features] = {
    Reads { case jsValue =>
      for {
        activatedObj <- (jsValue \ "activated")
          .validate[Map[String, FeatureSupport]]
        unknown <- (jsValue \ "unknown").validate[Set[UnknownFeature]]
      } yield {
        val activated = activatedObj.toSeq.map(x =>
          ActivatedFeature(featuresByName(x._1), x._2))
        Features(
          activated = activated.toSet,
          unknown = unknown
        )
      }
    }
  }

  implicit val getInfoResultReads: Reads[GetInfoResult] = {
    Json.reads[GetInfoResult]
  }

  implicit val peerInfoReads: Reads[PeerInfo] = {
    Json.reads[PeerInfo]
  }

  implicit val shortChannelIdReads: Reads[ShortChannelId] = {
    Reads { case jsValue =>
      SerializerUtil.processJsString(ShortChannelId.fromHumanReadableString)(
        jsValue)
    }
  }

  implicit val realChannelIdReads: Reads[RealChannelId] =
    Json.reads[RealChannelId]

  implicit val shortIdsReads: Reads[ShortIds] =
    Json.reads[ShortIds]

  implicit val nodeInfoReads: Reads[NodeInfo] = {
    Reads { case jsValue =>
      for {
        signature <- (jsValue \ "signature").validate[ECDigitalSignature]
        features <- (jsValue \ "features").validate[Features]
        timestamp <- (jsValue \ "timestamp" \ "unix")
          .validate[Instant](instantReadsSeconds)
        nodeId <- (jsValue \ "nodeId").validate[NodeId]
        rgbColor <- (jsValue \ "rgbColor").validate[String]
        alias <- (jsValue \ "alias").validate[String]
        addresses <- (jsValue \ "addresses").validate[Vector[InetSocketAddress]]
      } yield NodeInfo(signature,
                       features,
                       timestamp,
                       nodeId,
                       rgbColor,
                       alias,
                       addresses)
    }
  }

  implicit val paymentPreimageReads: Reads[PaymentPreimage] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil.processJsString(PaymentPreimage.fromHex)(jsValue)
    }
  }

  implicit val paymentSecretReads: Reads[PaymentSecret] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil.processJsString(PaymentSecret.fromHex)(jsValue)
    }
  }

  implicit val fundedChannelIdReads: Reads[FundedChannelId] = {
    Reads { case jsValue: JsValue =>
      SerializerUtil.processJsString(FundedChannelId.fromHex)(jsValue)
    }
  }

  implicit val channelDescReads: Reads[ChannelDesc] = {
    Json.reads[ChannelDesc]
  }

  implicit val createInvoiceResultReads: Reads[InvoiceResult] = {
    Reads { jsValue =>
      for {
        prefix <- (jsValue \ "prefix").validate[LnHumanReadablePart]
        timestamp <- (jsValue \ "timestamp")
          .validate[Instant](instantReadsSeconds)
        nodeId <- (jsValue \ "nodeId").validate[NodeId]
        serialized <- (jsValue \ "serialized").validate[String]
        description <- (jsValue \ "description").validate[String]
        paymentHash <- (jsValue \ "paymentHash").validate[Sha256Digest]
        expiry <- (jsValue \ "expiry").validate[Long]
      } yield InvoiceResult(prefix,
                            timestamp,
                            nodeId,
                            serialized,
                            description,
                            paymentHash,
                            expiry.seconds)
    }
  }

  implicit val openChannelInfoReads: Reads[OpenChannelInfo] = Reads { jsValue =>
    for {
      nodeId <- (jsValue \ "nodeId").validate[NodeId]
      shortChannelId <- (jsValue \ "data" \ "shortIds" \ "real" \ "realScid")
        .validate[ShortChannelId]
      channelId <- (jsValue \ "channelId").validate[FundedChannelId]
      state <- (jsValue \ "state").validate[ChannelState.NORMAL.type]
      remoteMsat <-
        (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toRemote")
          .validate[MilliSatoshis]
      localMsat <-
        (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toLocal")
          .validate[MilliSatoshis]

    } yield OpenChannelInfo(nodeId = nodeId,
                            shortChannelId = shortChannelId,
                            channelId = channelId,
                            localMsat = localMsat,
                            remoteMsat = remoteMsat,
                            state = state)
  }

  implicit val baseChannelInfoReads: Reads[BaseChannelInfo] = Reads { jsValue =>
    for {
      nodeId <- (jsValue \ "nodeId").validate[NodeId]
      channelId <- (jsValue \ "channelId").validate[FundedChannelId]
      state <- (jsValue \ "state").validate[ChannelState]
      remoteMsat <-
        (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toRemote")
          .validate[MilliSatoshis]
      localMsat <-
        (jsValue \ "data" \ "commitments" \ "localCommit" \ "spec" \ "toLocal")
          .validate[MilliSatoshis]

    } yield BaseChannelInfo(nodeId = nodeId,
                            channelId = channelId,
                            localMsat = localMsat,
                            remoteMsat = remoteMsat,
                            state = state)
  }

  implicit val channelInfoReads: Reads[ChannelInfo] = Reads { jsValue =>
    (jsValue \ "state")
      .validate[ChannelState]
      .flatMap {
        case ChannelState.NORMAL =>
          jsValue.validate[OpenChannelInfo]
        case _: ChannelState =>
          jsValue.validate[BaseChannelInfo]
      }
  }

  implicit val channelCommandResultStateReads: Reads[State] = Reads { jsValue =>
    SerializerUtil.processJsString(ChannelCommandResult.fromString)(jsValue)
  }

  implicit val channelCommandResultReads: Reads[ChannelCommandResult] = Reads {
    case obj: JsObject =>
      JsSuccess(ChannelCommandResult(obj.value.map { x =>
        val channelId = Try(FundedChannelId.fromHex(x._1)) match {
          case Success(id) => Right(id)
          case Failure(_)  => Left(ShortChannelId.fromHumanReadableString(x._1))
        }
        (channelId, x._2.validate[State].get)
      }))
    case err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
        _: JsNumber) =>
      SerializerUtil.buildJsErrorMsg("jsobject", err)
  }

  implicit val updateRelayFeeResultReads: Reads[UpdateRelayFeeResult] =
    Reads {
      case obj: JsObject =>
        JsSuccess(UpdateRelayFeeResult(obj.value.map { x =>
          val channelId = Try(FundedChannelId.fromHex(x._1)) match {
            case Success(id) => Right(id)
            case Failure(_) =>
              Left(ShortChannelId.fromHumanReadableString(x._1))
          }
          val result = Try(
            UpdateRelayFee.OK(
              channelId = FundedChannelId.fromHex(
                (x._2 \ "channelId").validate[String].get),
              feeBaseMsat =
                (x._2 \ "cmd" \ "feeBase").validate[MilliSatoshis].get,
              feeProportionalMillionths =
                (x._2 \ "cmd" \ "feeProportionalMillionths").validate[Long].get
            )) match {
            case Success(ok) => ok
            case Failure(_)  => UpdateRelayFee.Error(x._2.toString())
          }
          (channelId, result)
        }.toMap))

      case err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
          _: JsNumber) =>
        SerializerUtil.buildJsErrorMsg("jsobject", err)
    }

  implicit val channelFlagsReads: Reads[ChannelFlags] =
    Json.reads[ChannelFlags]

  implicit val channelUpdateReads: Reads[ChannelUpdate] = {
    Reads { jsValue =>
      for {
        signature <- (jsValue \ "signature").validate[ECDigitalSignature]
        chainHash <- (jsValue \ "chainHash").validate[DoubleSha256Digest]
        shortChannelId <- (jsValue \ "shortChannelId").validate[ShortChannelId]
        timestamp <- (jsValue \ "timestamp" \ "unix")
          .validate[Instant](instantReadsSeconds)
        channelFlags <- (jsValue \ "channelFlags").validate[ChannelFlags]
        cltvExpiryDelta <- (jsValue \ "cltvExpiryDelta").validate[Int]
        htlcMinimumMsat <- (jsValue \ "htlcMinimumMsat").validate[MilliSatoshis]
        feeProportionalMillionths <- (jsValue \ "feeProportionalMillionths")
          .validate[FeeProportionalMillionths]
        htlcMaximumMsat <- (jsValue \ "htlcMaximumMsat")
          .validateOpt[MilliSatoshis]
        feeBaseMsat <- (jsValue \ "feeBaseMsat").validate[MilliSatoshis]
      } yield ChannelUpdate(
        signature,
        chainHash,
        shortChannelId,
        timestamp,
        channelFlags,
        cltvExpiryDelta,
        htlcMinimumMsat,
        feeProportionalMillionths,
        htlcMaximumMsat,
        feeBaseMsat
      )
    }
  }

  implicit val paymentIdReads: Reads[PaymentId] = Reads { jsValue =>
    SerializerUtil.processJsString(s => PaymentId(UUID.fromString(s)))(jsValue)
  }

  implicit val sendToRouteResultReads: Reads[SendToRouteResult] =
    Json.reads[SendToRouteResult]

  //don't make this implicit so we don't accidentally read the wrong time unit
  val finiteDurationReadsMilliseconds: Reads[FiniteDuration] =
    Reads { js =>
      SerializerUtil.processJsNumberBigInt(_.longValue.millis)(js)
    }

  //don't make this implicit so we don't accidentally read the wrong time unit
  val finiteDurationReadsSeconds: Reads[FiniteDuration] = Reads { js =>
    SerializerUtil.processJsNumberBigInt(_.longValue.seconds)(js)
  }

  val instantReadsMilliseconds: Reads[Instant] =
    Reads { js =>
      SerializerUtil.processJsNumberBigInt(x =>
        Instant.ofEpochMilli(x.longValue))(js)
    }

  val instantReadsSeconds: Reads[Instant] = Reads { js =>
    SerializerUtil.processJsNumberBigInt(x =>
      Instant.ofEpochSecond(x.longValue))(js)
  }

  implicit val paymentTypeReads: Reads[PaymentType] = Reads { jsValue =>
    SerializerUtil.processJsString(PaymentType.fromString)(jsValue)
  }

  implicit val paymentReceivedReads: Reads[IncomingPaymentStatus.Received] =
    Reads { js =>
      for {
        amount <- (js \ "amount").validate[MilliSatoshis]
        receivedAt <- (js \ "receivedAt" \ "unix")
          .validate[Instant](instantReadsMilliseconds)
      } yield IncomingPaymentStatus.Received(amount, receivedAt)
    }

  implicit val hopReads: Reads[Hop] =
    Json.reads[Hop]

  implicit val paymentSentReads: Reads[OutgoingPaymentStatus.Succeeded] =
    Reads { js =>
      for {
        preimage <- (js \ "paymentPreimage").validate[PaymentPreimage]
        feesPaid <- (js \ "feesPaid").validate[MilliSatoshis]
        route <- (js \ "route").validate[Seq[Hop]]
        completed <- (js \ "completedAt" \ "unix")
          .validate[Instant](instantReadsMilliseconds)
      } yield OutgoingPaymentStatus.Succeeded(paymentPreimage = preimage,
                                              feesPaid = feesPaid,
                                              route = route,
                                              completedAt = completed)
    }

  implicit val paymentFailureTypeReads: Reads[PaymentFailure.Type] = Reads {
    jsValue =>
      jsValue
        .validate[String]
        .flatMap { s =>
          s.toLowerCase match {
            case "local"  => JsSuccess(PaymentFailure.Local)
            case "remote" => JsSuccess(PaymentFailure.Remote)
            case "unreadableremote" =>
              JsSuccess(PaymentFailure.UnreadableRemote)
            case _ =>
              throw new RuntimeException(s"Unknown payment failure type `$s`")
          }
        }
  }

  implicit val paymentFailureReads: Reads[PaymentFailure] =
    Json.reads[PaymentFailure]

  implicit val paymentFailedReads: Reads[OutgoingPaymentStatus.Failed] =
    Json.reads[OutgoingPaymentStatus.Failed]

  implicit val outgoingPaymentStatusReads: Reads[OutgoingPaymentStatus] =
    Reads { jsValue =>
      (jsValue \ "type")
        .validate[String]
        .flatMap {
          case "pending" => JsSuccess(OutgoingPaymentStatus.Pending)
          case "sent"    => jsValue.validate[OutgoingPaymentStatus.Succeeded]
          case "failed"  => jsValue.validate[OutgoingPaymentStatus.Failed]
        }
    }

  implicit val incomingPaymentStatusReads: Reads[IncomingPaymentStatus] =
    Reads { jsValue =>
      (jsValue \ "type")
        .validate[String]
        .flatMap {
          case "pending"  => JsSuccess(IncomingPaymentStatus.Pending)
          case "expired"  => JsSuccess(IncomingPaymentStatus.Expired)
          case "received" => jsValue.validate[IncomingPaymentStatus.Received]
        }
    }

  implicit val paymentRequestReads: Reads[PaymentRequest] = Reads { js =>
    for {
      prefix <- (js \ "prefix").validate[LnHumanReadablePart]
      timestamp <- (js \ "timestamp")
        .validate[Instant](instantReadsSeconds)
      nodeId <- (js \ "nodeId").validate[NodeId]
      serialized <- (js \ "serialized").validate[String]
      description <- (js \ "serialized").validate[String]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      paymentMetadata <- (js \ "paymentMetadata").validate[String]
      expiry <- (js \ "expiry")
        .validate[FiniteDuration](finiteDurationReadsSeconds)
      amount <- (js \ "amount").validateOpt[MilliSatoshis]
      minFinalCltvExpiry <- (js \ "minFinalCltvExpiry").validate[Int]
      features <- (js \ "features").validate[Features]
    } yield PaymentRequest(prefix,
                           timestamp,
                           nodeId,
                           serialized,
                           description,
                           paymentHash,
                           paymentMetadata,
                           expiry,
                           minFinalCltvExpiry,
                           amount,
                           features)
  }

  implicit val paymentSucceededReads: Reads[OutgoingPayment] = Reads { js =>
    for {
      id <- (js \ "id").validate[PaymentId]
      parentId <- (js \ "parentId").validate[PaymentId]
      externalId <- (js \ "externalId").validateOpt[String]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      paymentType <- (js \ "paymentType").validate[PaymentType]
      amount <- (js \ "amount").validate[MilliSatoshis]
      recipientAmount <- (js \ "recipientAmount").validate[MilliSatoshis]
      recipientNodeId <- (js \ "recipientNodeId").validate[NodeId]
      createdAt <- (js \ "createdAt" \ "unix")
        .validate[Instant](instantReadsMilliseconds)
      paymentRequest <- (js \ "paymentRequest").validateOpt[PaymentRequest]
      status <- (js \ "status").validate[OutgoingPaymentStatus]
    } yield OutgoingPayment(id,
                            parentId,
                            externalId,
                            paymentHash,
                            paymentType,
                            amount,
                            recipientAmount,
                            recipientNodeId,
                            createdAt,
                            paymentRequest,
                            status)
  }

  implicit val receivedPaymentResultReads: Reads[IncomingPayment] = Reads {
    js =>
      for {
        paymentRequest <- (js \ "invoice").validate[PaymentRequest]
        paymentPreimage <- (js \ "paymentPreimage").validate[PaymentPreimage]
        paymentType <- (js \ "paymentType").validate[PaymentType]
        createdAt <- (js \ "createdAt" \ "unix")
          .validate[Instant](instantReadsMilliseconds)
        status <- (js \ "status").validate[IncomingPaymentStatus]
      } yield IncomingPayment(paymentRequest,
                              paymentPreimage,
                              paymentType,
                              createdAt,
                              status)
  }

  implicit val channelResultReads: Reads[ChannelResult] = Reads { js =>
    for {
      nodeId <- (js \ "nodeId").validate[NodeId]
      channelId <- (js \ "channelId").validate[FundedChannelId]
      state <- (js \ "state").validate[ChannelState]
      feeBaseMsat <- (js \ "data" \ "channelUpdate" \ "feeBaseMsat")
        .validateOpt[MilliSatoshis]
      feeProportional <-
        (js \ "data" \ "channelUpdate" \ "feeProportionalMillionths")
          .validateOpt[FeeProportionalMillionths]
      data <- (js \ "data").validate[JsObject]
    } yield ChannelResult(nodeId = nodeId,
                          state = state,
                          channelId = channelId,
                          feeBaseMsat = feeBaseMsat,
                          feeProportionalMillionths = feeProportional,
                          data = data)
  }

  implicit val lnInvoiceReads: Reads[LnInvoice] =
    Reads[LnInvoice] {
      case JsString(invoice) =>
        LnInvoice.fromStringT(invoice) match {
          case Success(paymentRequest) => JsSuccess(paymentRequest)
          case Failure(err) =>
            JsError(s"Invalid refund invoice: ${err.toString}")
        }
      case bad @ (_: JsNumber | _: JsObject | _: JsArray | _: JsBoolean |
          JsNull) =>
        JsError(s"Invalid type on refund invoice: $bad, expected JsString")
    }

  implicit val receivedPaymentPartReads: Reads[ReceivedPayment.Part] = Reads {
    js =>
      for {
        amount <- (js \ "amount").validate[MilliSatoshis]
        fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
        timestamp <- (js \ "timestamp" \ "unix")
          .validate[Instant](instantReadsMilliseconds)
      } yield ReceivedPayment.Part(amount, fromChannelId, timestamp)
  }

  implicit val receivedPaymentReads: Reads[ReceivedPayment] =
    Json.reads[ReceivedPayment]

  implicit val sentPaymentPartReads: Reads[SentPayment.Part] = Reads { js =>
    for {
      id <- (js \ "id").validate[PaymentId]
      amount <- (js \ "amount").validate[MilliSatoshis]
      feesPaid <- (js \ "feesPaid").validate[MilliSatoshis]
      toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp" \ "unix")
        .validate[Instant](instantReadsMilliseconds)
    } yield SentPayment.Part(id, amount, feesPaid, toChannelId, timestamp)
  }

  implicit val sentPaymentReads: Reads[SentPayment] = Json.reads[SentPayment]

  implicit val relayedPaymentReads: Reads[RelayedPayment] = Reads { js =>
    for {
      amountIn <- (js \ "amountIn").validate[MilliSatoshis]
      amountOut <- (js \ "amountOut").validate[MilliSatoshis]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
      toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp" \ "unix")
        .validate[Instant](instantReadsMilliseconds)
    } yield RelayedPayment(amountIn,
                           amountOut,
                           paymentHash,
                           fromChannelId,
                           toChannelId,
                           timestamp)
  }
  implicit val auditResultReads: Reads[AuditResult] = Json.reads[AuditResult]

  implicit val networkFeesResultReads: Reads[NetworkFeesResult] = Reads { js =>
    for {
      remoteNodeId <- (js \ "remoteNodeId").validate[NodeId]
      channelId <- (js \ "channelId").validate[FundedChannelId]
      txId <- (js \ "txId").validate[DoubleSha256DigestBE]
      fee <- (js \ "fee").validate[Satoshis]
      txType <- (js \ "txType").validate[String]
      timestamp <- (js \ "timestamp" \ "unix")
        .validate[Instant](instantReadsMilliseconds)
    } yield NetworkFeesResult(remoteNodeId,
                              channelId,
                              txId,
                              fee,
                              txType,
                              timestamp)
  }

  implicit val channelStatsDirectionReads: Reads[ChannelStats.Direction] =
    Reads { json =>
      SerializerUtil.processJsString(ChannelStats.Direction.fromString)(json)
    }

  implicit val channelStatsReads: Reads[ChannelStats] =
    Json.reads[ChannelStats]

  implicit val usableBalancesResultReads: Reads[UsableBalancesResult] =
    Json.reads[UsableBalancesResult]

  implicit val paymentRelayedEventReads: Reads[WebSocketEvent.PaymentRelayed] =
    Reads { js =>
      for {
        amountIn <- (js \ "amountIn").validate[MilliSatoshis]
        amountOut <- (js \ "amountOut").validate[MilliSatoshis]
        paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
        fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
        toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
        timestamp <- (js \ "timestamp" \ "unix")
          .validate[Instant](instantReadsMilliseconds)
      } yield WebSocketEvent.PaymentRelayed(amountIn,
                                            amountOut,
                                            paymentHash,
                                            fromChannelId,
                                            toChannelId,
                                            timestamp)
    }

  implicit val paymentReceivedEventPartReads: Reads[
    WebSocketEvent.PaymentReceived.Part] = Reads { js =>
    for {
      amount <- (js \ "amount").validate[MilliSatoshis]
      fromChannelId <- (js \ "fromChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp" \ "unix")
        .validate[Instant](instantReadsMilliseconds)
    } yield WebSocketEvent.PaymentReceived.Part(amount,
                                                fromChannelId,
                                                timestamp)
  }

  implicit val paymentReceivedEventReads: Reads[
    WebSocketEvent.PaymentReceived] = Reads { js =>
    for {
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      parts <- (js \ "parts")
        .validate[Vector[WebSocketEvent.PaymentReceived.Part]]
    } yield WebSocketEvent.PaymentReceived(paymentHash, parts)
  }

  implicit val paymentFailedEventReads: Reads[WebSocketEvent.PaymentFailed] =
    Reads { js =>
      for {
        id <- (js \ "id").validate[PaymentId]
        paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
        failures <- (js \ "failures").validate[Vector[JsObject]]
        timestamp <- (js \ "timestamp" \ "unix")
          .validate[Instant](instantReadsMilliseconds)
      } yield WebSocketEvent.PaymentFailed(id,
                                           paymentHash,
                                           failures.map(_.toString()),
                                           timestamp)
    }

  implicit val paymentSentEventPartReads: Reads[
    WebSocketEvent.PaymentSent.Part] = Reads { js =>
    for {
      id <- (js \ "id").validate[PaymentId]
      amount <- (js \ "amount").validate[MilliSatoshis]
      feesPaid <- (js \ "feesPaid").validate[MilliSatoshis]
      toChannelId <- (js \ "toChannelId").validate[FundedChannelId]
      timestamp <- (js \ "timestamp" \ "unix")
        .validate[Instant](instantReadsMilliseconds)
    } yield WebSocketEvent.PaymentSent.Part(id,
                                            amount,
                                            feesPaid,
                                            toChannelId,
                                            timestamp)
  }

  implicit val paymentSentEventReads: Reads[WebSocketEvent.PaymentSent] =
    Reads { js =>
      for {
        id <- (js \ "id").validate[PaymentId]
        paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
        paymentPreimage <- (js \ "paymentPreimage").validate[PaymentPreimage]
        parts <- (js \ "parts")
          .validate[Vector[WebSocketEvent.PaymentSent.Part]]
      } yield WebSocketEvent.PaymentSent(id,
                                         paymentHash,
                                         paymentPreimage,
                                         parts)
    }

  implicit val paymentSettlingOnchainEventReads: Reads[
    WebSocketEvent.PaymentSettlingOnchain] = Reads { js =>
    for {
      amount <- (js \ "amount").validate[MilliSatoshis]
      paymentHash <- (js \ "paymentHash").validate[Sha256Digest]
      timestamp <- (js \ "timestamp" \ "unix")
        .validate[Instant](instantReadsMilliseconds)
    } yield WebSocketEvent.PaymentSettlingOnchain(amount,
                                                  paymentHash,
                                                  timestamp)
  }

  implicit val webSocketEventReads: Reads[WebSocketEvent] =
    Reads { js =>
      (js \ "type")
        .validate[String]
        .flatMap {
          case "payment-relayed"  => js.validate[WebSocketEvent.PaymentRelayed]
          case "payment-received" => js.validate[WebSocketEvent.PaymentReceived]
          case "payment-failed" =>
            js.validate[WebSocketEvent.PaymentFailed]
          case "payment-sent" =>
            js.validate[WebSocketEvent.PaymentSent]
          case "payment-settling-onchain" =>
            js.validate[WebSocketEvent.PaymentSettlingOnchain]
        }
    }

  implicit val onChainBalanceReads: Reads[OnChainBalance] =
    Json.reads[OnChainBalance]

  implicit val walletTransactionReads: Reads[WalletTransaction] =
    Json.reads[WalletTransaction]

  def jsToSatoshis(js: Value): Satoshis =
    js match {
      case str: Str =>
        Satoshis(BigInt(str.value))
      case num: Num =>
        Satoshis(num.value.toLong)
      case _: Value =>
        throw Value.InvalidData(js, "Expected value in Satoshis")
    }

  implicit val byteVectorReads: Reads[ByteVector] = {
    Reads { jsValue =>
      SerializerUtil.processJsStringOpt(str => ByteVector.fromHex(str))(jsValue)
    }
  }

  implicit val outputStatusReads: Reads[OutputStatus] = {
    Reads { jsValue =>
      SerializerUtil.processJsStringOpt(OutputStatus.fromStringOpt)(jsValue)
    }
  }

  implicit val connectionDirectionReads: Reads[ConnectionDirection] = {
    Reads { jsValue =>
      SerializerUtil.processJsStringOpt(ConnectionDirection.fromStringOpt)(
        jsValue)
    }
  }

  implicit val localOrRemoteReads: Reads[LocalOrRemote] = {
    Reads { jsValue =>
      SerializerUtil.processJsStringOpt(LocalOrRemote.fromStringOpt)(jsValue)
    }
  }

  implicit val invoiceStatusReads: Reads[InvoiceStatus] = {
    Reads { jsValue =>
      SerializerUtil.processJsStringOpt(InvoiceStatus.fromStringOpt)(jsValue)
    }
  }

  implicit val closedChannelTypeReads: Reads[ClosedChannelType] = {
    Reads { jsValue =>
      SerializerUtil.processJsStringOpt(ClosedChannelType.fromStringOpt)(
        jsValue)
    }
  }

  implicit val routeReads: Reads[Route] = {
    Reads { js =>
      for {
        amount <- (js \ "amount").validate[MilliSatoshis]
        nodeIds <- (js \ "nodeIds").validateOpt[Vector[NodeId]]
        shortChannelIds <- (js \ "shortChannelIds")
          .validateOpt[Vector[ShortChannelId]]
      } yield {
        if (shortChannelIds.nonEmpty) {
          ChannelRoute(amount, shortChannelIds.get)
        } else {
          NodeRoute(amount, nodeIds.getOrElse(Vector.empty))
        }
      }
    }

  }

  implicit val findRouteResultReads: Reads[Vector[Route]] = {
    Reads { js =>
      (js \ "routes").validate[JsArray].map(_.value.toVector.map(_.as[Route]))
    }
  }

}
