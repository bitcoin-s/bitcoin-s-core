package org.bitcoins.commons.serializers

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.RescanComplete
import org.bitcoins.commons.serializers.JsonReaders.jsToSatoshis
import org.bitcoins.core.api.chain.db.{CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, IncomingDLCOfferDb}
import org.bitcoins.core.api.wallet.{CoinSelectionAlgo, WalletInfo}
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.config.DLC
import org.bitcoins.core.crypto.*
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.dlc.accounting.DLCWalletAccounting
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.hd.{AddressType, HDAccount, HDPath, HDPurpose}
import org.bitcoins.core.number.{Int32, UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.dlc.models.DLCStatus.*
import org.bitcoins.core.protocol.dlc.models.*
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitness,
  ScriptWitnessV0,
  WitnessScriptPubKey
}
import org.bitcoins.core.protocol.tlv.*
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.core.util.{NetworkUtil, TimeUtil}
import org.bitcoins.core.util.TimeUtil.*
import org.bitcoins.core.util.sorted.OrderedSchnorrSignatures
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{AddressLabelTag, TxoState}
import org.bitcoins.crypto.*
import scodec.bits.ByteVector
import ujson.*
import upickle.default.*

import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.file.Path
import java.time.Instant
import java.util.Date
import scala.util.Try

object Picklers {

  implicit val pathPickler: ReadWriter[Path] =
    readwriter[String].bimap(_.toString, str => new File(str).toPath)

  implicit val inetSocketAddress: ReadWriter[InetSocketAddress] =
    readwriter[String].bimap(
      addr => s"${addr.getHostName}:${addr.getPort}",
      str => {
        val uri = new URI("tcp://" + str)
        InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
      }
    )

  implicit val byteVectorPickler: ReadWriter[ByteVector] =
    readwriter[String].bimap(_.toHex, str => ByteVector.fromValidHex(str))

  implicit val bitcoinAddressPickler: ReadWriter[BitcoinAddress] =
    readwriter[String]
      .bimap(_.value, BitcoinAddress.fromString)

  implicit val bitcoinsPickler: ReadWriter[Bitcoins] =
    readwriter[Double].bimap(_.toBigDecimal.toDouble, Bitcoins(_))

  implicit val satoshisPickler: ReadWriter[Satoshis] =
    readwriter[Long].bimap(_.toLong, Satoshis.apply)

  implicit val schnorrNoncePickler: ReadWriter[SchnorrNonce] =
    readwriter[String].bimap(_.hex, SchnorrNonce.fromHex)

  implicit val enumEventDescriptorPickler
      : ReadWriter[EnumEventDescriptorV0TLV] =
    readwriter[String].bimap(_.hex, EnumEventDescriptorV0TLV.fromHex)

  implicit val digitDecompEventDescriptorPickler
      : ReadWriter[DigitDecompositionEventDescriptorV0TLV] =
    readwriter[String].bimap(
      _.hex,
      DigitDecompositionEventDescriptorV0TLV.fromHex
    )

  implicit val eventDescriptorPickler: ReadWriter[EventDescriptorTLV] =
    readwriter[String].bimap(_.hex, EventDescriptorTLV.fromHex)

  implicit val oracleEventVoPickler: ReadWriter[OracleEventV0TLV] =
    readwriter[String].bimap(_.hex, OracleEventV0TLV.fromHex)

  implicit val instantPickler: ReadWriter[Instant] =
    readwriter[Long].bimap(_.getEpochSecond, Instant.ofEpochSecond)

  implicit val datePickler: ReadWriter[Date] =
    readwriter[String].bimap(TimeUtil.iso8601ToString, TimeUtil.iso8601ToDate)

  implicit val aesPasswordPickler: ReadWriter[AesPassword] =
    readwriter[String].bimap(_.toStringSensitive, AesPassword.fromString)

  implicit val sha256DigestBEPickler: ReadWriter[Sha256DigestBE] =
    readwriter[String].bimap(_.hex, Sha256DigestBE.fromHex)

  implicit val sha256DigestPickler: ReadWriter[Sha256Digest] =
    readwriter[String].bimap(_.hex, Sha256Digest.fromHex)

  implicit val doubleSha256DigestBEPickler: ReadWriter[DoubleSha256DigestBE] =
    readwriter[String].bimap(_.hex, DoubleSha256DigestBE.fromHex)

  implicit val uInt32Pickler: ReadWriter[UInt32] =
    readwriter[Long].bimap(_.toLong, long => UInt32(long))

  implicit val satoshisPerVirtualBytePickler
      : ReadWriter[SatoshisPerVirtualByte] =
    readwriter[Long]
      .bimap(_.toLong, long => SatoshisPerVirtualByte(Satoshis(long)))

  implicit val oracleInfoPickler: ReadWriter[OracleInfo] =
    readwriter[String].bimap(_.hex, OracleInfo.fromHex)

  implicit val oracleAnnouncementPickler: ReadWriter[OracleAnnouncementTLV] =
    readwriter[String].bimap(_.hex, OracleAnnouncementTLV.fromHex)

  implicit val contractInfoPickler: ReadWriter[ContractInfo] =
    readwriter[String].bimap(_.hex, ContractInfo.fromHex)

  implicit val contractInfoTLVPickler: ReadWriter[ContractInfoV0TLV] =
    readwriter[String].bimap(_.hex, ContractInfoV0TLV.fromHex)

  implicit val schnorrDigitalSignaturePickler
      : ReadWriter[SchnorrDigitalSignature] =
    readwriter[String].bimap(_.hex, SchnorrDigitalSignature.fromHex)

  implicit val partialSignaturePickler: ReadWriter[PartialSignature[?]] =
    readwriter[String].bimap(_.hex, PartialSignature.fromHex)

  implicit val lnMessageDLCOfferTLVPickler: ReadWriter[LnMessage[DLCOfferTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCOfferTLV).fromHex)

  implicit val txoStatePickler: ReadWriter[TxoState] = {
    readwriter[String].bimap(_.toString.toLowerCase, TxoState.fromString)
  }

  implicit val privKeyPathPickler: ReadWriter[HDPath] = {
    readwriter[String].bimap(_.toString, HDPath.fromString)
  }

  implicit val scriptPubKeyPickler: ReadWriter[ScriptPubKey] = {
    readwriter[String].bimap(_.asmHex, ScriptPubKey.fromAsmHex(_))
  }

  private def parseWitnessElements(arr: ujson.Arr): ScriptWitness = {
    val stackElements = arr.value.toVector.map {
      case obj: ujson.Obj =>
        val witnessStr = obj(PicklerKeys.witnessKey).str
        ByteVector.fromValidHex(witnessStr)
      case x: ujson.Value =>
        sys.error(s"Expected witness json object, got=$x")
    }

    ScriptWitness.apply(stackElements.reverse)
  }

  private def writeWitnessElements(witness: ScriptWitness): ujson.Arr = {
    val vec: Vector[ujson.Obj] = witness.stack.reverse.map { w =>
      ujson.Obj(PicklerKeys.witnessKey -> Str(w.toHex))
    }.toVector

    ujson.Arr.from(vec)
  }

  implicit val scriptWitnessPickler: ReadWriter[ScriptWitness] = {
    readwriter[Arr].bimap(writeWitnessElements, parseWitnessElements)
  }

  private def writeOutput(o: TransactionOutput): Obj = {
    Obj(
      PicklerKeys.satoshisKey -> writeJs(o.value.satoshis),
      PicklerKeys.scriptPubKeyKey -> writeJs(o.scriptPubKey)
    )
  }

  private def readOutput(obj: Obj): TransactionOutput = {
    val sats = Satoshis(obj(PicklerKeys.satoshisKey).num.toLong)
    val scriptPubKey =
      ScriptPubKey.fromAsmHex(obj(PicklerKeys.scriptPubKeyKey).str)
    TransactionOutput(sats, scriptPubKey)
  }

  implicit val txOutputPickler: ReadWriter[TransactionOutput] =
    readwriter[Obj].bimap(writeOutput, readOutput)

  private def writeSpendingInfoDb(si: SpendingInfoDb): Obj = {
    Obj(
      PicklerKeys.idKey -> {
        si.id match {
          case None     => ujson.Null
          case Some(id) => Num(id.toDouble)
        }
      },
      PicklerKeys.outPointKey -> writeJs(si.outPoint),
      PicklerKeys.outputKey -> writeJs(si.output),
      PicklerKeys.hdPathKey -> writeJs(si.privKeyPath),
      PicklerKeys.redeemScriptKey -> writeJs(si.redeemScriptOpt),
      PicklerKeys.witnessKey -> writeJs(si.scriptWitnessOpt),
      PicklerKeys.stateKey -> writeJs(si.state),
      PicklerKeys.txIdKey -> writeJs(si.txid),
      PicklerKeys.spendingTxIdKey -> writeJs(si.spendingTxIdOpt)
    )
  }

  private def readSpendingInfoDb(obj: Obj): SpendingInfoDb = {
    val id = obj(PicklerKeys.idKey).numOpt.map(_.toLong)
    val outpoint =
      upickle.default.read[TransactionOutPoint](obj(PicklerKeys.outPointKey))
    val output =
      upickle.default.read[TransactionOutput](obj(PicklerKeys.outputKey))
    val hdPath = upickle.default.read[HDPath](obj(PicklerKeys.hdPathKey))
    val redeemScript = upickle.default.read[Option[ScriptPubKey]](
      obj(PicklerKeys.redeemScriptKey)
    )
    val scriptWitness =
      upickle.default.read[Option[ScriptWitness]](obj(PicklerKeys.witnessKey))
    val state = upickle.default.read[TxoState](obj(PicklerKeys.stateKey))
    val txId =
      upickle.default.read[DoubleSha256DigestBE](obj(PicklerKeys.txIdKey))
    val spendingTxId = upickle.default.read[Option[DoubleSha256DigestBE]](
      obj(PicklerKeys.spendingTxIdKey)
    )
    SpendingInfoDb(
      id = id,
      outpoint = outpoint,
      output = output,
      hdPath = hdPath,
      redeemScriptOpt = redeemScript,
      scriptWitnessOpt = scriptWitness,
      state = state,
      txId = txId,
      spendingTxIdOpt = spendingTxId
    )
  }

  implicit val spendingInfoDbPickler: ReadWriter[SpendingInfoDb] = {
    readwriter[Obj].bimap(writeSpendingInfoDb, readSpendingInfoDb)
  }

  private def parseU64(str: ujson.Str): UInt64 = {
    UInt64(BigInt(str.str))
  }

  private def parseFundingInput(obj: ujson.Obj): FundingInputTLV = {
    val inputSerialId = parseU64(obj(PicklerKeys.inputSerialIdKey).str)
    val prevTx = Transaction.fromHex(obj(PicklerKeys.prevTxKey).str)
    val prevTxVout = obj(PicklerKeys.prevTxVoutKey).num.toLong
    val sequence = UInt32(obj(PicklerKeys.sequenceKey).num.toLong)
    val maxWitnessLen = UInt16(obj(PicklerKeys.maxWitnessLenKey).num.toLong)
    val redeemScriptStr = obj(PicklerKeys.redeemScriptKey).str
    val redeemScriptOpt = if (redeemScriptStr.nonEmpty) {
      val spk =
        WitnessScriptPubKey.fromAsmHex(obj(PicklerKeys.redeemScriptKey).str)
      Some(spk)
    } else {
      None
    }

    FundingInputV0TLV(
      inputSerialId,
      prevTx,
      UInt32(prevTxVout),
      sequence,
      maxWitnessLen,
      redeemScriptOpt
    )
  }

  private def parseFundingInputs(arr: ujson.Arr): Vector[FundingInputTLV] = {
    arr.value.toVector.map {
      case inputObj: ujson.Obj =>
        parseFundingInput(inputObj)
      case x: ujson.Value =>
        sys.error(s"Expected obj, got=$x")
    }
  }

  private def parseCetAdaptorSignatures(obj: ujson.Obj): CETSignaturesTLV = {
    val ecAdaptorSignaturesArr = obj(PicklerKeys.ecdsaAdaptorSignaturesKey).arr
    val adaptorSigs = parseAdaptorSignatures(ecAdaptorSignaturesArr)
    CETSignaturesV0TLV(adaptorSigs)
  }

  private def parseAdaptorSignatures(
      arr: ujson.Arr
  ): Vector[ECAdaptorSignature] = {
    arr.value.toVector.map {
      case obj: ujson.Obj =>
        ECAdaptorSignature.fromHex(obj(PicklerKeys.signatureKey).str)
      case x: ujson.Value =>
        sys.error(s"Excpected string for ecdsa adaptor siganture, got obj=$x")
    }
  }

  private def writeAdaptorSignatures(
      sigs: Vector[ECAdaptorSignature]
  ): Vector[ujson.Obj] = {
    sigs.map { sig =>
      ujson.Obj(PicklerKeys.signatureKey -> Str(sig.hex))
    }
  }

  private def writeCetAdaptorSigs(
      cetSignaturesTLV: CETSignaturesTLV
  ): ujson.Obj = {
    cetSignaturesTLV match {
      case v0: CETSignaturesV0TLV =>
        val sigsVec = writeAdaptorSignatures(v0.sigs)
        ujson.Obj(
          PicklerKeys.ecdsaAdaptorSignaturesKey -> ujson.Arr.from(sigsVec)
        )
    }
  }

  private def readAcceptTLV(obj: ujson.Obj): DLCAcceptTLV = {
    val tempContractId =
      Sha256Digest.fromHex(obj(PicklerKeys.tempContractIdKey).str)
    val acceptCollateral = Satoshis(
      obj(PicklerKeys.acceptCollateralKey).num.toLong
    )
    val fundingPubKey =
      ECPublicKey.fromHex(obj(PicklerKeys.fundingPubKeyKey).str)
    val payoutSpk = ScriptPubKey.fromAsmHex(obj(PicklerKeys.payoutSpkKey).str)
    val payoutSerialId = parseU64(obj(PicklerKeys.payoutSerialIdKey).str)
    val fundingInputs = parseFundingInputs(
      obj(PicklerKeys.fundingInputsKey).arr
    )
    val changeSpk = ScriptPubKey.fromAsmHex(obj(PicklerKeys.changeSpkKey).str)
    val changeSerialId = parseU64(obj(PicklerKeys.changeSerialIdKey).str)
    val cetAdaptorSigs = parseCetAdaptorSignatures(
      obj(PicklerKeys.cetAdaptorSignaturesKey).obj
    )
    val refundSignature =
      ECDigitalSignature.fromHex(obj(PicklerKeys.refundSignatureKey).str)
    val negotiationFields = {
      obj(PicklerKeys.negotiationFieldsKey).strOpt match {
        case Some(str) =>
          sys.error(s"Don't know how to parse negotiation fields, got=$str")
        case None => NegotiationFieldsTLV.empty
      }
    }

    val acceptTLV = DLCAcceptTLV(
      tempContractId = tempContractId,
      acceptCollateralSatoshis = acceptCollateral,
      fundingPubKey = fundingPubKey,
      payoutSPK = payoutSpk,
      payoutSerialId = payoutSerialId,
      fundingInputs = fundingInputs,
      changeSPK = changeSpk,
      changeSerialId = changeSerialId,
      cetSignatures = cetAdaptorSigs,
      refundSignature = refundSignature,
      negotiationFields = negotiationFields
    )

    acceptTLV
  }

  private def writeAcceptTLV(accept: DLCAcceptTLV): ujson.Obj = {
    Obj(
      PicklerKeys.tempContractIdKey -> Str(accept.tempContractId.hex),
      PicklerKeys.acceptCollateralKey -> Num(
        accept.acceptCollateralSatoshis.toLong.toDouble
      ),
      PicklerKeys.fundingPubKeyKey -> Str(accept.fundingPubKey.hex),
      PicklerKeys.payoutSpkKey -> Str(accept.payoutSPK.asmHex),
      PicklerKeys.payoutSerialIdKey -> Str(
        accept.payoutSerialId.toBigInt.toString()
      ),
      PicklerKeys.fundingInputsKey -> writeJs(accept.fundingInputs),
      PicklerKeys.changeSpkKey -> Str(accept.changeSPK.asmHex),
      PicklerKeys.changeSerialIdKey -> Str(
        accept.changeSerialId.toBigInt.toString()
      ),
      PicklerKeys.cetAdaptorSignaturesKey -> writeCetAdaptorSigs(
        accept.cetSignatures
      ),
      PicklerKeys.refundSignatureKey -> Str(accept.refundSignature.hex),
      PicklerKeys.negotiationFieldsKey -> ujson.Null
    )
  }

  private def parseFundingSignatures(obj: ujson.Obj): FundingSignaturesTLV = {
    val fundingSignatures: Vector[ujson.Value] = obj(
      PicklerKeys.fundingSignaturesKey
    ).arr.toVector
    val witV0 = paresFundingSignaturesArr(fundingSignatures)
    FundingSignaturesV0TLV(witV0)
  }

  private def paresFundingSignaturesArr(
      arr: Vector[ujson.Value]
  ): Vector[ScriptWitnessV0] = {
    arr.map {
      case obj: ujson.Obj =>
        val witnessElementsArr = obj(PicklerKeys.witnessElementsKey).arr
        val witness: ScriptWitness = {
          parseWitnessElements(witnessElementsArr)
        }

        val scriptWitnessV0 = witness.asInstanceOf[ScriptWitnessV0]
        scriptWitnessV0
      case x =>
        sys.error(s"Expected array of objects for funding signatures, got=$x")
    }
  }

  private def writeFundingSignatures(
      fundingSigs: FundingSignaturesTLV
  ): ujson.Obj = {
    val sigs: Vector[ujson.Obj] = fundingSigs match {
      case v0: FundingSignaturesV0TLV =>
        val witnessJson: Vector[Obj] = {
          v0.witnesses.map { wit =>
            val witJson = writeWitnessElements(wit)
            ujson.Obj(PicklerKeys.witnessElementsKey -> witJson)
          }
        }
        witnessJson
    }
    ujson.Obj(
      PicklerKeys.fundingSignaturesKey -> ujson.Arr.from(sigs)
    )
  }

  private def readSignTLV(obj: ujson.Obj): DLCSignTLV = {
    val contractId = ByteVector.fromValidHex(obj(PicklerKeys.contractIdKey).str)
    val adaptorSigs = parseCetAdaptorSignatures(
      obj(PicklerKeys.cetAdaptorSignaturesKey).obj
    )
    val refundSignature =
      ECDigitalSignature.fromHex(obj(PicklerKeys.refundSignatureKey).str)
    val fundingSignatures = parseFundingSignatures(
      obj(PicklerKeys.fundingSignaturesKey).obj
    )

    val signTLV =
      DLCSignTLV(contractId, adaptorSigs, refundSignature, fundingSignatures)

    signTLV

  }

  private def writeSignTLV(sign: DLCSignTLV): ujson.Obj = {
    ujson.Obj(
      PicklerKeys.contractIdKey -> sign.contractId.toHex,
      PicklerKeys.cetAdaptorSignaturesKey -> writeCetAdaptorSigs(
        sign.cetSignatures
      ),
      PicklerKeys.refundSignatureKey -> ujson.Str(sign.refundSignature.hex),
      PicklerKeys.fundingSignaturesKey ->
        writeFundingSignatures(sign.fundingSignatures)
    )
  }

  implicit val dlcAcceptTLVPickler: ReadWriter[DLCAcceptTLV] = {
    readwriter[ujson.Obj].bimap(writeAcceptTLV, readAcceptTLV)
  }

  implicit val dlcSignTLVPickler: ReadWriter[DLCSignTLV] = {
    readwriter[ujson.Obj].bimap(writeSignTLV, readSignTLV)
  }

  implicit val lnMessageDLCAcceptTLVPickler
      : ReadWriter[LnMessage[DLCAcceptTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCAcceptTLV).fromHex)

  implicit val lnMessageDLCSignTLVPickler: ReadWriter[LnMessage[DLCSignTLV]] =
    readwriter[String].bimap(_.hex, LnMessageFactory(DLCSignTLV).fromHex)

  implicit val blockStampPickler: ReadWriter[BlockStamp] =
    readwriter[String].bimap(_.mkString, BlockStamp.fromString)

  implicit val psbtPickler: ReadWriter[PSBT] =
    readwriter[String].bimap(_.base64, PSBT.fromString)

  implicit val transactionPickler: ReadWriter[Transaction] =
    readwriter[String].bimap(_.hex, Transaction.fromHex)

  implicit val blockPickler: ReadWriter[Block] = {
    readwriter[String].bimap(_.hex, Block.fromHex)
  }

  implicit val extPubKeyPickler: ReadWriter[ExtPublicKey] =
    readwriter[String].bimap(_.toString, ExtPublicKey.fromString)

  implicit val transactionOutPointPickler: ReadWriter[TransactionOutPoint] =
    readwriter[String].bimap(_.hex, TransactionOutPoint.fromHex)

  implicit val coinSelectionAlgoPickler: ReadWriter[CoinSelectionAlgo] =
    readwriter[String].bimap(_.toString, CoinSelectionAlgo.fromString)

  implicit val addressLabelTagPickler: ReadWriter[AddressLabelTag] =
    readwriter[String].bimap(_.name, AddressLabelTag.apply)

  implicit val lockUnspentOutputParameterPickler
      : ReadWriter[LockUnspentOutputParameter] =
    readwriter[Value].bimap(_.toJson, LockUnspentOutputParameter.fromJson)

  // can't make implicit because it will overlap with ones needed for cli
  val announcementV0JsonWriter: Writer[OracleAnnouncementV0TLV] =
    writer[Obj].comap { announcement =>
      val noncesJson = announcement.eventTLV.nonces.map { nonce =>
        Str(nonce.hex)
      }

      val descriptorJson = announcement.eventTLV.eventDescriptor match {
        case EnumEventDescriptorV0TLV(outcomes) =>
          Obj(
            "outcomes" -> outcomes.map(Str(_)),
            "hex" -> announcement.eventTLV.eventDescriptor.hex
          )
        case numeric: NumericEventDescriptorTLV =>
          Obj(
            "base" -> Num(numeric.base.toLong.toDouble),
            "isSigned" -> Bool(numeric.isSigned),
            "unit" -> Str(numeric.unit),
            "precision" -> Num(numeric.precision.toLong.toDouble),
            "hex" -> announcement.eventTLV.eventDescriptor.hex
          )
      }

      val maturityStr =
        TimeUtil.iso8601ToString(Date.from(announcement.eventTLV.maturation))

      val eventJson = Obj(
        "nonces" -> noncesJson,
        "maturity" -> Str(maturityStr),
        "descriptor" -> descriptorJson,
        "eventId" -> Str(announcement.eventTLV.eventId)
      )

      Obj(
        "announcementSignature" -> Str(announcement.announcementSignature.hex),
        "publicKey" -> Str(announcement.publicKey.hex),
        "event" -> eventJson,
        "hex" -> announcement.hex
      )
    }

  // can't make implicit because it will overlap with ones needed for cli
  val oracleAnnouncementTLVJsonWriter: Writer[OracleAnnouncementTLV] =
    writer[Value].comap { case v0: OracleAnnouncementV0TLV =>
      writeJs(v0)(using announcementV0JsonWriter)
    }

  // can't make implicit because it will overlap with ones needed for cli
  val oracleAttestmentV0Writer: Writer[OracleAttestmentV0TLV] =
    writer[Obj].comap { attestments =>
      val sigsJson = attestments.sigs.map(sig => Str(sig.hex))
      val valuesJson = attestments.outcomes.map(Str(_))

      Obj(
        "eventId" -> Str(attestments.eventId),
        "signatures" -> sigsJson,
        "values" -> valuesJson,
        "hex" -> attestments.hex
      )
    }

  implicit val fundingInputV0Writer: Writer[FundingInputTLV] =
    writer[Value].comap { case v0: FundingInputV0TLV =>
      writeJs(v0)(using fundingInputWriter)
    }

  implicit val fundingInputWriter: Writer[FundingInputV0TLV] =
    writer[Obj].comap { input =>
      import input._

      val redeemScriptJson = redeemScriptOpt match {
        case Some(rs) => Str(rs.hex)
        case None     => Str("")
      }

      Obj(
        "inputSerialId" -> Str(inputSerialId.toBigInt.toString()),
        "prevTx" -> Str(prevTx.hex),
        "prevTxVout" -> Num(prevTxVout.toLong.toDouble),
        "sequence" -> Num(sequence.toLong.toDouble),
        "maxWitnessLen" -> Num(maxWitnessLen.toLong.toDouble),
        "redeemScript" -> redeemScriptJson
      )
    }

  implicit val tlvPointReader: Reader[TLVPoint] = {
    reader[Obj].map { (obj: Obj) =>
      val map = obj.value
      val outcome = map(PicklerKeys.outcomeKey).num.toLong
      val payout = jsToSatoshis(map(PicklerKeys.payoutKey))
      val extraPrecision = map(PicklerKeys.extraPrecisionKey).num.toInt
      TLVPoint(outcome, payout, extraPrecision)
    }
  }

  implicit val tlvPointWriter: Writer[TLVPoint] = {
    writer[Obj].comap { point =>
      Obj(
        PicklerKeys.outcomeKey -> Num(point.outcome.toDouble),
        PicklerKeys.payoutKey -> Num(point.value.toLong.toDouble),
        PicklerKeys.extraPrecisionKey -> Num(point.extraPrecision.toDouble)
      )
    }
  }

  implicit val hyperbolaPayoutCurvePieceTLVWriter
      : Writer[HyperbolaPayoutCurvePieceTLV] = {
    writer[Obj].comap { piece =>
      Obj(
        PicklerKeys.usePositivePiece -> Bool(piece.usePositivePiece),
        PicklerKeys.translateOutcome -> Num(
          piece.translateOutcome.toBigDecimal.toDouble
        ),
        PicklerKeys.translatePayout -> Num(
          piece.translatePayout.toBigDecimal.toDouble
        ),
        PicklerKeys.a -> Num(piece.a.toBigDecimal.toDouble),
        PicklerKeys.b -> Num(piece.b.toBigDecimal.toDouble),
        PicklerKeys.c -> Num(piece.c.toBigDecimal.toDouble),
        PicklerKeys.d -> Num(piece.d.toBigDecimal.toDouble)
      )
    }

  }

  implicit val payoutFunctionV0TLVWriter: Writer[PayoutFunctionV0TLV] = {
    def endpoint(json: Value, isEndpoint: Boolean): Value = json match {
      case obj: Obj =>
        // drop old value on the floor if there is one
        obj.value.put(PicklerKeys.isEndpointKey, Bool(isEndpoint))
        Obj(obj.value)
      case v: Value => v
    }

    writer[Obj].comap { payoutFunc =>
      val endPointsJs = payoutFunc.endpoints.map { point =>
        endpoint(writeJs(point), isEndpoint = true)
      }

      val midPointJs = payoutFunc.pieces.flatMap {
        case polynomialPiece: PolynomialPayoutCurvePieceTLV =>
          polynomialPiece.midpoints.map { point =>
            endpoint(writeJs(point), isEndpoint = false)
          }
        case hyperbolaPiece: HyperbolaPayoutCurvePieceTLV =>
          Vector(writeJs(hyperbolaPiece))
      }

      val points =
        (endPointsJs ++ midPointJs).sortBy(_.obj(PicklerKeys.outcomeKey).num)
      Obj(PicklerKeys.pointsKey -> points)
    }
  }

  implicit val payoutFunctionV0TLVReader: Reader[PayoutFunctionV0TLV] = {
    reader[Obj].map { (obj: Obj) =>
      val pointsArr = obj(PicklerKeys.pointsKey).arr
      val points: Vector[TLVPoint] = pointsArr.map {
        case x @ (_: Arr | _: Num | Null | _: Bool | _: Str) =>
          sys.error(
            s"Cannot have $x when parsing payout curve points, expected json object"
          )
        case obj: Obj =>
          upickle.default.read[TLVPoint](obj)
      }.toVector

      DLCPayoutCurve
        .fromPoints(points, serializationVersion = DLCSerializationVersion.Beta)
        .toTLV
    }
  }

  implicit val roundingIntervalsV0TLVWriter: Writer[RoundingIntervalsV0TLV] =
    writer[Obj].comap { roundingIntervals =>
      import roundingIntervals._

      val intervalsJs = intervalStarts.map { i =>
        Obj(
          "beginInterval" -> Num(i._1.toDouble),
          "roundingMod" -> Num(i._2.toLong.toDouble)
        )
      }

      Obj("intervals" -> intervalsJs)
    }

  implicit val contractDescriptorV0: ReadWriter[ContractDescriptorV0TLV] = {
    readwriter[Value].bimap(contractV0Writer, contractV0Reader)
  }

  private def contractV0Reader(value: Value): ContractDescriptorV0TLV = {
    parseContractDescriptor(value)
  }

  private def contractV0Writer(v0: ContractDescriptorV0TLV): Value = {
    val outcomesJs: ujson.Obj = v0.outcomes.map { case (outcome, payout) =>
      outcome -> Num(payout.toLong.toDouble)
    }
    Obj(PicklerKeys.outcomesKey -> outcomesJs, "hex" -> v0.hex)
  }

  implicit val contractDescriptorV1Writer: Writer[ContractDescriptorV1TLV] =
    writer[Obj].comap { v1 =>
      import v1._

      Obj(
        "numDigits" -> Num(numDigits.toDouble),
        "payoutFunction" -> writeJs(payoutFunction),
        "roundingIntervals" -> writeJs(roundingIntervals),
        "hex" -> v1.hex
      )
    }

  implicit val contractDescriptorWriter: Writer[ContractDescriptorTLV] =
    writer[Value].comap {
      case v0: ContractDescriptorV0TLV =>
        writeJs(v0)(using contractDescriptorV0)
      case v1: ContractDescriptorV1TLV =>
        writeJs(v1)(using contractDescriptorV1Writer)
    }

  implicit val oracleInfoV0TLVWriter: Writer[OracleInfoV0TLV] =
    writer[Obj].comap { oracleInfo =>
      Obj(
        "announcement" -> writeJs(oracleInfo.announcement)(
          using oracleAnnouncementTLVJsonWriter
        )
      )
    }

  implicit val oracleInfoV1TLVWriter: Writer[OracleInfoV1TLV] =
    writer[Obj].comap { oracleInfo =>
      import oracleInfo._
      Obj(
        "threshold" -> Num(threshold.toDouble),
        "announcements" -> oracles.map(o =>
          writeJs(o)(using oracleAnnouncementTLVJsonWriter))
      )
    }

  implicit val oracleParamsV0TLVWriter: Writer[OracleParamsV0TLV] =
    writer[Obj].comap { params =>
      import params._
      Obj(
        "maxErrorExp" -> Num(maxErrorExp.toDouble),
        "minFailExp" -> Num(minFailExp.toDouble),
        "maximizeCoverage" -> Bool(maximizeCoverage)
      )
    }

  implicit val oracleParamsTLVWriter: Writer[OracleParamsTLV] =
    writer[Value].comap { case v0: OracleParamsV0TLV =>
      writeJs(v0)
    }

  implicit val oracleInfoV2TLVWriter: Writer[OracleInfoV2TLV] =
    writer[Obj].comap { oracleInfo =>
      import oracleInfo._
      Obj(
        "threshold" -> Num(threshold.toDouble),
        "announcements" -> oracles.map(o =>
          writeJs(o)(using oracleAnnouncementTLVJsonWriter)),
        "params" -> writeJs(params)
      )
    }

  implicit val oracleInfoTLVWriter: Writer[OracleInfoTLV] =
    writer[Value].comap {
      case v0: OracleInfoV0TLV => writeJs(v0)
      case v1: OracleInfoV1TLV => writeJs(v1)
      case v2: OracleInfoV2TLV => writeJs(v2)
    }

  // can't make implicit because it will overlap with ones needed for cli
  val contractInfoV0TLVJsonWriter: Writer[ContractInfoV0TLV] =
    writer[Obj].comap { contractInfo =>
      import contractInfo._

      Obj(
        PicklerKeys.totalCollateralKey -> writeJs(totalCollateral),
        PicklerKeys.contractDescriptorKey -> writeJs(contractDescriptor),
        PicklerKeys.oracleInfoKey -> writeJs(oracleInfo)
      )
    }

  val contractInfoV1TLVJsonWriter: Writer[ContractInfoV1TLV] = {
    writer[Obj].comap { contractInfo =>
      val arrayVec: Vector[ujson.Obj] = contractInfo.contractOraclePairs.map {
        case (c, o) =>
          val contractDescriptorJson = writeJs(c)
          val oracleInfoJson = writeJs(o)
          ujson.Obj(
            PicklerKeys.contractDescriptorKey -> contractDescriptorJson,
            PicklerKeys.oracleInfoKey -> oracleInfoJson
          )
      }

      val arrayJson = ujson.Arr.from(arrayVec)

      Obj(
        PicklerKeys.totalCollateralKey -> Num(
          contractInfo.totalCollateral.toLong.toDouble
        ),
        PicklerKeys.pairsKey -> arrayJson
      )
    }
  }

  val contractInfoJsonWriter: Writer[ContractInfoTLV] = {
    writer[ujson.Value].comap {
      case contractInfoV0TLV: ContractInfoV0TLV =>
        writeJs(contractInfoV0TLV)(using contractInfoV0TLVJsonWriter)
      case contractInfoV1TLV: ContractInfoV1TLV =>
        writeJs(contractInfoV1TLV)(using contractInfoV1TLVJsonWriter)
    }
  }

  implicit val offerTLVWriter: Writer[DLCOfferTLV] =
    writer[Obj].comap { offer =>
      import offer._
      Obj(
        "contractFlags" -> Str(ByteVector.fromByte(contractFlags).toHex),
        "chainHash" -> Str(chainHash.hex),
        "contractInfo" -> writeJs(contractInfo)(using contractInfoJsonWriter),
        "fundingPubKey" -> Str(fundingPubKey.hex),
        "payoutSPK" -> Str(payoutSPK.hex),
        "payoutSerialId" -> Num(payoutSerialId.toBigInt.toDouble),
        "offerCollateral" -> Num(offererCollateralSatoshis.toLong.toDouble),
        "fundingInputs" -> fundingInputs.map(i => writeJs(i)),
        "changeSPK" -> Str(changeSPK.hex),
        "changeSerialId" -> Str(changeSerialId.toBigInt.toString()),
        "fundOutputSerialId" -> Num(fundOutputSerialId.toBigInt.toDouble),
        "feeRatePerVb" -> Num(feeRate.toLong.toDouble),
        "cetLocktime" -> Num(contractMaturityBound.toUInt32.toLong.toDouble),
        "refundLocktime" -> Num(contractTimeout.toUInt32.toLong.toDouble),
        PicklerKeys.tempContractIdKey -> Str(offer.tempContractId.hex)
      )
    }

  implicit val payoutAddressW: Writer[PayoutAddress] = writer[Obj].comap {
    payoutAddress =>
      import payoutAddress._
      Obj(
        "address" -> Str(address.toString),
        "isExternal" -> Bool(isExternal)
      )
  }

  implicit val optionPayoutAddressW: Writer[Option[PayoutAddress]] =
    writer[Value].comap { payoutAddressOpt =>
      payoutAddressOpt
        .map(pa => writeJs(pa))
        .getOrElse(ujson.Null)
    }

  implicit val optionContactDbW: Writer[Option[DLCContactDb]] =
    writer[Value].comap {
      case Some(contact) => writeContactDb(contact)
      case None          => ujson.Null
    }

  implicit val offeredW: Writer[Offered] =
    writer[Obj].comap { offered =>
      import offered._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble
        ),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble
        ),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val acceptedComputingAdaptorSigsW
      : Writer[AcceptedComputingAdaptorSigs] = writer[Obj].comap { accepted =>
    import accepted._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble
      ),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble
      ),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val acceptedW: Writer[Accepted] = writer[Obj].comap { accepted =>
    import accepted._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble
      ),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble
      ),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val signedComputingAdaptorSigsW: Writer[SignedComputingAdaptorSigs] =
    writer[Obj].comap { signed =>
      import signed._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble
        ),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble
        ),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val signedW: Writer[Signed] = writer[Obj].comap { signed =>
    import signed._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble
      ),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble
      ),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val broadcastedW: Writer[Broadcasted] =
    writer[Obj].comap { broadcasted =>
      import broadcasted._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble
        ),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble
        ),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val confirmedW: Writer[Confirmed] =
    writer[Obj].comap { confirmed =>
      import confirmed._
      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble
        ),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble
        ),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  private val counterPartyPayoutKey: String = "counterPartyPayout"

  implicit val claimedW: Writer[Claimed] = writer[Obj].comap { claimed =>
    import claimed._
    val (oraclesJs, outcomesJs) = oracleOutcome match {
      case EnumOracleOutcome(oracles, outcome) =>
        (
          Arr.from(oracles.map(o => Str(o.announcement.hex))),
          Str(outcome.outcome)
        )
      case numeric: NumericOracleOutcome =>
        (
          Arr.from(numeric.oracles.map(_.announcement.hex)),
          Arr.from(numeric.outcomes.map(o => Arr.from(o.digits)))
        )
    }

    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble
      ),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble
      ),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex),
      "closingTxId" -> Str(closingTxId.hex),
      "oracleSigs" -> oracleSigs.map(sig => Str(sig.hex)),
      "outcomes" -> outcomesJs,
      "oracles" -> oraclesJs,
      PicklerKeys.myPayout -> Num(claimed.myPayout.satoshis.toLong.toDouble),
      counterPartyPayoutKey -> Num(
        claimed.counterPartyPayout.satoshis.toLong.toDouble
      ),
      PicklerKeys.pnl -> Num(claimed.pnl.satoshis.toLong.toDouble),
      PicklerKeys.rateOfReturn -> Num(claimed.rateOfReturn.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val remoteClaimedW: Writer[RemoteClaimed] =
    writer[Obj].comap { remoteClaimed =>
      import remoteClaimed._
      val (oraclesJs, outcomesJs) = oracleOutcome match {
        case EnumOracleOutcome(oracles, outcome) =>
          (
            Arr.from(oracles.map(o => Str(o.announcement.hex))),
            Str(outcome.outcome)
          )
        case numeric: NumericOracleOutcome =>
          (
            Arr.from(numeric.oracles.map(_.announcement.hex)),
            Arr.from(numeric.outcomes.map(o => Arr.from(o.digits)))
          )
      }

      Obj(
        "state" -> Str(statusString),
        "dlcId" -> Str(dlcId.hex),
        "isInitiator" -> Bool(isInitiator),
        "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
        PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
        "contractId" -> Str(contractId.toHex),
        "contractInfo" -> Str(contractInfo.hex),
        "contractMaturity" -> Num(
          timeouts.contractMaturity.toUInt32.toLong.toDouble
        ),
        "contractTimeout" -> Num(
          timeouts.contractTimeout.toUInt32.toLong.toDouble
        ),
        "feeRate" -> Num(feeRate.toLong.toDouble),
        "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
        "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
        "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
        "fundingTxId" -> Str(fundingTxId.hex),
        "closingTxId" -> Str(closingTxId.hex),
        "oracleSigs" -> oracleSigs.map(sig => Str(sig.hex)),
        "outcomes" -> outcomesJs,
        "oracles" -> oraclesJs,
        PicklerKeys.myPayout -> Num(
          remoteClaimed.myPayout.satoshis.toLong.toDouble
        ),
        counterPartyPayoutKey -> Num(
          remoteClaimed.counterPartyPayout.satoshis.toLong.toDouble
        ),
        PicklerKeys.pnl -> Num(remoteClaimed.pnl.satoshis.toLong.toDouble),
        PicklerKeys.rateOfReturn -> Num(remoteClaimed.rateOfReturn.toDouble),
        "payoutAddress" -> writeJs(payoutAddress),
        "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
      )
    }

  implicit val refundedW: Writer[Refunded] = writer[Obj].comap { refunded =>
    import refunded._
    Obj(
      "state" -> Str(statusString),
      "dlcId" -> Str(dlcId.hex),
      "isInitiator" -> Bool(isInitiator),
      "lastUpdated" -> Str(iso8601ToString(lastUpdated)),
      PicklerKeys.tempContractIdKey -> Str(tempContractId.hex),
      "contractId" -> Str(contractId.toHex),
      "contractInfo" -> Str(contractInfo.hex),
      "contractMaturity" -> Num(
        timeouts.contractMaturity.toUInt32.toLong.toDouble
      ),
      "contractTimeout" -> Num(
        timeouts.contractTimeout.toUInt32.toLong.toDouble
      ),
      "feeRate" -> Num(feeRate.toLong.toDouble),
      "totalCollateral" -> Num(totalCollateral.satoshis.toLong.toDouble),
      "localCollateral" -> Num(localCollateral.satoshis.toLong.toDouble),
      "remoteCollateral" -> Num(remoteCollateral.satoshis.toLong.toDouble),
      "fundingTxId" -> Str(fundingTxId.hex),
      "closingTxId" -> Str(closingTxId.hex),
      PicklerKeys.myPayout -> Num(refunded.myPayout.satoshis.toLong.toDouble),
      counterPartyPayoutKey -> Num(
        refunded.counterPartyPayout.satoshis.toLong.toDouble
      ),
      PicklerKeys.pnl -> Num(refunded.pnl.satoshis.toLong.toDouble),
      PicklerKeys.rateOfReturn -> Num(refunded.rateOfReturn.toDouble),
      "payoutAddress" -> writeJs(payoutAddress),
      "peer" -> peer.map(p => writeJs(p)).getOrElse(Null)
    )
  }

  implicit val dlcOfferAddW: Writer[IncomingDLCOfferDb] = writer[Obj].comap {
    offerDb =>
      Obj(
        "hash" -> offerDb.hash.hex,
        "receivedAt" -> Num(offerDb.receivedAt.getEpochSecond.toDouble),
        "peer" -> offerDb.peer.map(Str.apply).getOrElse(Null),
        "message" -> offerDb.message.map(Str.apply).getOrElse(Null),
        "offerTLV" -> offerDb.offerTLV.hex
      )
  }

  implicit val dlcOfferRemoveW: Writer[Sha256Digest] = writer[Value].comap {
    offerHash => writeJs(offerHash.hex)
  }

  implicit val dlcStatusW: Writer[DLCStatus] = writer[Value].comap {
    case o: Offered =>
      writeJs(o)(using offeredW)
    case a: AcceptedComputingAdaptorSigs =>
      writeJs(a)(using acceptedComputingAdaptorSigsW)
    case a: Accepted =>
      writeJs(a)(using acceptedW)
    case s: SignedComputingAdaptorSigs =>
      writeJs(s)(using signedComputingAdaptorSigsW)
    case s: Signed =>
      writeJs(s)(using signedW)
    case b: Broadcasted =>
      writeJs(b)(using broadcastedW)
    case c: Confirmed =>
      writeJs(c)(using confirmedW)
    case c: Claimed =>
      writeJs(c)(using claimedW)
    case r: RemoteClaimed =>
      writeJs(r)(using remoteClaimedW)
    case r: Refunded =>
      writeJs(r)(using refundedW)
  }

  implicit val dlcOfferAddR: Reader[IncomingDLCOfferDb] = reader[Obj].map {
    obj =>
      val hash = Sha256Digest(obj("hash").str)
      val peer = Try(obj("peer").str).toOption
      val message = Try(obj("message").str).toOption
      val receivedAt = Instant.ofEpochSecond(obj("receivedAt").num.toLong)
      val offerTLV = DLCOfferTLV.fromHex(obj("offerTLV").str)
      IncomingDLCOfferDb(
        hash = hash,
        peer = peer,
        message = message,
        receivedAt = receivedAt,
        offerTLV = offerTLV
      )
  }

  implicit val dlcOfferRemoveR: Reader[Sha256Digest] =
    reader[Value].map { obj => Sha256Digest(obj.str) }

  implicit val dlcStatusR: Reader[DLCStatus] = reader[Obj].map { obj =>
    val dlcId = Sha256Digest(obj("dlcId").str)
    val state = DLCState.fromString(obj("state").str)
    val lastUpdated = iso8601ToInstant(obj("lastUpdated").str)
    val isInitiator = obj("isInitiator").bool
    val tempContractId = Sha256Digest(obj(PicklerKeys.tempContractIdKey).str)
    val contractInfoTLV = ContractInfoV0TLV(obj("contractInfo").str)
    val contractMaturity =
      BlockStamp(UInt32(obj("contractMaturity").num.toLong))
    val contractTimeout = BlockStamp(UInt32(obj("contractTimeout").num.toLong))
    val feeRate = SatoshisPerVirtualByte.fromLong(obj("feeRate").num.toLong)
    val totalCollateral = Satoshis(obj("totalCollateral").num.toLong)
    val localCollateral = Satoshis(obj("localCollateral").num.toLong)
    val peerOpt =
      if (obj("peer").isNull || !obj.value.contains("peer")) None
      else Some(obj("peer").str)

    lazy val contractId = ByteVector.fromValidHex(obj("contractId").str)
    lazy val fundingTxId = DoubleSha256DigestBE(obj("fundingTxId").str)
    lazy val closingTxId = DoubleSha256DigestBE(obj("closingTxId").str)
    lazy val oracleSigs = {
      val unsorted = obj("oracleSigs").arr
        .map(value => SchnorrDigitalSignature(value.str))
        .toVector
      OrderedSchnorrSignatures.fromUnsorted(unsorted)
    }

    val payoutAddressJs = obj("payoutAddress")
    lazy val payoutAddress: Option[PayoutAddress] = payoutAddressJs match {
      case json: Obj =>
        json("address").strOpt.map(a =>
          PayoutAddress(
            BitcoinAddress.fromString(a),
            json("isExternal").boolOpt.getOrElse(false)
          ))
      case Null => None
      case v: Value =>
        throw new IllegalArgumentException(s"Unexpected payout address $v")

    }

    lazy val outcomesJs = obj("outcomes")
    lazy val outcomes = outcomesJs.strOpt match {
      case Some(value) => Vector(EnumOutcome(value))
      case None =>
        outcomesJs.arr.map { outcomeJs =>
          val digits = outcomeJs.arr.map(value => value.num.toInt)
          UnsignedNumericOutcome(digits.toVector)
        }.toVector
    }

    lazy val oraclesJs = obj("oracles")
    lazy val oracles = oraclesJs.arr.map { value =>
      val announcementTLV = OracleAnnouncementTLV(value.str)
      SingleOracleInfo(announcementTLV)
    }.toVector

    lazy val oracleOutcome = outcomes.head match {
      case outcome: EnumOutcome =>
        EnumOracleOutcome(
          oracles.asInstanceOf[Vector[EnumSingleOracleInfo]],
          outcome
        )
      case UnsignedNumericOutcome(_) =>
        val numericOutcomes =
          outcomes.map(_.asInstanceOf[UnsignedNumericOutcome])
        val numericOracles =
          oracles.map(_.asInstanceOf[NumericSingleOracleInfo])
        NumericOracleOutcome(numericOracles.zip(numericOutcomes))
    }

    lazy val myPayoutJs = obj(PicklerKeys.myPayout)
    lazy val myPayoutOpt = myPayoutJs.numOpt.map(sats => Satoshis(sats.toLong))
    lazy val theirPayoutJs = obj(counterPartyPayoutKey)
    lazy val theirPayoutOpt =
      theirPayoutJs.numOpt.map(sats => Satoshis(sats.toLong))

    state match {
      case DLCState.Offered =>
        Offered(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          payoutAddress,
          peerOpt
        )
      case DLCState.AcceptComputingAdaptorSigs =>
        AcceptedComputingAdaptorSigs(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          payoutAddress,
          peerOpt
        )
      case DLCState.Accepted =>
        Accepted(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          payoutAddress,
          peerOpt
        )
      case DLCState.SignComputingAdaptorSigs =>
        SignedComputingAdaptorSigs(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Signed =>
        Signed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Broadcasted =>
        Broadcasted(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Confirmed =>
        Confirmed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          payoutAddress,
          peerOpt
        )
      case DLCState.Claimed =>
        Claimed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          oracleSigs,
          oracleOutcome,
          myPayout = myPayoutOpt.get,
          counterPartyPayout = theirPayoutOpt.get,
          payoutAddress,
          peerOpt
        )
      case DLCState.RemoteClaimed =>
        require(
          oracleSigs.size == 1,
          "Remote claimed should only have one oracle sig"
        )
        RemoteClaimed(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          oracleSigs.head,
          oracleOutcome,
          myPayout = myPayoutOpt.get,
          counterPartyPayout = theirPayoutOpt.get,
          payoutAddress,
          peerOpt
        )
      case DLCState.Refunded =>
        Refunded(
          dlcId,
          isInitiator,
          lastUpdated,
          tempContractId,
          contractId,
          ContractInfo.fromTLV(contractInfoTLV),
          DLCTimeouts(contractMaturity, contractTimeout),
          feeRate,
          totalCollateral,
          localCollateral,
          fundingTxId,
          closingTxId,
          myPayout = myPayoutOpt.get,
          counterPartyPayout = theirPayoutOpt.get,
          payoutAddress,
          peerOpt
        )
    }
  }

  implicit val dlcWalletAccountingWriter: Writer[DLCWalletAccounting] = {
    writer[Obj].comap { (walletAccounting: DLCWalletAccounting) =>
      Obj(
        PicklerKeys.myCollateral -> Num(
          walletAccounting.myCollateral.satoshis.toLong.toDouble
        ),
        PicklerKeys.theirCollateral -> Num(
          walletAccounting.theirCollateral.satoshis.toLong.toDouble
        ),
        PicklerKeys.myPayout -> Num(
          walletAccounting.myPayout.satoshis.toLong.toDouble
        ),
        PicklerKeys.theirPayout -> Num(
          walletAccounting.theirPayout.satoshis.toLong.toDouble
        ),
        PicklerKeys.pnl -> Num(walletAccounting.pnl.satoshis.toLong.toDouble),
        PicklerKeys.rateOfReturn -> Num(walletAccounting.rateOfReturn.toDouble)
      )
    }
  }

  implicit val mnemonicCodePickler: ReadWriter[MnemonicCode] =
    readwriter[String].bimap(
      _.words.mkString(" "),
      str => MnemonicCode.fromWords(str.split(' ').toVector)
    )

  implicit val extPrivateKeyPickler: ReadWriter[ExtPrivateKey] =
    readwriter[String].bimap(ExtKey.toString, ExtPrivateKey.fromString)

  implicit val oracleAnnouncementTLV: ReadWriter[OracleAnnouncementV0TLV] =
    readwriter[String].bimap(_.hex, OracleAnnouncementV0TLV.fromHex)

  implicit val oracleAttestmentTLV: ReadWriter[OracleAttestmentTLV] =
    readwriter[String].bimap(_.hex, OracleAttestmentTLV.fromHex)

  implicit val oracleAttestmentV0TLV: ReadWriter[OracleAttestmentV0TLV] =
    readwriter[String].bimap(_.hex, OracleAttestmentV0TLV.fromHex)

  implicit val ecPublicKeyPickler: ReadWriter[ECPublicKey] =
    readwriter[String].bimap(_.hex, ECPublicKey.fromHex)

  implicit val addressTypePickler: ReadWriter[AddressType] =
    readwriter[String].bimap(_.toString, AddressType.fromString)

  def parseContractDescriptor(payoutsVal: Value): ContractDescriptorV0TLV = {
    val outcomes = payoutsVal(PicklerKeys.outcomesKey)
    val payouts: Vector[(String, Satoshis)] = outcomes.obj.toVector.map {
      case (outcome, payoutJs) =>
        val payout = jsToSatoshis(payoutJs.num)
        (outcome, payout)
    }
    ContractDescriptorV0TLV(outcomes = payouts)
  }

  private def readBlockHeaderResult(obj: Obj): GetBlockHeaderResult = {
    val hash = DoubleSha256DigestBE.fromHex(obj(PicklerKeys.hashKey).str)
    val confirmations = obj(PicklerKeys.confirmationsKey).num.toInt
    val height = obj(PicklerKeys.heightKey).num.toInt
    val version = obj(PicklerKeys.versionKey).num.toInt
    val versionHex = Int32.fromHex(obj(PicklerKeys.versionHexKey).str)
    val merkleroot =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.merklerootKey).str)
    val time = UInt32(obj(PicklerKeys.timeKey).num.toLong)
    val mediantime = UInt32(obj(PicklerKeys.mediantimeKey).num.toLong)
    val nonce = UInt32(obj(PicklerKeys.nonceKey).num.toLong)
    val bits = UInt32.fromHex(obj(PicklerKeys.bitsKey).str)
    val difficulty = obj(PicklerKeys.difficultyKey).num
    val chainWork = obj(PicklerKeys.chainworkKey).str
    val previousBlockHash = obj(PicklerKeys.previousblockhashKey).strOpt.map {
      str =>
        DoubleSha256DigestBE.fromHex(str)
    }

    val nextblockhash = obj(PicklerKeys.nextblockhashKey).strOpt.map { str =>
      DoubleSha256DigestBE.fromHex(str)
    }

    GetBlockHeaderResult(
      hash = hash,
      confirmations = confirmations,
      height = height,
      version = version,
      versionHex = versionHex,
      merkleroot = merkleroot,
      time = time,
      mediantime = mediantime,
      nonce = nonce,
      bits = bits,
      difficulty = difficulty,
      chainwork = chainWork,
      previousblockhash = previousBlockHash,
      nextblockhash = nextblockhash
    )
  }

  private def writeBlockHeaderResult(header: GetBlockHeaderResult): Obj = {
    val json = Obj(
      PicklerKeys.rawKey -> Str(header.blockHeader.hex),
      PicklerKeys.hashKey -> Str(header.hash.hex),
      PicklerKeys.confirmationsKey -> Num(header.confirmations),
      PicklerKeys.heightKey -> Num(header.height),
      PicklerKeys.versionKey -> Num(header.version.toLong.toDouble),
      PicklerKeys.versionHexKey -> Str(Int32(header.version).hex),
      PicklerKeys.merklerootKey -> Str(header.merkleroot.hex),
      PicklerKeys.timeKey -> Num(header.time.toBigInt.toDouble),
      PicklerKeys.mediantimeKey -> Num(header.mediantime.toLong.toDouble),
      PicklerKeys.nonceKey -> Num(header.nonce.toBigInt.toDouble),
      PicklerKeys.bitsKey -> Str(header.bits.hex),
      PicklerKeys.difficultyKey -> Num(header.difficulty.toDouble),
      PicklerKeys.chainworkKey -> Str(header.chainwork),
      PicklerKeys.previousblockhashKey -> {
        header.previousblockhash.map(_.hex) match {
          case Some(str) => Str(str)
          case None      => ujson.Null
        }
      },
      PicklerKeys.nextblockhashKey -> {
        header.nextblockhash.map(_.hex) match {
          case Some(str) => Str(str)
          case None      => ujson.Null
        }
      }
    )
    json
  }

  implicit val getBlockHeaderResultPickler: ReadWriter[GetBlockHeaderResult] = {
    readwriter[ujson.Obj]
      .bimap(writeBlockHeaderResult(_), readBlockHeaderResult(_))
  }

  implicit val compactFilterHeaderPickler: ReadWriter[CompactFilterHeaderDb] = {
    readwriter[ujson.Obj]
      .bimap(writeCompactFilterHeaderDb(_), readCompactFilterHeaderDb(_))
  }

  implicit val compactFilterDbPickler: ReadWriter[CompactFilterDb] = {
    readwriter[ujson.Obj]
      .bimap(writeCompactFilterDb(_), readCompactFilterDb(_))
  }

  private def writeCompactFilterDb(
      compactFilterDb: CompactFilterDb
  ): ujson.Obj = {
    ujson.Obj(
      PicklerKeys.hashKey -> ujson.Str(compactFilterDb.hashBE.hex),
      PicklerKeys.filterTypeKey -> ujson.Str(
        compactFilterDb.filterType.toString
      ),
      PicklerKeys.compactFilterBytesKey -> ujson.Str(
        compactFilterDb.bytes.toHex
      ),
      PicklerKeys.heightKey -> ujson.Num(compactFilterDb.height),
      PicklerKeys.blockHashKey -> ujson.Str(compactFilterDb.blockHashBE.hex)
    )
  }

  private def readCompactFilterDb(obj: ujson.Obj): CompactFilterDb = {
    val hash = DoubleSha256DigestBE.fromHex(obj(PicklerKeys.hashKey).str)
    val filterType = FilterType.fromString(obj(PicklerKeys.filterTypeKey).str)
    val bytes =
      ByteVector.fromValidHex(obj(PicklerKeys.compactFilterBytesKey).str)
    val height = obj(PicklerKeys.heightKey).num.toInt
    val blockHash =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.blockHashKey).str)

    CompactFilterDb(
      hashBE = hash,
      filterType = filterType,
      bytes = bytes,
      height = height,
      blockHashBE = blockHash
    )
  }

  private def writeCompactFilterHeaderDb(
      filterHeaderDb: CompactFilterHeaderDb
  ): ujson.Obj = {
    ujson.Obj(
      PicklerKeys.hashKey -> ujson.Str(filterHeaderDb.hashBE.hex),
      PicklerKeys.filterHashKey -> ujson.Str(filterHeaderDb.filterHashBE.hex),
      PicklerKeys.previousFilterHeaderKey ->
        ujson.Str(filterHeaderDb.previousFilterHeaderBE.hex),
      PicklerKeys.blockHashKey -> ujson.Str(filterHeaderDb.blockHashBE.hex),
      PicklerKeys.heightKey -> ujson.Num(filterHeaderDb.height)
    )
  }

  private def readCompactFilterHeaderDb(
      obj: ujson.Obj
  ): CompactFilterHeaderDb = {
    val hash = DoubleSha256DigestBE.fromHex(obj(PicklerKeys.hashKey).str)
    val filterHash =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.filterHashKey).str)
    val previousFilterHeader =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.previousFilterHeaderKey).str)
    val blockHash =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.blockHashKey).str)
    val height = obj(PicklerKeys.heightKey).num
    CompactFilterHeaderDb(
      hashBE = hash,
      filterHashBE = filterHash,
      previousFilterHeaderBE = previousFilterHeader,
      blockHashBE = blockHash,
      height = height.toInt
    )
  }

  private def writeContactDb(contact: DLCContactDb): ujson.Obj = {
    Obj(
      PicklerKeys.aliasKey -> contact.alias,
      PicklerKeys.addressKey -> s"${contact.address.getHostName}:${contact.address.getPort}",
      PicklerKeys.memoKey -> contact.memo
    )
  }

  private def readContactDb(obj: ujson.Obj): DLCContactDb = {
    val addressStr = obj(PicklerKeys.addressKey).str
    val address: InetSocketAddress =
      NetworkUtil.parseInetSocketAddress(addressStr, DLC.DefaultPort)
    DLCContactDb(
      alias = obj(PicklerKeys.aliasKey).str,
      address = address,
      memo = obj(PicklerKeys.memoKey).str
    )
  }

  implicit val contactDbPickler: ReadWriter[DLCContactDb] = {
    readwriter[ujson.Obj]
      .bimap(writeContactDb(_), readContactDb(_))
  }

  implicit val rescanComplete: ReadWriter[RescanComplete] = {
    readwriter[ujson.Value].bimap(writeRescanComplete(_), readRescanComplete(_))
  }

  private def writeRescanComplete(rescanComplete: RescanComplete): ujson.Str = {
    ujson.Str(rescanComplete.payload)
  }

  private def readRescanComplete(value: ujson.Value): RescanComplete = {
    RescanComplete(value.str)
  }

  implicit val feeUnit: ReadWriter[FeeUnit] = {
    readwriter[ujson.Value].bimap(writeFeeUnit(_), readFeeUnit(_))
  }

  private def writeFeeUnit(unit: FeeUnit): Value = unit match {
    case SatoshisPerVirtualByte(currencyUnit) =>
      ujson.Num(currencyUnit.satoshis.toDouble)
    case err: FeeUnit =>
      throw new RuntimeException(s"Unsupported fee unit type: `$err`")
  }

  private def readFeeUnit(value: Value): FeeUnit = {
    SatoshisPerVirtualByte.fromLong(value.num.toLong)
  }

  implicit val hdPurpose: ReadWriter[HDPurpose] = {
    readwriter[ujson.Str]
      .bimap(_.toString, str => HDPurpose.fromString(str.str))
  }

  implicit val walletInfo: ReadWriter[WalletInfo] = {
    readwriter[ujson.Obj].bimap(writeWalletInfo, readWalletInfo)
  }

  private def writeWalletInfo(info: WalletInfo): Obj = {
    Obj(PicklerKeys.walletKey -> {
      Obj(
        PicklerKeys.keyManagerKey -> Obj(
          PicklerKeys.rootXpubKey -> Str(info.rootXpub.toString)
        ),
        PicklerKeys.walletNameKey -> Str(info.walletName),
        PicklerKeys.xpubKey -> Str(info.xpub.toString),
        PicklerKeys.hdPathKey -> Str(info.hdAccount.toString),
        PicklerKeys.heightKey -> Num(info.height),
        PicklerKeys.blockHashKey -> Str(info.blockHash.hex),
        PicklerKeys.rescanKey -> info.rescan,
        PicklerKeys.importKey -> info.imported
      )
    })

  }

  private def readWalletInfo(walletObj: Obj): WalletInfo = {
    val obj = walletObj(PicklerKeys.walletKey).obj
    val walletName = obj(PicklerKeys.walletNameKey).str

    val rootXpubObj =
      obj(PicklerKeys.keyManagerKey).obj(PicklerKeys.rootXpubKey).str
    val rootXpub = ExtPublicKey.fromString(rootXpubObj)
    val xpubKey = ExtPublicKey.fromString(obj(PicklerKeys.xpubKey).str)
    val hdAccount = HDAccount.fromString(obj(PicklerKeys.hdPathKey).str)
    val height = obj(PicklerKeys.heightKey).num.toInt
    val blockHash =
      DoubleSha256DigestBE.fromHex(obj(PicklerKeys.blockHashKey).str)
    val rescan = obj(PicklerKeys.rescanKey).bool
    val imported = obj(PicklerKeys.importKey).bool
    WalletInfo(
      walletName = walletName,
      rootXpub = rootXpub,
      xpub = xpubKey,
      hdAccount = hdAccount,
      height = height,
      blockHash = blockHash,
      rescan = rescan,
      imported = imported
    )
  }

}
