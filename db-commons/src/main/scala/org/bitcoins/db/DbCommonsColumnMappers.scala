package org.bitcoins.db

import org.bitcoins.core.api.node.{NodeStateDescriptor, NodeStateDescriptorType}
import org.bitcoins.core.api.wallet.{
  WalletStateDescriptor,
  WalletStateDescriptorType
}
import org.bitcoins.core.config.{BitcoinNetwork, BitcoinNetworks}
import org.bitcoins.core.crypto.{ExtKeyPubVersion, _}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.hd._
import org.bitcoins.core.number.{Int32, UInt32, UInt64, UInt8}
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.dlc.models.{
  ContractInfo,
  DLCState,
  SingleOracleInfo
}
import org.bitcoins.core.protocol.ln.channel.ShortChannelId
import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.core.protocol.ln._
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{
  Bech32Address,
  BitcoinAddress,
  BlockTimeStamp
}
import org.bitcoins.core.psbt.{InputPSBTMap, PSBT}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import slick.jdbc.{GetResult, JdbcProfile}

import java.net.{InetSocketAddress, URI}
import scala.util.Try

class DbCommonsColumnMappers(val profile: JdbcProfile) {

  import profile.api._

  /** If executing something like this:
    *
    * {{{
    * sql"SELECT * FROM sqlite_master where type='table'"
    * }}}
    *
    * you end up with something like this:
    * {{{
    * /-------+---------------+---------------+----------+----------------------\
    * | 1     | 2             | 3             | 4        | 5                    |
    * | type  | name          | tbl_name      | rootpage | sql                  |
    * |-------+---------------+---------------+----------+----------------------|
    * | table | block_headers | block_headers | 2        | CREATE TABLE "blo... |
    * \-------+---------------+---------------+----------+----------------------/
    * }}}
    *
    * This is most likely an implementation that will break of you try and cast
    * the result of a different raw SQL query into a
    * [[org.bitcoins.db.SQLiteTableInfo SQLiteTableInfo]].
    */
  implicit val sqliteTableInfoReader: GetResult[SQLiteTableInfo] =
    GetResult[SQLiteTableInfo] { row =>
      row.nextString() // type
      row.nextString() // name
      val tableName = row.nextString()
      row.nextString() // rootpage
      val sql = row.nextString()
      SQLiteTableInfo(tableName, sql)

    }

  implicit val doubleSha256DigestBEMapper: BaseColumnType[
    DoubleSha256DigestBE] =
    MappedColumnType.base[DoubleSha256DigestBE, String](
      _.hex,
      DoubleSha256DigestBE.fromHex
    )

  implicit val doubleSha256DigestMapper: BaseColumnType[DoubleSha256Digest] =
    MappedColumnType.base[DoubleSha256Digest, String](
      _.hex,
      DoubleSha256Digest.fromHex)

  implicit val bigIntMapper: BaseColumnType[BigInt] =
    MappedColumnType
      .base[BigInt, String](
        { bigInt =>
          val bytes = ByteVector(bigInt.toByteArray)
          val padded = if (bytes.length <= 33) {
            bytes.padLeft(33)
          } else bytes

          padded.toHex
        },
        { hex =>
          val bytes = ByteVector.fromValidHex(hex).dropWhile(_ == 0x00).toArray
          BigInt(1, bytes)
        }
      )

  implicit val bigIntPostgresMapper: BaseColumnType[BigInt] =
    MappedColumnType
      .base[BigInt, BigDecimal](BigDecimal(_), _.toBigInt)

  implicit val sha256DigestBEMapper: BaseColumnType[Sha256DigestBE] =
    MappedColumnType.base[Sha256DigestBE, String](_.hex, Sha256DigestBE.fromHex)

  implicit val sha256DigestMapper: BaseColumnType[Sha256Digest] =
    MappedColumnType.base[Sha256Digest, String](_.hex, Sha256Digest.fromHex)

  implicit val ecPublicKeyMapper: BaseColumnType[ECPublicKey] =
    MappedColumnType.base[ECPublicKey, String](_.hex, ECPublicKey.fromHex)

  implicit val fieldElementMapper: BaseColumnType[FieldElement] =
    MappedColumnType.base[FieldElement, String](_.hex, FieldElement.fromHex)

  implicit val ecDigitalSignatureMapper: BaseColumnType[ECDigitalSignature] =
    MappedColumnType.base[ECDigitalSignature, String](
      _.hex,
      ECDigitalSignature.fromHex)

  implicit val signingVersionMapper: BaseColumnType[SigningVersion] =
    MappedColumnType.base[SigningVersion, String](_.toString,
                                                  SigningVersion.fromString)

  implicit val schnorrPublicKeyMapper: BaseColumnType[SchnorrPublicKey] =
    MappedColumnType
      .base[SchnorrPublicKey, String](_.hex, SchnorrPublicKey.fromHex)

  implicit val schnorrNonceMapper: BaseColumnType[SchnorrNonce] =
    MappedColumnType.base[SchnorrNonce, String](_.hex, SchnorrNonce.fromHex)

  implicit val oracleInfoTLVMapper: BaseColumnType[OracleInfoTLV] =
    MappedColumnType.base[OracleInfoTLV, String](_.hex, OracleInfoTLV.fromHex)

  implicit val sha256Hash160DigestMapper: BaseColumnType[Sha256Hash160Digest] =
    MappedColumnType
      .base[Sha256Hash160Digest, String](_.hex, Sha256Hash160Digest.fromHex)

  implicit val uInt8Mapper: BaseColumnType[UInt8] =
    MappedColumnType.base[UInt8, Int](
      tmap = _.toInt,
      tcomap = UInt8(_)
    )

  /** Responsible for mapping a [[org.bitcoins.core.number.UInt32 UInt32]] to a long in Slick, and vice versa */
  implicit val uInt32Mapper: BaseColumnType[UInt32] =
    MappedColumnType.base[UInt32, Long](
      tmap = _.toLong,
      tcomap = UInt32(_)
    )

  implicit val int32Mapper: BaseColumnType[Int32] = {
    MappedColumnType.base[Int32, Long](tmap = _.toLong, tcomap = Int32(_))
  }

  /** Responsible for mapping a [[org.bitcoins.core.protocol.transaction.TransactionOutput TransactionOutput]] to hex in Slick, and vice versa */
  implicit val transactionOutputMapper: BaseColumnType[TransactionOutput] = {
    MappedColumnType.base[TransactionOutput, String](
      _.hex,
      TransactionOutput(_)
    )
  }

  implicit val uint64Mapper: BaseColumnType[UInt64] = {
    MappedColumnType.base[UInt64, String](
      { case u64: UInt64 =>
        uInt64ToHex(u64)
      },
      UInt64.fromHex
    )
  }

  def uInt64ToHex(u64: UInt64): String = {
    val bytes = u64.bytes
    val padded = if (bytes.length <= 8) {
      bytes.padLeft(8)
    } else bytes

    padded.toHex
  }

  implicit val transactionOutPointMapper: BaseColumnType[
    TransactionOutPoint] = {
    MappedColumnType
      .base[TransactionOutPoint, String](_.hex, TransactionOutPoint(_))
  }

  implicit val scriptPubKeyMapper: BaseColumnType[ScriptPubKey] = {
    MappedColumnType.base[ScriptPubKey, String](_.hex, ScriptPubKey(_))
  }

  implicit val scriptWitnessMapper: BaseColumnType[ScriptWitness] = {
    MappedColumnType
      .base[ScriptWitness, String](
        _.hex,
        hex => RawScriptWitnessParser.read(ByteVector.fromValidHex(hex)))
  }

  implicit val byteVectorMapper: BaseColumnType[ByteVector] = {
    MappedColumnType
      .base[ByteVector, String](_.toHex, ByteVector.fromValidHex(_))
  }

  implicit val xpubMapper: BaseColumnType[ExtPublicKey] = {
    MappedColumnType
      .base[ExtPublicKey, String](_.toString, ExtPublicKey.fromString(_))
  }

  implicit val hdCoinTypeMapper: BaseColumnType[HDCoinType] = {
    MappedColumnType.base[HDCoinType, Int](_.toInt, HDCoinType.fromInt)
  }

  implicit val hdPathMappper: BaseColumnType[HDPath] =
    MappedColumnType
      .base[HDPath, String](_.toString, HDPath.fromString(_))

  implicit val segwitPathMappper: BaseColumnType[SegWitHDPath] =
    MappedColumnType
      .base[SegWitHDPath, String](_.toString, SegWitHDPath.fromString)

  implicit val hdChainTypeMapper: BaseColumnType[HDChainType] =
    MappedColumnType.base[HDChainType, Int](_.index, HDChainType.fromInt)

  implicit val hdPurposeMapper: BaseColumnType[HDPurpose] =
    MappedColumnType
      .base[HDPurpose, Int](
        _.constant,
        purpose =>
          HDPurposes.fromConstant(purpose).getOrElse(HDPurpose(purpose)))

  implicit val bitcoinAddressMapper: BaseColumnType[BitcoinAddress] =
    MappedColumnType
      .base[BitcoinAddress, String](_.value, BitcoinAddress.fromString)

  implicit val bech32AddressMapper: BaseColumnType[Bech32Address] =
    MappedColumnType
      .base[Bech32Address, String](_.value, Bech32Address.fromString)

  implicit val scriptTypeMapper: BaseColumnType[ScriptType] =
    MappedColumnType
      .base[ScriptType, String](_.toString, ScriptType.fromString)

  implicit val txMapper: BaseColumnType[Transaction] =
    MappedColumnType.base[Transaction, String](_.hex, Transaction.fromHex)

  implicit val PSBTMapper: BaseColumnType[PSBT] =
    MappedColumnType.base[PSBT, String](_.base64, PSBT.fromBase64)

  implicit val currencyUnitMapper: BaseColumnType[CurrencyUnit] =
    MappedColumnType
      .base[CurrencyUnit, Long](_.satoshis.toLong, l => Satoshis(l))

  implicit val satoshisMapper: BaseColumnType[Satoshis] =
    MappedColumnType
      .base[Satoshis, Long](_.toLong, l => Satoshis(l))

  implicit val filterTypeMapper: BaseColumnType[FilterType] =
    MappedColumnType
      .base[FilterType, Short](FilterType.getCode, FilterType.byCode)

  implicit val txoStateMapper: BaseColumnType[TxoState] = {
    MappedColumnType
      .base[TxoState, String](_.toString, TxoState.fromString(_))
  }

  implicit val satoshisPerByteMapper: BaseColumnType[SatoshisPerByte] = {
    MappedColumnType
      .base[SatoshisPerByte, Long](_.toLong, SatoshisPerByte.fromLong)
  }

  implicit val addressTagMapper: BaseColumnType[AddressTagName] = {
    MappedColumnType
      .base[AddressTagName, String](_.name, InternalAddressTagName.fromString)
  }

  implicit val addressTagTypeMapper: BaseColumnType[AddressTagType] = {
    MappedColumnType
      .base[AddressTagType, String](_.typeName,
                                    InternalAddressTagType.fromString)
  }

  implicit val hdAccountMapper: BaseColumnType[HDAccount] = {
    MappedColumnType.base[HDAccount, String](
      _.toString,
      str => HDAccount.fromPath(BIP32Path.fromString(str)).get)
  }

  implicit val contractInfoMapper: BaseColumnType[ContractInfo] = {
    MappedColumnType
      .base[ContractInfo, String](_.hex, ContractInfo.fromHex)
  }

  implicit val contractInfoTLVMapper: BaseColumnType[ContractInfoV0TLV] = {
    MappedColumnType
      .base[ContractInfoV0TLV, String](_.hex, ContractInfoV0TLV.fromHex)
  }

  implicit val contractDescriptorTLVMapper: BaseColumnType[
    ContractDescriptorTLV] = {
    MappedColumnType
      .base[ContractDescriptorTLV, String](_.hex, ContractDescriptorTLV.fromHex)
  }

  implicit val oracleParamsV0TLVMapper: BaseColumnType[OracleParamsV0TLV] = {
    MappedColumnType
      .base[OracleParamsV0TLV, String](_.hex, OracleParamsV0TLV.fromHex)
  }

  implicit val negotiationFieldsTLVMapper: BaseColumnType[
    NegotiationFieldsTLV] = {
    MappedColumnType
      .base[NegotiationFieldsTLV, String](_.hex, NegotiationFieldsTLV.fromHex)
  }

  implicit val dlcOutcomeTypeMapper: BaseColumnType[DLCOutcomeType] = {
    val enumStr = "Enum:"
    val unsignedNumStr = "Unsigned:"

    MappedColumnType.base[DLCOutcomeType, String](
      {
        case EnumOutcome(outcome) =>
          s"$enumStr$outcome"
        case UnsignedNumericOutcome(digits) =>
          s"$unsignedNumStr" + digits.mkString("|")
        case _: SignedNumericOutcome =>
          throw new RuntimeException("Unknown outcome type")
      },
      str => {
        if (str.startsWith(enumStr)) {
          EnumOutcome(str.drop(enumStr.length))
        } else if (str.startsWith(unsignedNumStr)) {
          val data = str.drop(unsignedNumStr.length)
          val strVec = data.split('|')
          val ints = strVec.map(_.toInt)

          UnsignedNumericOutcome(ints.toVector)
        } else
          throw new RuntimeException("Unknown outcome type")
      }
    )
  }

  implicit val dlcOutcomeTypeVecMapper: BaseColumnType[
    Vector[DLCOutcomeType]] = {
    val enumStr = "Enum:"
    val unsignedNumStr = "Unsigned:"

    MappedColumnType.base[Vector[DLCOutcomeType], String](
      vec =>
        {
          vec.map {
            case EnumOutcome(outcome) =>
              s"$enumStr$outcome"
            case UnsignedNumericOutcome(digits) =>
              s"$unsignedNumStr" + digits.mkString("|")
            case _: SignedNumericOutcome =>
              throw new RuntimeException("Unknown outcome type")
          }
        }.mkString,
      str => {
        if (str.startsWith(enumStr)) {
          val strs = str.split(s"$enumStr")
          strs.toVector.map(s => EnumOutcome(s))
        } else if (str.startsWith(unsignedNumStr)) {
          val strs = str.split(s"$unsignedNumStr")
          strs.toVector.flatMap { data =>
            if (data.isEmpty) {
              None
            } else {
              val strVec = data.split('|')
              val ints = strVec.flatMap(s => Try(s.toInt).toOption)
              Some(UnsignedNumericOutcome(ints.toVector))
            }
          }
        } else throw new RuntimeException("Unknown outcome type")
      }
    )
  }

  implicit val singleOracleInfoVecMapper: BaseColumnType[
    Vector[SingleOracleInfo]] =
    MappedColumnType.base[Vector[SingleOracleInfo], String](
      _.map(_.announcement.hex).mkString("|"),
      str => {
        val strs = str.split('|').toVector
        strs.map { str =>
          val announcementTLV = OracleAnnouncementTLV(str)
          SingleOracleInfo(announcementTLV)
        }
      }
    )

  implicit val blockStampWithFutureMapper: BaseColumnType[BlockTimeStamp] = {
    MappedColumnType.base[BlockTimeStamp, Long](
      _.toUInt32.toLong,
      long => BlockTimeStamp(UInt32(long)))
  }

  implicit val partialSigMapper: BaseColumnType[PartialSignature] = {
    MappedColumnType
      .base[PartialSignature, String](_.hex, PartialSignature.fromHex)
  }

  implicit val partialSigsMapper: BaseColumnType[Vector[PartialSignature]] = {
    MappedColumnType
      .base[Vector[PartialSignature], String](
        _.foldLeft("")(_ ++ _.hex),
        hex =>
          if (hex.isEmpty) Vector.empty
          else InputPSBTMap(hex ++ "00").partialSignatures)
  }

  implicit val satoshisPerVirtualByteMapper: BaseColumnType[
    SatoshisPerVirtualByte] = {
    MappedColumnType
      .base[SatoshisPerVirtualByte, String](
        _.currencyUnit.hex,
        hex => SatoshisPerVirtualByte(Satoshis.fromHex(hex)))
  }

  implicit val networkMapper: BaseColumnType[BitcoinNetwork] = {
    MappedColumnType
      .base[BitcoinNetwork, String](_.name, BitcoinNetworks.fromString)
  }

  implicit val schnorrDigitalSignatureMapper: BaseColumnType[
    SchnorrDigitalSignature] = {
    MappedColumnType.base[SchnorrDigitalSignature, String](
      _.hex,
      SchnorrDigitalSignature.fromHex)
  }

  implicit val schnorrDigitalSignatureVecMapper: BaseColumnType[
    Vector[SchnorrDigitalSignature]] = {
    MappedColumnType.base[Vector[SchnorrDigitalSignature], String](
      _.foldLeft("")(_ ++ _.hex),
      _.toArray
        .grouped(64 * 2)
        .map(new String(_))
        .map(SchnorrDigitalSignature.fromHex)
        .toVector
    )
  }

  implicit val walletStateDescriptorTypeMapper: BaseColumnType[
    WalletStateDescriptorType] =
    MappedColumnType.base[WalletStateDescriptorType, String](
      _.toString,
      WalletStateDescriptorType.fromString)

  implicit val walletStateDescriptorMapper: BaseColumnType[
    WalletStateDescriptor] =
    MappedColumnType.base[WalletStateDescriptor, String](
      _.toString,
      WalletStateDescriptor.fromString)

  implicit val ecAdaptorSignatureMapper: BaseColumnType[ECAdaptorSignature] = {
    MappedColumnType.base[ECAdaptorSignature, String](
      _.hex,
      ECAdaptorSignature.fromHex)
  }

  implicit val dlcStateMapper: BaseColumnType[DLCState] = {
    MappedColumnType
      .base[DLCState, String](_.toString, DLCState.fromString)
  }

  implicit val eventDescriptorTLVMapper: BaseColumnType[EventDescriptorTLV] = {
    MappedColumnType.base[EventDescriptorTLV, String](
      _.hex,
      EventDescriptorTLV.fromHex)
  }

  implicit val oracleAnnouncementTLV: BaseColumnType[OracleAnnouncementTLV] = {
    MappedColumnType.base[OracleAnnouncementTLV, String](
      _.hex,
      OracleAnnouncementTLV.fromHex)
  }

  implicit val lnInvoiceMapper: BaseColumnType[LnInvoice] = {
    MappedColumnType.base[LnInvoice, String](_.toString, LnInvoice.fromString)
  }

  implicit val nodeIdMapper: BaseColumnType[NodeId] = {
    MappedColumnType.base[NodeId, String](_.hex, NodeId.fromHex)
  }

  implicit val chainCodeMapper: BaseColumnType[ChainCode] = {
    MappedColumnType.base[ChainCode, String](_.hex, ChainCode.fromHex(_))
  }

  implicit val extKeyPubVersion: BaseColumnType[ExtKeyPubVersion] = {
    MappedColumnType.base[ExtKeyPubVersion, String](_.hex,
                                                    ExtKeyPubVersion.fromHex)
  }

  implicit val dlcSerializationVersion: BaseColumnType[
    DLCSerializationVersion] = {
    MappedColumnType.base[DLCSerializationVersion, String](
      _.toString,
      DLCSerializationVersion.fromString)
  }

  implicit val dlcOfferTLVMapper: BaseColumnType[DLCOfferTLV] = {
    MappedColumnType.base[DLCOfferTLV, String](_.hex, DLCOfferTLV.fromHex)
  }

  implicit val inetSocketAddressMapper: BaseColumnType[InetSocketAddress] = {
    def toString(address: InetSocketAddress): String =
      s"${address.getHostName}:${address.getPort}"

    def fromString(str: String): InetSocketAddress = {
      val uri = new URI(s"tcp://$str")
      InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
    }

    MappedColumnType.base[InetSocketAddress, String](toString, fromString)
  }

  implicit val PaymentSecretMapper: BaseColumnType[PaymentSecret] =
    MappedColumnType.base[PaymentSecret, String](_.hex, PaymentSecret.fromHex)

  implicit val ShortChannelIdMapper: BaseColumnType[ShortChannelId] =
    MappedColumnType.base[ShortChannelId, Long](_.u64.toLong,
                                                l => ShortChannelId(UInt64(l)))

  implicit val MilliSatoshisMapper: BaseColumnType[MilliSatoshis] =
    MappedColumnType.base[MilliSatoshis, Long](_.toLong, MilliSatoshis.apply(_))

  implicit val nodeStateDescriptorTypeMapper: BaseColumnType[
    NodeStateDescriptorType] =
    MappedColumnType.base[NodeStateDescriptorType, String](
      _.toString,
      NodeStateDescriptorType.fromString)

  implicit val nodeStateDescriptorMapper: BaseColumnType[NodeStateDescriptor] =
    MappedColumnType.base[NodeStateDescriptor, String](
      _.toString,
      NodeStateDescriptor.fromString)

}
