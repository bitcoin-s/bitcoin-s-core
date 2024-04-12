package org.bitcoins.core.config

import org.bitcoins.core.protocol.blockchain._
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256DigestBE, StringFactory}
import scodec.bits.ByteVector

sealed abstract class NetworkParameters {

  /** The parameters of the blockchain we are connecting to */
  def chainParams: ChainParams

  def p2pkhNetworkByte: ByteVector =
    chainParams.base58Prefix(Base58Type.PubKeyAddress)

  def p2shNetworkByte: ByteVector =
    chainParams.base58Prefix(Base58Type.ScriptAddress)
  def privateKey: ByteVector = chainParams.base58Prefix(Base58Type.SecretKey)

  /** @see [[https://github.com/bitcoin/bitcoin/blob/84d0fdce11709c8e26b9c450d47727ab36641437/src/chainparams.cpp Bitcoin Core]]
    *     `chainparams.cpp nDefaultPort`
    */
  def port: Int

  /** @see [[https://github.com/bitcoin/bitcoin/blob/bccb4d29a8080bf1ecda1fc235415a11d903a680/src/chainparamsbase.cpp Bitcoin Core]]
    *     `chainparamsbase.cpp`
    */
  def rpcPort: Int

  // $COVERAGE-OFF$
  def name: String = chainParams.networkId
  // $COVERAGE-ON$

  /** The seeds used to bootstrap the network */
  def dnsSeeds: Seq[String]

  /** The message start string is designed to be unlikely to occur in normal data.
    * The characters are rarely used upper ASCII, not valid as UTF-8, and produce
    * a large 32-bit integer with any alignment.
    * https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L108
    */
  def magicBytes: ByteVector

  def isSameNetworkBytes(other: NetworkParameters): Boolean = {
    p2pkhNetworkByte == other.p2pkhNetworkByte &&
    p2shNetworkByte == other.p2shNetworkByte
  }

}

sealed abstract class BitcoinNetwork extends NetworkParameters {

  override def chainParams: BitcoinChainParams
}

// $COVERAGE-OFF$
sealed abstract class MainNet extends BitcoinNetwork {
  override def chainParams: MainNetChainParams.type = MainNetChainParams

  /** @inheritdoc
    */
  override def port = 8333

  /** @inheritdoc
    */
  override def rpcPort = 8332

  /** @inheritdoc
    */
  override def dnsSeeds: Vector[String] = {
    Vector(
      //"seed.bitcoin.sipa.be", very slow, commenting out for now
      "dnsseed.bluematt.me",
      "dnsseed.bitcoin.dashjr.org",
      "seed.bitcoinstats.com",
      //"seed.btc.petertodd.net", very slow, commenting out for now
      "seed.bitcoin.jonasschnelli.ch",
      "seed.bitcoin.sprovoost.nl",
      "dnsseed.emzy.de",
      "seed.bitcoin.wiz.biz"
    )
  }

  /** @inheritdoc
    */
  override def magicBytes: ByteVector = ByteVector(0xf9, 0xbe, 0xb4, 0xd9)

}

case object MainNet extends MainNet

sealed abstract class TestNet3 extends BitcoinNetwork {
  override def chainParams: TestNetChainParams.type = TestNetChainParams

  /** @inheritdoc
    */
  override def port = 18333

  /** @inheritdoc
    */
  override def rpcPort = 18332

  /** @inheritdoc
    */
  override def dnsSeeds: Seq[String] =
    Seq("testnet-seed.bitcoin.jonasschnelli.ch",
        "seed.tbtc.petertodd.org",
        "seed.testnet.bitcoin.sprovoost.nl",
        "testnet-seed.bluematt.me")
  /*
   * @inheritdoc
   */
  override def magicBytes: ByteVector = ByteVector(0x0b, 0x11, 0x09, 0x07)

}

case object TestNet3 extends TestNet3

sealed abstract class RegTest extends BitcoinNetwork {
  override def chainParams: RegTestNetChainParams.type = RegTestNetChainParams

  /** @inheritdoc
    */
  override def port = 18444

  /** @inheritdoc
    */
  override def rpcPort = 18443

  /** There's no DNS seeds on regtest
    */
  override def dnsSeeds: Nil.type = Nil

  /** @inheritdoc
    */
  override def magicBytes: ByteVector = ByteVector(0xfa, 0xbf, 0xb5, 0xda)
}

case object RegTest extends RegTest

sealed abstract class SigNet extends BitcoinNetwork {
  override def chainParams: SigNetChainParams = SigNetChainParams()

  /** @inheritdoc
    */
  override def port = 38333

  /** @inheritdoc
    */
  override def rpcPort = 38332

  /** @inheritdoc
    */
  override def dnsSeeds: Seq[String] =
    Seq("178.128.221.177",
        "2a01:7c8:d005:390::5",
        "ntv3mtqw5wt63red.onion:38333")

  /** @inheritdoc
    */
  override def magicBytes: ByteVector = {
    CryptoUtil
      .doubleSHA256(chainParams.signetChallenge.bytes)
      .bytes
      .take(4)
  }

}

case object SigNet extends SigNet
// $COVERAGE-ON$

object Networks extends StringFactory[NetworkParameters] {
  val knownNetworks: Seq[NetworkParameters] = BitcoinNetworks.knownNetworks
  val secretKeyBytes: Seq[ByteVector] = BitcoinNetworks.secretKeyBytes
  val p2pkhNetworkBytes: Seq[ByteVector] = BitcoinNetworks.p2pkhNetworkBytes
  val p2shNetworkBytes: Seq[ByteVector] = BitcoinNetworks.p2shNetworkBytes

  override def fromString(string: String): NetworkParameters =
    BitcoinNetworks.fromString(string)

  def magicToNetwork: Map[ByteVector, NetworkParameters] =
    BitcoinNetworks.magicToNetwork

  def bytesToNetwork: Map[ByteVector, NetworkParameters] =
    BitcoinNetworks.bytesToNetwork

  def fromChainHash(chainHash: DoubleSha256DigestBE): NetworkParameters = {
    knownNetworks
      .find(_.chainParams.genesisBlock.blockHeader.hashBE == chainHash)
      .getOrElse(throw new IllegalArgumentException(
        s"$chainHash is not a recognized Chain Hash"))
  }
}

object BitcoinNetworks extends StringFactory[BitcoinNetwork] {

  val knownNetworks: Seq[BitcoinNetwork] =
    Seq(MainNet, TestNet3, RegTest, SigNet)
  val secretKeyBytes: Seq[ByteVector] = knownNetworks.map(_.privateKey)
  val p2pkhNetworkBytes: Seq[ByteVector] = knownNetworks.map(_.p2pkhNetworkByte)
  val p2shNetworkBytes: Seq[ByteVector] = knownNetworks.map(_.p2shNetworkByte)

  /** Uses the notation used in `bitcoin.conf` */
  override def fromString(string: String): BitcoinNetwork =
    string.toLowerCase match {
      case "mainnet"  => MainNet
      case "main"     => MainNet
      case "testnet3" => TestNet3
      case "testnet"  => TestNet3
      case "test"     => TestNet3
      case "regtest"  => RegTest
      case "signet"   => SigNet
      case "sig"      => SigNet
      case _: String =>
        throw new IllegalArgumentException(s"Invalid network $string")
    }

  /** Map of magic network bytes to the corresponding network */
  lazy val magicToNetwork: Map[ByteVector, NetworkParameters] =
    Map(
      MainNet.magicBytes -> MainNet,
      TestNet3.magicBytes -> TestNet3,
      RegTest.magicBytes -> RegTest,
      SigNet.magicBytes -> SigNet
    )

  lazy val bytesToNetwork: Map[ByteVector, NetworkParameters] =
    Map(
      MainNet.p2shNetworkByte -> MainNet,
      MainNet.p2pkhNetworkByte -> MainNet,
      MainNet.privateKey -> MainNet,
      TestNet3.p2pkhNetworkByte -> TestNet3,
      TestNet3.p2shNetworkByte -> TestNet3,
      TestNet3.privateKey -> TestNet3

      // Omitting regtest and signet as they have the same network bytes as testnet3
    )
}
