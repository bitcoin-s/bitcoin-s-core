package org.bitcoins.rpc.config

import org.bitcoins.commons.util.BitcoinSLogger

import java.io.File
import java.nio.file.{Files, Paths}
import org.bitcoins.core.config._
import org.bitcoins.crypto.MaskedToString

/** This trait contains the information we need to authenticate
  * to a `bitcoind` node.
  */
sealed trait BitcoindAuthCredentials {
  def password: String

  def username: String
}

object BitcoindAuthCredentials extends BitcoinSLogger {
  import scala.jdk.CollectionConverters.ListHasAsScala

  /** Authenticate by providing a username and password.
    * If you are connecting to a local `bitcoind` you
    * should instead use cookie based authentication.
    * If you are connecting to a remote `bitcoind`, you
    * should use the Bitcoin Core-provided script
    * `rpcauth.py` to generate credentials. This will
    * give you a `rpcauth=...` string you can put in
    * your remote `bitcoind` configuration, as well as
    * a set of `rpcuser=...` and `rpcpassword=...` you
    * can put in your local `bitcoin.conf` configuration
    * file or provide directly to this class.
    *
    * @see [[https://github.com/bitcoin/bitcoin/tree/master/share/rpcauth rpcauth.py]],
    *      canonical Python script provided by Bitcoin Core to generate the
    *      auth credentials.
    */
  case class PasswordBased(
      username: String,
      password: String
  ) extends BitcoindAuthCredentials
      with MaskedToString {

    override def toStringSensitive: String = {
      s"PasswordBased($username,$password)"
    }
  }

  /** Authenticate by providing a cookie file
    * found in the `bitcoind` data directory.
    * This is the most secure as well as user
    * friendly way of authenticating, but it
    * is not always suitable for situtations
    * where the `bitcoind` instance is on a
    * remote server.
    */
  case class CookieBased(
      network: NetworkParameters,
      datadir: File = BitcoindConfig.DEFAULT_DATADIR)
      extends BitcoindAuthCredentials {

    private[bitcoins] lazy val cookiePath = {
      val middleSegment = network match {
        case MainNet  => ""
        case TestNet3 => "testnet3"
        case RegTest  => "regtest"
        case SigNet   => "signet"

      }
      Paths.get(datadir.toString, middleSegment, ".cookie")
    }

    /** The cookie is a string looking like
      * `__cookie__:AUTO_GENERATED_PASSWORD`
      */
    def cookie: String = {
      if (Files.exists(cookiePath)) {
        val cookieLines = Files.readAllLines(cookiePath).asScala
        cookieLines.head
      } else {
        throw new RuntimeException(s"Could not find $cookiePath!")
      }
    }

    def username: String = cookie.split(":").head
    def password: String = cookie.split(":").last

  }

  def fromConfig(config: BitcoindConfig): BitcoindAuthCredentials = {
    val datadir = config.datadir
    val username = config.username
    val password = config.password
    (username, password) match {
      case (Some(user), Some(pass)) =>
        PasswordBased(user, pass)
      case (_, _) =>
        CookieBased(config.network, datadir = datadir)
    }
  }
}
