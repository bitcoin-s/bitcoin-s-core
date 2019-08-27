package org.bitcoins.rpc
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Path}

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

import scala.io.Source
import akka.stream.StreamTcpException
import java.nio.file.Paths
import scala.util.Properties
import org.bitcoins.rpc.config.BitcoindConfig
import org.bitcoins.rpc.config.BitcoindAuthCredentials
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.core.config.RegTest
import java.net.URI
import scala.concurrent.Future
import org.scalatest.compatible.Assertion

class BitcoindInstanceTest extends BitcoindRpcTest {

  private val sampleConf: Seq[String] = {
    val source = Source.fromURL(getClass.getResource("/sample-bitcoin.conf"))
    source.getLines.toSeq
  }

  private val datadir: Path = Files.createTempDirectory(null)

  override protected def beforeAll(): Unit = {
    val confFile = new File(datadir.toString + "/bitcoin.conf")
    val pw = new PrintWriter(confFile)
    sampleConf.foreach(line => pw.write(line + "\n"))
    pw.close()
  }

  /**
    * Tests that the client can call the isStartedF method
    * without throwing and then start
    */
  private def testClientStart(client: BitcoindRpcClient): Future[Assertion] = {
    clientAccum += client
    for {
      firstStarted <- client.isStartedF
      _ <- client.start()
      secondStarted <- client.isStartedF

      _ <- client.getBalance
    } yield {
      assert(!firstStarted)
      assert(secondStarted)
    }
  }

  behavior of "BitcoindInstance"

  it should "start a bitcoind with cookie based authentication" in {
    val confStr = s"""
                     |regtest=1
                     |daemon=1
                     |port=${RpcUtil.randomPort}
                     |rpcport=${RpcUtil.randomPort}
    """.stripMargin

    val conf = BitcoindConfig(confStr, BitcoindRpcTestUtil.tmpDir())
    val instance = BitcoindInstance.fromConfig(conf)
    assert(
      instance.authCredentials
        .isInstanceOf[BitcoindAuthCredentials.CookieBased])

    val cli = new BitcoindRpcClient(instance)
    testClientStart(cli)
  }

  it should "start a bitcoind with user and password based authentication" in {
    val confStr = s"""
                     |daemon=1
                     |regtest=1
                     |rpcuser=foobar
                     |rpcpassword=barfoo
                     |port=${RpcUtil.randomPort}
                     |rpcport=${RpcUtil.randomPort}
      """.stripMargin

    val conf = BitcoindConfig(confStr, BitcoindRpcTestUtil.tmpDir())
    val instance = BitcoindInstance.fromConfig(conf)
    assert(
      instance.authCredentials
        .isInstanceOf[BitcoindAuthCredentials.PasswordBased])
    testClientStart(new BitcoindRpcClient(instance))
  }

  // the values in this conf was generated by executing
  // rpcauth.py from Bicoin Core like this:
  //
  // ❯ ./rpcauth.py bitcoin-s strong_password
  // String to be appended to bitcoin.conf:
  // rpcauth=bitcoin-s:6d7580be1deb4ae52bc4249871845b09$82b282e7c6493f6982a5a7af9fbb1b671bab702e2f31bbb1c016bb0ea1cc27ca
  // Your password:
  // strong_password
  it should "start a bitcoind with auth based authentication" in {
    val port = RpcUtil.randomPort
    val rpcPort = RpcUtil.randomPort
    val confStr = s"""
                     |daemon=1
                     |rpcauth=bitcoin-s:6d7580be1deb4ae52bc4249871845b09$$82b282e7c6493f6982a5a7af9fbb1b671bab702e2f31bbb1c016bb0ea1cc27ca
                     |regtest=1
                     |port=${RpcUtil.randomPort}
                     |rpcport=${RpcUtil.randomPort}
       """.stripMargin

    val conf = BitcoindConfig(confStr, BitcoindRpcTestUtil.tmpDir())
    val authCredentials =
      BitcoindAuthCredentials.PasswordBased(username = "bitcoin-s",
                                            password = "strong_password")
    val instance =
      BitcoindInstance(
        network = RegTest,
        uri = new URI(s"http://localhost:$port"),
        rpcUri = new URI(s"http://localhost:$rpcPort"),
        authCredentials = authCredentials,
        datadir = conf.datadir
      )

    testClientStart(new BitcoindRpcClient(instance))
  }

  it should "parse a bitcoin.conf file, start bitcoind, mine some blocks and quit" in {
    val instance = BitcoindInstance.fromDatadir(datadir.toFile)
    val client = new BitcoindRpcClient(instance)

    for {
      _ <- client.start()
      _ <- client.generate(101)
      balance <- client.getBalance
      _ <- BitcoindRpcTestUtil.stopServers(Vector(client))
      _ <- client.getBalance
        .map { balance =>
          logger.error(s"Got unexpected balance: $balance")
          fail("Was able to connect to bitcoind after shutting down")
        }
        .recover {
          case _: StreamTcpException =>
            ()
        }
    } yield assert(balance > Bitcoins(0))

  }

}
