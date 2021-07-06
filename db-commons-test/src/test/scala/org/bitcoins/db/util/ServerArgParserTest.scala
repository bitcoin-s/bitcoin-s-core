package org.bitcoins.db.util

import com.typesafe.config.ConfigFactory
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class ServerArgParserTest extends BitcoinSUnitTest {

  behavior of "ServerArgParser"

  it must "handle no command line flags" in {
    val parser = ServerArgParser(Vector.empty)

    //config must be empty
    assert(parser.toConfig == ConfigFactory.empty())
  }

  it must "handle having all command line args we support" in {
    val datadir = BitcoinSTestAppConfig.tmpDir().toAbsolutePath.toString
    val args = Vector("--rpcport",
                      "1234",
                      "--rpcbind",
                      "my.cool.site.com",
                      "--datadir",
                      s"${datadir}",
                      "--force-recalc-chainwork")
    println(s"datadir=$datadir")
    val parser = ServerArgParser(args)

    val config = parser.toConfig

    assert(config.hasPath(s"bitcoin-s.datadir"))
    assert(config.hasPath(s"bitcoin-s.server.rpcbind"))
    assert(config.hasPath(s"bitcoin-s.server.rpcport"))
    assert(config.hasPath(s"bitcoin-s.chain.force-recalc-chainwork"))
  }
}
