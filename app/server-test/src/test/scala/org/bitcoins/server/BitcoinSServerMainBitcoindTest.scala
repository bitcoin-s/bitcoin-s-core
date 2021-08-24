package org.bitcoins.server

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.testkit.fixtures.BitcoinSAppConfigBitcoinFixtureNotStarted

/** Test starting bitcoin-s with bitcoind as the backend for app */
class BitcoinSServerMainBitcoindTest
    extends BitcoinSAppConfigBitcoinFixtureNotStarted {

  behavior of "BitcoinSServerMain"

  it must "start our app server with bitcoind as a backend" in {
    config: FixtureParam =>
      val server = new BitcoinSServerMain(ServerArgParser.empty)(system, config)

      val cliConfig = Config(rpcPortOpt = Some(config.rpcPort))

      for {
        _ <- server.start()
        // Await RPC server started
        _ <- BitcoinSServer.startedF

        info = ConsoleCli.exec(WalletInfo, cliConfig)
        balance = ConsoleCli.exec(GetBalance(true), cliConfig)
        addr = ConsoleCli.exec(GetNewAddress(None), cliConfig)
        blockHash = ConsoleCli.exec(GetBestBlockHash, cliConfig)
      } yield {
        assert(info.isSuccess)
        assert(balance.isSuccess)
        assert(balance.get == "0 sats")
        assert(addr.isSuccess)
        assert(blockHash.isSuccess)
      }
  }
}
