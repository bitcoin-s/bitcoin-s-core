---
id: version-1.9.10-rpc-bitcoind
title: bitcoind/Bitcoin Core
original_id: rpc-bitcoind
---

## Downloading bitcoind

The Bitcoin Core RPC client in Bitcoin-S currently supports the Bitcoin Core
- 25
- 26
- 27

version lines. It can be set up to work with both local and remote Bitcoin Core servers.

You can fetch them using bitcoin-s by running the following sbt command. If you already have bitcoind installed on your machine, you can skip this step.


```bash
sbt downloadBitcoind
```

The binaries will be stored in `~/.bitcoin-s/binaries/bitcoind/`


## Connecting to a local `bitcoind` instance

### Getting started quickly, with default options:

```scala
implicit val ec: ExecutionContext = ExecutionContext.global
implicit val system: ActorSystem = ActorSystem("System")
// this reads authentication credentials and
// connection details from the default data
// directory on your platform
val client = BitcoindRpcClient.fromDatadir(binary=new File("/path/to/bitcoind"), datadir=new File("/path/to/bitcoind-datadir"))

val balance: Future[Bitcoins] = for {
  _ <- client.start()
  balance <- client.getBalance
} yield balance
```

## Multi-wallet `bitcoind` instances

When using the `bitcoind` with multiple wallets you will need to specify the wallet's name.
To do so the wallet rpc functions have an optional `walletName` parameter.

```scala
implicit val ec: ExecutionContext = ExecutionContext.global
implicit val system: ActorSystem = ActorSystem("System")
val client = BitcoindRpcClient.fromDatadir(binary=new File("/path/to/bitcoind"), datadir=new File("/path/to/bitcoind-datadir"))

for {
  _ <- client.start()
  _ <- client.walletPassphrase("mypassword", 10000, "walletName")
  balance <- client.getBalance("walletName")
} yield balance
```

## Connecting to a remote `bitcoind`

First, we create a secure connection to our `bitcoind` instance by setting
up a SSH tunnel:

```bash
ssh -L 8332:localhost:8332 my-cool-user@my-cool-website.com
```

> Note: the port number '8332' is the default for mainnet. If you want to
> connect to a testnet `bitcoind`, the default port is '18332'

Now that we have a secure connection between our remote `bitcoind`, we're
ready to create the connection with our RPC client

```scala
implicit val ec: ExecutionContext = ExecutionContext.global
implicit val system: ActorSystem = ActorSystem("System")
val username = "FILL_ME_IN" //this username comes from 'rpcuser' in your bitcoin.conf file
val password = "FILL_ME_IN" //this password comes from your 'rpcpassword' in your bitcoin.conf file

val authCredentials = BitcoindAuthCredentials.PasswordBased(
  username = username,
  password = password
)

val bitcoindInstance = {
  BitcoindInstanceLocal(
    network = MainNet,
    uri = new URI(s"http://localhost:${MainNet.port}"),
    rpcUri = new URI(s"http://localhost:${MainNet.rpcPort}"),
    authCredentials = authCredentials
  )
}

val rpcCli = BitcoindRpcClient(bitcoindInstance)

rpcCli.getBalance.onComplete { case balance =>
  println(s"Wallet balance=${balance}")
}
```

## Error handling

All errors returned by Bitcoin Core are mapped to a corresponding
[`BitcoindException`](https://github.com/bitcoin-s/bitcoin-s/blob/master/bitcoind-rpc/src/main/scala/org/bitcoins/rpc/BitcoindException.scala).
These exceptions contain an error code and a message. `BitcoindException` is a sealed
trait, which means you can easily pattern match exhaustively. Of course, other errors
could also happen: network errors, stack overflows or out-of-memory errors. The provided
class is only intended to cover errors returned by Bitcoin Core. An example of how error
handling could look:

```scala
implicit val system: ActorSystem = ActorSystem()
implicit val ec: ExecutionContext = system.dispatcher

// let's assume you have an already running client,
// so there's no need to start this one
val cli = BitcoindRpcClient.fromDatadir(binary=new File("/path/to/bitcoind"), datadir=new File("/path/to/bitcoind-datadir"))

// let's also assume you have a bitcoin address
val address: BitcoinAddress = BitcoinAddress("bc1qm8kec4xvucdgtzppzvvr2n6wp4m4w0k8akhf98")

val txid: Future[DoubleSha256DigestBE] =
  cli.sendToAddress(address, 3.bitcoins).recoverWith {
    case BitcoindWalletException.UnlockNeeded(_) =>
      cli.walletPassphrase("my_passphrase", 60).flatMap { _ =>
        cli.sendToAddress(address, 3.bitcoins)
      }
  }
```
