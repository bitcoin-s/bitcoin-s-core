---
title: Wallet Sync
id: version-1.9.10-wallet-sync
original_id: wallet-sync
---

## High level wallet state

Our wallet infrastructure has a specific table called `state_descriptors`.
This tracks chain state for our wallet.
Here is an example of the contents of this table

>sqlite> select * from state_descriptors;
SyncHeight|0000000000000000000134aa9e949ea1d053042b8dfa59bdc73b0322a88f009e 665741

If you look carefully in the second column, you will see a string encoding indicating
what the wallet state is. In this case, the last block hash seen by the wallet is

>0000000000000000000134aa9e949ea1d053042b8dfa59bdc73b0322a88f009e

and height

>665741

If you have access to a wallet, you can call

[`wallet.getSyncDescriptorOpt`](https://github.com/bitcoin-s/bitcoin-s/blob/36b5fc142715f8ab3ad053465d53dc29ab319790/wallet/src/main/scala/org/bitcoins/wallet/Wallet.scala#L160) to get access to this information

#### Wallet state from the cli

Alternatively, you can retrieve this information with `bitcoin-s-cli`

```
 ./bitcoin-s-cli walletinfo
{
  "wallet": {
    "keymanager": {
      "rootXpub": "..."
    },
    "xpub": "...",
    "hdPath": "...",
    "height": 1906239,
    "blockHash": "00000000dcf1066b8cd764a6104a9b5e95a55cd31adf9107974b2581ac90fdb9"
  }
}
```

## Syncing a wallet

Bitcoin-s provides a utility object called [`WalletSync`](https://github.com/bitcoin-s/bitcoin-s/blob/f3e81d027dfdda79e26642d5c29d381874ee72da/wallet/src/main/scala/org/bitcoins/wallet/sync/WalletSync.scala#L10)
that provides useful utilities for syncing a bitcoin-s wallet.

### Syncing wallet for with access to full blocks

Inside of `WalletSync` we have a method called [`WalletSync.syncFullBlocks`](https://github.com/bitcoin-s/bitcoin-s/blob/f3e81d027dfdda79e26642d5c29d381874ee72da/wallet/src/main/scala/org/bitcoins/wallet/sync/WalletSync.scala#L18)
This method takes 4 parameters

- a [Wallet](https://github.com/bitcoin-s/bitcoin-s/blob/36b5fc142715f8ab3ad053465d53dc29ab319790/wallet/src/main/scala/org/bitcoins/wallet/Wallet.scala#L46) to sync
- `getBlockHeaderFunc` is a function to retrieve a block header based on a blockHash
- `getBestBlockHashFunc` is a function to retrieve the best block hash for our blockchain
- `getBlockFunc` is a function to retrieve a full [`Block`](https://github.com/bitcoin-s/bitcoin-s/blob/8a148357d560a40bf21e7c0e3f4074cd276534fe/core/src/main/scala/org/bitcoins/core/protocol/blockchain/Block.scala#L18) that corresponds to a block hash

Given these for things, we can use [`WalletSync.syncFullBlocks`](https://github.com/bitcoin-s/bitcoin-s/blob/f3e81d027dfdda79e26642d5c29d381874ee72da/wallet/src/main/scala/org/bitcoins/wallet/sync/WalletSync.scala#L18) to sync our entire wallet.

Here is a code example


```scala
implicit val system: ActorSystem = ActorSystem(s"wallet-sync-example")
implicit val ec: ExecutionContext = system.dispatcher

// this reads authentication credentials and
// connection details from the default data
// directory on your platform
val client = BitcoindRpcClient.fromDatadir(binary=new File("/path/to/bitcoind"), datadir=new File("/path/to/bitcoind-datadir"))

//yay! Now we have a started bitcoind.
//We will use this as our datasource for syncing our wallet
val bitcoindRpcClientF: Future[BitcoindRpcClient] = client.start()


//wait for bitcoind to get started
val bitcoind = Await.result(bitcoindRpcClientF, 10.seconds)

val getBestBlockHashFunc = () => bitcoind.getBestBlockHash()

val getBlockHeaderFunc = { (hash: DoubleSha256DigestBE) => bitcoind.getBlockHeaderRaw(hash) }

val getBlockFunc = { (hash: DoubleSha256DigestBE) => bitcoind.getBlockRaw(hash) }

val genesisHashBEF = bitcoind.getBlockHash(0)

//yay! We are now all setup. Using our 3 functions above and a wallet, we can now sync
//a fresh wallet
implicit val walletAppConfig: WalletAppConfig = WalletAppConfig.fromDefaultDatadir()

val wallet = Wallet(bitcoind, bitcoind)

//yay! we have a synced wallet
val syncedWalletF = genesisHashBEF.flatMap { genesisHash => 
  WalletSync.syncFullBlocks(wallet,
      getBlockHeaderFunc,
      getBestBlockHashFunc,
      getBlockFunc, 
      genesisHash)
}
```
