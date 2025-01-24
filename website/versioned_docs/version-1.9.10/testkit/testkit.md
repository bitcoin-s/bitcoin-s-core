---
id: version-1.9.10-testkit
title: Testkit
original_id: testkit
---

## Philosophy of Testkit

The high level of of the bitcoin-s testkit is to mimic and provide functionality to test 3rd party applications.

There are other examples of these in the Scala ecosystem like the `akka-testkit` and `slick-testkit`.

We use this testkit to test bitcoin-s it self.


### Testkit for bitcoind

This gives the ability to create and destroy `bitcoind` on the underlying operating system to test against.

Our [BitcoindRpcClient](/api/org/bitcoins/rpc/client/common/BitcoindRpcClient) is tested with the functionality provided in the testkit.
A quick example of a useful utility method is [BitcoindRpcTestUtil.startedBitcoindRpcClient()](/api/org/bitcoins/testkit/rpc/BitcoindRpcTestUtil).
This spins up a bitcoind regtest instance on machine and generates 101 blocks on that node.

This gives you the ability to start spending money immediately with that bitcoind node.

```scala
implicit val system: ActorSystem = ActorSystem("bitcoind-testkit-example")
implicit val ec: ExecutionContext = system.dispatcher

//pick our bitcoind version we want to spin up
val bitcoindV = BitcoindVersion.newest

//create an instance
val instance = BitcoindRpcTestUtil.instance(versionOpt = Some(bitcoindV))

//now let's create an rpc client off of that instance
val bitcoindRpcClientF = BitcoindRpcTestUtil.startedBitcoindRpcClient(Some(instance), Vector.newBuilder)

//yay! it's started. Now you can run tests against this.
//let's just grab the block count for an example
val blockCountF = for {
  bitcoind <- bitcoindRpcClientF
  count <- bitcoind.getBlockCount()
} yield {
  //run a test against the block count
  assert(count > 0, s"Block count was not more than zero!")
}

//when you are done, don't forget to destroy it! Otherwise it will keep running on the underlying os
val stoppedF = for {
  rpc <- bitcoindRpcClientF
  _ <- blockCountF
  stopped <- BitcoindRpcTestUtil.stopServers(Vector(rpc))
} yield stopped
```

For more information on how the bitcoind rpc client works, see our [bitcoind rpc docs](../rpc/bitcoind.md)

#### Caching bitcoind in test cases

When doing integration tests with bitcoind, you likely do not want to spin up a
new bitcoind for _every_ test that is run.

Not to fear, when using `testkit` you can use our bitcoind fixtures for your unit tests!
These will only spin up on bitcoind per test suite, rather than one bitcoind per test.

We currently have two types of fixtures available to users of this dependency

1. [Connected pairs of bitcoind nodes](https://github.com/bitcoin-s/bitcoin-s/blob/eaac9c154c25f3bd76615ea2151092f06df6bdb4/testkit/src/main/scala/org/bitcoins/testkit/rpc/BitcoindFixtures.scala#L282)
2. [Bitcoind nodes with funded wallets](https://github.com/bitcoin-s/bitcoin-s/blob/eaac9c154c25f3bd76615ea2151092f06df6bdb4/testkit/src/main/scala/org/bitcoins/testkit/rpc/BitcoindFixtures.scala#L161)

If you mixin either of those traits for your test, you will now have access to the corresponding fixture.

You can find an examples of how to use these two test fixtures

1. [Example of using a connected pair of nodes in test suite](https://github.com/bitcoin-s/bitcoin-s/blob/32a6db930bdf849a94d92cd1de160b87845ab168/bitcoind-rpc-test/src/test/scala/org/bitcoins/rpc/common/WalletRpcTest.scala#L37)
2. [Example of using a bitcoind with funded wallet in test suite](https://github.com/bitcoin-s/bitcoin-s/blob/eaac9c154c25f3bd76615ea2151092f06df6bdb4/testkit/src/main/scala/org/bitcoins/testkit/rpc/BitcoindFixtures.scala#L161)


### Testkit for eclair

We have similar utility methods for eclair. Eclair's testkit requires a bitcoind running (which we can spin up thanks to our bitcoind testkit).

Here is an example of spinning up an eclair lightning node, that is connected to a bitcoind and testing your lightning application.

Make sure to run `sbt downloadBitcoind downloadEclair` before running this so you have access to the underlying eclair binares

```scala
//Steps:
//1. Open and confirm channel on the underlying blockchain (regtest)
//2. pay an invoice
//3. Await until the payment is processed
//4. assert the node has received the payment
//5. cleanup

implicit val system: ActorSystem = ActorSystem("eclair-testkit-example")
implicit val ec: ExecutionContext = system.dispatcher

//we need a bitcoind to connect eclair nodes to
lazy val bitcoindRpcClientF: Future[BitcoindRpcClient] = {
    for {
      cli <- EclairRpcTestUtil.startedBitcoindRpcClient()
      // make sure we have enough money to open channels
      address <- cli.getNewAddress
      _ <- cli.generateToAddress(200, address)
    } yield cli
}

//let's create two eclair nodes now
val clientF = for {
  bitcoind <- bitcoindRpcClientF
  e <- EclairRpcTestUtil.randomEclairClient(Some(bitcoind))
} yield e

val otherClientF = for {
  bitcoind <- bitcoindRpcClientF
  e <- EclairRpcTestUtil.randomEclairClient(Some(bitcoind))
} yield e

//great, setup done! Let's run the test
//to verify we can send a payment over the channel
for {
  client <- clientF
  otherClient <- otherClientF
  _ <- EclairRpcTestUtil.openAndConfirmChannel(clientF, otherClientF)
  invoice <- otherClient.createInvoice("abc", 50.msats)
  info <- otherClient.getInfo
  _ = assert(info.nodeId == invoice.nodeId)
  infos <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
  _ = assert(infos.isEmpty)
  paymentId <- client.payInvoice(invoice)
  _ <- EclairRpcTestUtil.awaitUntilPaymentSucceeded(client, paymentId)
  sentInfo <- client.getSentInfo(invoice.lnTags.paymentHash.hash)
} yield {
  assert(sentInfo.head.amount == 50.msats)
}

//don't forget to shutdown everything!
val stop1F = clientF.map(c => EclairRpcTestUtil.shutdown(c))

val stop2F = otherClientF.map(o => EclairRpcTestUtil.shutdown(o))

val stoppedBitcoindF = for {
  bitcoind <- bitcoindRpcClientF
  _ <- BitcoindRpcTestUtil.stopServers(Vector(bitcoind))
} yield ()


val resultF = for {
  _ <- stop1F
  _ <- stop2F
  _ <- stoppedBitcoindF
  _ <- system.terminate()
} yield ()

Await.result(resultF, 180.seconds)
```

### Other modules

You may find useful testkit functionality for other modules here

1. [Chain](/api/org/bitcoins/testkit/chain/ChainUnitTest)
2. [Key Manager](/api/org/bitcoins/testkit/keymanager/KeyManagerApiUnitTest)
3. [Wallet](/api/org/bitcoins/testkit/wallet/BitcoinSWalletTest)
4. [Node](/api/org/bitcoins/testkit/node/NodeUnitTest)

In general, you will find constructors and destructors of fixtures that can be useful when testing your applications
if you are using any of those modules.
