---
id: version-1.9.10-sign
title: Sign API
original_id: sign
---

### The [`Sign` API](/api/org/bitcoins/crypto/Sign)

This is the API we define to sign things with. It takes in an arbitrary byte vector and returns a `Future[ECDigitalSignature]`. The reason we incorporate `Future`s here is for extensibility of this API. We would like to provide implementations of this API for hardware devices, which need to be asynchronous since they may require user input.

From [Sign.scala](/api/org/bitcoins/crypto/Sign):

```scala
import scodec.bits._
import org.bitcoins.crypto._
import scala.concurrent._
import scala.concurrent.duration._

trait Sign {
  def signFunction: ByteVector => Future[ECDigitalSignature]

  def signFuture(bytes: ByteVector): Future[ECDigitalSignature] =
    signFunction(bytes)

  def sign(bytes: ByteVector): ECDigitalSignature = {
    Await.result(signFuture(bytes), 30.seconds)
  }

  def publicKey: ECPublicKey
}
```

The `ByteVector` that is input to the `signFunction` should be the hash that is output from [`TransactionSignatureSerializer`](/api/org/bitcoins/core/crypto/TransactionSignatureSerializer)'s `hashForSignature` method. Our in-memory [`BaseECKey`](/api/org/bitcoins/crypto/BaseECKey) types implement the `Sign` API.

If you wanted to implement a new `Sign` api for a hardware wallet, you can easily pass it into the `TxBuilder`/`Signer` classes to allow for you to use those devices to sign with Bitcoin-S.

This API is currently used to sign ordinary transactions with our [`Signer`](/api/org/bitcoins/core/wallet/signer/Signer)s. The `Signer` subtypes (i.e. `P2PKHSigner`) implement the specific functionality needed to produce a valid digital signature for their corresponding script type.


### The [`ExtSign`](/api/org/bitcoins/crypto/Sign) API.

An [ExtKey](/api/org/bitcoins/core/crypto/ExtKey) is a data structure that can be used to generate more keys from a parent key. For more information look at [hd-keys.md](../core/hd-keys.md)

You can sign with `ExtPrivateKey` the same way you could with a normal `ECPrivateKey`.

```scala
import org.bitcoins.core.hd._
import org.bitcoins.core.crypto._

val extPrivKey = ExtPrivateKey(ExtKeyVersion.SegWitMainNetPriv)
// extPrivKey: ExtPrivateKey = Masked(ExtPrivateKeyImpl)

extPrivKey.sign(DoubleSha256Digest.empty.bytes)
// res0: ECDigitalSignature = ECDigitalSignature(3045022100c2a83f9acfd45e7cf1154d4ea9b2008d38fac981e6090bcc22d366b55ec5dee8022061fe305b4f64bb21919ac942a8440067f4fe0e7644a176872590bb9ceb71c035)

val path = BIP32Path(Vector(BIP32Node(0,HardenedType.defaultOpt)))
// path: BIP32Path = m/0'

extPrivKey.sign(DoubleSha256Digest.empty.bytes,path)
// res1: ECDigitalSignature = ECDigitalSignature(304402206c6728cffcb23a4a897c2a6c300aab713af724bfe43aa654467d9de7ce934ccf022032c7b5c836b6b5c96e4b45526eaa37bd0f36190dd05c9e34fdd3306eab911820)
```

With `ExtSign`, you can use `ExtPrivateKey` to sign transactions inside of `TxBuilder` since `UTXOSpendingInfo` takes in `Sign` as a parameter. 

You can also provide a `path` to use to derive a child `ExtPrivateKey`, and then sign with that child private key
