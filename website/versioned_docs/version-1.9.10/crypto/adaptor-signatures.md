---
id: version-1.9.10-adaptor-signatures
title: Adaptor Signatures
original_id: adaptor-signatures
---

Bitcoin-S now has support for [ECDSA Adaptor Signatures](https://github.com/discreetlogcontracts/dlcspecs/blob/03bf7095c2016e1ce9c9fb612920872d4456f179/ECDSA-adaptor.md).

There are four relevant functions to adaptor signatures:

* `sign` (aka encrypt)
  * This function belongs to `ECPrivateKey` and creates an adaptor signature given a message (`ByteVector`) and an adaptor point (`ECPublicKey`).
* `verify`
  * Verifies an adaptor signature given the signing public key, the message and the adaptor point.
* `complete` (aka decrypt)
  * This function belongs to `ECPrivateKey` and computes a valid ECDSA signature given a valid adaptor signature whose adaptor point is this private key's public key.
* `extract` (aka recover)
  * This function belongs to `ECPublicKey` and computes the adaptor secret (private key to this public key) given a valid adaptor signature for this adaptor point, and the valid ECDSA signature computed using `complete`.

The following code shows each function to do with adaptor signature usage:


```scala
// Alice generates an adaptor signature using her private key and the adaptor point
val adaptorSig = privKey.adaptorSign(adaptorPoint, msg)

// Bob verifies this adaptor signature using Alice's public key and the adaptor point
require(pubKey.adaptorVerify(msg, adaptorPoint, adaptorSig))

// Bob computes a valid ECDSA signature using the adaptorSignature, which he knows
val sig = adaptorSecret.completeAdaptorSignature(adaptorSig)

// Anyone can validate this signature
require(pubKey.verify(msg, sig))

// Alice can compute the adaptor secret from the signatures
val secret = adaptorPoint.extractAdaptorSecret(adaptorSig, sig)
require(secret == adaptorSecret)
```

