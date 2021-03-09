package org.bitcoins.crypto

import org.bitcoins.testkitcore.gen.CryptoGenerators
import org.bitcoins.testkitcore.util.BitcoinSSyncTest

class BCryptoECDigitalSignatureTest extends BitcoinSSyncTest {

  behavior of "BCryptoECDigitalSignatureTest"

  it must "be able to generate valid signatures with bouncy castle" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.sha256Digest) {
      case (privKey: ECPrivateKey, hash: Sha256Digest) =>
        val sig = BCryptoCryptoRuntime.sign(privKey, hash.bytes)
        val pubKey = privKey.publicKey

        assert(pubKey.verify(hash, sig))
    }
  }
}
