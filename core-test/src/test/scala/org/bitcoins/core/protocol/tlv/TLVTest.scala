package org.bitcoins.core.protocol.tlv

import org.bitcoins.testkit.core.gen.TLVGen
import org.bitcoins.testkit.util.BitcoinSUnitTest

class TLVTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  "TLV" must "have serizliation symmetry" in {
    forAll(TLVGen.tlv) { tlv =>
      assert(TLV(tlv.bytes) == tlv)
    }
  }

  "UnknownTLV" must "have serialization symmetry" in {
    forAll(TLVGen.unknownTLV) { unknown =>
      assert(UnknownTLV(unknown.bytes) == unknown)
    }
  }

  "ErrorTLV" must "have serialization symmetry" in {
    forAll(TLVGen.errorTLV) { error =>
      assert(ErrorTLV(error.bytes) == error)
    }
  }

  "PingTLV" must "have serialization symmetry" in {
    forAll(TLVGen.pingTLV) { ping =>
      assert(PingTLV(ping.bytes) == ping)
    }
  }

  "PongTLV" must "have serialization symmetry" in {
    forAll(TLVGen.pongTLV) { pong =>
      assert(PongTLV(pong.bytes) == pong)
    }
  }
}
