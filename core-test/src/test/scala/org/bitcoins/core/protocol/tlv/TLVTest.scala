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
      assert(TLV(unknown.bytes) == unknown)
    }
  }

  "ErrorTLV" must "have serialization symmetry" in {
    forAll(TLVGen.errorTLV) { error =>
      assert(ErrorTLV(error.bytes) == error)
      assert(TLV(error.bytes) == error)
    }
  }

  "PingTLV" must "have serialization symmetry" in {
    forAll(TLVGen.pingTLV) { ping =>
      assert(PingTLV(ping.bytes) == ping)
      assert(TLV(ping.bytes) == ping)
    }
  }

  "PongTLV" must "have serialization symmetry" in {
    forAll(TLVGen.pongTLV) { pong =>
      assert(PongTLV(pong.bytes) == pong)
      assert(TLV(pong.bytes) == pong)
    }
  }

  "ContractInfoV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.contractInfoV0TLV) { contractInfo =>
      assert(ContractInfoV0TLV(contractInfo.bytes) == contractInfo)
      assert(TLV(contractInfo.bytes) == contractInfo)
    }
  }

  "OracleInfoV0TLV" must "have serialization symmetry" in {
    forAll(TLVGen.oracleInfoV0TLV) { oracleInfo =>
      assert(OracleInfoV0TLV(oracleInfo.bytes) == oracleInfo)
      assert(TLV(oracleInfo.bytes) == oracleInfo)
    }
  }

  "FundingInputTempTLV" must "have serialization symmetry" in {
    forAll(TLVGen.fundingInputTempTLV) { fundingInput =>
      assert(FundingInputTempTLV(fundingInput.bytes) == fundingInput)
      assert(TLV(fundingInput.bytes) == fundingInput)
    }
  }

  "DLCOfferTLV" must "have serialization symmetry" in {
    forAll(TLVGen.dLCOfferTLV) { offer =>
      assert(DLCOfferTLV(offer.bytes) == offer)
      assert(TLV(offer.bytes) == offer)
    }
  }
}
