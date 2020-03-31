package org.bitcoins.core.wallet.fee

import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  WitnessTransaction
}

class FeeRateTest extends BitcoinSUnitTest {

  val tx: Transaction = Transaction(
    "02000000000101a2619b5d58b209439c937e563018efcf174063ca011e4f177a5b14e5ba76211c0100000017160014614e9b96cbc7477eda98f0936385ded6b636f74efeffffff024e3f57c4000000001600147cf00288c2c1b3c5cdf275db532a1c15c514bb2fae1112000000000016001440efb02597b9e9d9bc968f12cec3347e2e264c570247304402205768c2ac8178539fd44721e2a7541bedd6b55654f095143514624203c133f7e8022060d51f33fc2b5c1f51f26c7f703de21be6246dbb5fb7e1c6919aae6d442610c6012102b99a63f166ef53ca67a5c55ae969e80c33456e07189f8457e3438f000be42c19307d1900")

  it must "calculate the correct fee with a SatoshisPerByte fee rate" in {
    val feeRate = SatoshisPerByte(3.7)

    assert(feeRate.calc(tx) == Satoshis(503))
  }

  it must "calculate the correct fee with a SatoshisPerVirtualByte fee rate" in {
    val feeRate = SatoshisPerVirtualByte(3.7)

    assert(feeRate.calc(tx) == Satoshis(606))
  }

  it must "calculate the correct fee with a BitcoinsPerKiloByte fee rate" in {
    val feeRate = BitcoinsPerKiloByte(0.000037)

    assert(feeRate.calc(tx) == Satoshis(503))
  }

  it must "calculate the correct fee with a SatoshisPerKiloByte fee rate" in {
    val feeRate = SatoshisPerKiloByte(3700)

    assert(feeRate.calc(tx) == Satoshis(503))
  }

  it must "calculate the correct fee with a FlatSatoshis fee rate" in {
    val feeRate = FlatSatoshis(500)

    assert(feeRate.calc(tx) == Satoshis(500))
  }

  it must "have symmetry for SatoshisPerByte and SatoshisPerVirtualByte with BaseTransactions" in {
    val baseTx = BaseTransaction(
      "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000")

    val satoshisPerByte = SatoshisPerByte(3.7)
    val satoshisPerVByte = SatoshisPerVirtualByte(3.7)

    assert(satoshisPerByte.calc(baseTx) == satoshisPerVByte.calc(baseTx))
  }

  it must "have NOT symmetry for SatoshisPerByte and SatoshisPerVirtualByte with WitnessTransactions" in {
    val wtx = WitnessTransaction(
      "02000000000101a2619b5d58b209439c937e563018efcf174063ca011e4f177a5b14e5ba76211c0100000017160014614e9b96cbc7477eda98f0936385ded6b636f74efeffffff024e3f57c4000000001600147cf00288c2c1b3c5cdf275db532a1c15c514bb2fae1112000000000016001440efb02597b9e9d9bc968f12cec3347e2e264c570247304402205768c2ac8178539fd44721e2a7541bedd6b55654f095143514624203c133f7e8022060d51f33fc2b5c1f51f26c7f703de21be6246dbb5fb7e1c6919aae6d442610c6012102b99a63f166ef53ca67a5c55ae969e80c33456e07189f8457e3438f000be42c19307d1900")

    val satoshisPerByte = SatoshisPerByte(3.7)
    val satoshisPerVByte = SatoshisPerVirtualByte(3.7)

    assert(satoshisPerByte.calc(wtx) != satoshisPerVByte.calc(wtx))
  }

  it must "correctly convert SatoshisPerByte to SatoshisPerKiloByte" in {
    val satoshisPerByte = SatoshisPerByte(3.7)

    assert(satoshisPerByte.toSatPerKb == SatoshisPerKiloByte(3700))
  }

  it must "correctly convert SatoshisPerByte to BitcoinsPerKiloByte" in {
    val satoshisPerByte = SatoshisPerByte(3.7)

    assert(satoshisPerByte.toBTCPerKb == BitcoinsPerKiloByte(0.000037))
  }

  it must "correctly convert SatoshisPerKiloByte to SatoshisPerByte" in {
    val satPerKb = SatoshisPerKiloByte(3700)

    assert(satPerKb.toSatPerByte == SatoshisPerByte(3.7))
  }

  it must "correctly convert BitcoinsPerKiloByte to SatoshisPerByte" in {
    val btcPerKb = BitcoinsPerKiloByte(0.000037)

    assert(btcPerKb.toSatPerByte == SatoshisPerByte(3.7))
  }

  it must "correctly convert SatoshisPerKiloByte to BitcoinsPerKiloByte" in {
    val satPerKb = SatoshisPerKiloByte(3700)
    val btcPerKb = BitcoinsPerKiloByte(0.000037)

    assert(satPerKb.toBTCPerKb == btcPerKb)
    assert(btcPerKb.toSatPerKb == satPerKb)
  }

  it must "correctly calculate the correct fee rate for a transaction" in {
    val inputAmount = Bitcoins(32.96382044)
    val satsPerByte = SatoshisPerByte.calc(tx, inputAmount)
    val satsPerVByte = SatoshisPerVirtualByte.calc(tx, inputAmount)
    val satsPerKByte = SatoshisPerKiloByte.calc(tx, inputAmount)
    val btcPerKByte = BitcoinsPerKiloByte.calc(tx, inputAmount)

    assert(satsPerByte == SatoshisPerByte(8424.235294117647))
    assert(satsPerVByte == SatoshisPerVirtualByte(6985.951219512195))
    assert(satsPerKByte == SatoshisPerKiloByte(8.424235294117647))
    assert(btcPerKByte == BitcoinsPerKiloByte(0.00000008424235294117647))
  }

  it must "fail to calculate a fee greater than a the max value for SatoshisPerByte" in {
    val feeRate = SatoshisPerByte(Satoshis.max.toDouble)

    assertThrows[IllegalArgumentException](feeRate.calc(tx))
  }

  it must "fail to calculate a fee greater than a the max value for SatoshisPerVirtualByte" in {
    val feeRate = SatoshisPerVirtualByte(Satoshis.max.toDouble)

    assertThrows[IllegalArgumentException](feeRate.calc(tx))
  }

  it must "fail to calculate a fee greater than a the max value for SatoshisPerKiloByte" in {
    val feeRate = SatoshisPerKiloByte(Satoshis.max.toDouble)

    assertThrows[IllegalArgumentException](feeRate.calc(tx))
  }

  it must "fail to calculate a fee greater than a the max value for BitcoinsPerKiloByte" in {
    val feeRate = BitcoinsPerKiloByte(Satoshis.max.toDouble)

    assertThrows[IllegalArgumentException](feeRate.calc(tx))
  }

  it must "fail to calculate a fee greater than a the max value for FlatSatoshis" in {
    assertThrows[IllegalArgumentException](FlatSatoshis(Satoshis.max.toLong))
  }
}
