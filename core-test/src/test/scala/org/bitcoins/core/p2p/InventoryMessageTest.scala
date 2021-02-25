package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class InventoryMessageTest extends BitcoinSUnitTest {

  it must " have serialization symmetry" in {
    forAll(DataMessageGenerator.inventoryMessages) { invMessage =>
      assert(InventoryMessage(invMessage.hex) == invMessage)
    }
  }

  it must "have a meaningful toString" in {
    forAll(DataMessageGenerator.inventoryMessages) { inv =>
      assert(inv.toString.length < 200)
    }
  }
}
