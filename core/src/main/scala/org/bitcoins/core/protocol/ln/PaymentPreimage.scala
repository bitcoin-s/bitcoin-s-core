package org.bitcoins.core.protocol.ln

import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Payment preimage for generating LN invoices.
  */
case class PaymentPreimage(bytes: ByteVector) extends NetworkElement {
  require(bytes.size == 32, s"Payment preimage size must be 32 bytes")

  lazy val hash: Sha256Digest = CryptoUtil.sha256(bytes)
}

object PaymentPreimage extends Factory[PaymentPreimage] {

  override def fromBytes(bytes: ByteVector): PaymentPreimage = {
    new PaymentPreimage(bytes)
  }

  def random: PaymentPreimage = fromBytes(ECPrivateKey.freshPrivateKey.bytes)

}
