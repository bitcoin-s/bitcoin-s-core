package org.bitcoins.core.script.crypto

import org.bitcoins.core.script.ScriptOperationFactory

/** Created by chris on 3/24/16.
  */
trait CryptoSignatureEvaluationFactory
    extends ScriptOperationFactory[CryptoSignatureEvaluation] {

  /** The current [[CryptoSignatureEvaluation]] operations. */
  override val operations: scala.collection.immutable.Vector[
    org.bitcoins.core.script.crypto.CryptoSignatureEvaluation
      & Product
      & java.io.Serializable] =
    Vector(OP_CHECKMULTISIG,
           OP_CHECKMULTISIGVERIFY,
           OP_CHECKSIG,
           OP_CHECKSIGVERIFY,
           OP_CHECKSIGADD)

}

object CryptoSignatureEvaluationFactory extends CryptoSignatureEvaluationFactory
