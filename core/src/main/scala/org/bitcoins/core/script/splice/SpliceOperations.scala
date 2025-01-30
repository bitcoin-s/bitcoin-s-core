package org.bitcoins.core.script.splice

import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.constant.ScriptOperation

/** Created by chris on 1/22/16.
  */
sealed trait SpliceOperation extends ScriptOperation

case object OP_CAT extends SpliceOperation {
  override val opCode: Int = 126
}

case object OP_SUBSTR extends SpliceOperation {
  override val opCode: Int = 127
}

case object OP_LEFT extends SpliceOperation {
  override val opCode: Int = 128
}

case object OP_RIGHT extends SpliceOperation {
  override val opCode: Int = 129
}

case object OP_SIZE extends SpliceOperation {
  override val opCode: Int = 130
}

object SpliceOperation extends ScriptOperationFactory[SpliceOperation] {

  override val operations: scala.collection.immutable.Vector[
    org.bitcoins.core.script.splice.SpliceOperation
      & Product
      & java.io.Serializable] =
    Vector(OP_CAT, OP_LEFT, OP_RIGHT, OP_SIZE, OP_SUBSTR)
}
