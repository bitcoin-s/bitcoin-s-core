package org.bitcoins.core.script.bitwise

import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.constant.ScriptOperation

/** Created by chris on 1/6/16.
  */
sealed trait BitwiseOperation extends ScriptOperation

/** Returns 1 if the inputs are exactly equal, 0 otherwise. */
case object OP_EQUAL extends BitwiseOperation {
  override val opCode: Int = 135
}

/** Same as [[OP_EQUAL]], but runs
  * [[org.bitcoins.core.script.control.OP_VERIFY]] afterward.
  */
case object OP_EQUALVERIFY extends BitwiseOperation {
  override val opCode: Int = 136
}

/** Flips all of the bits in the input. disabled. */
case object OP_INVERT extends BitwiseOperation {
  override val opCode: Int = 131
}

/** Boolean and between each bit in the inputs. disabled. */
case object OP_AND extends BitwiseOperation {
  override val opCode: Int = 132
}

/** Boolean or between each bit in the inputs. disabled. */
case object OP_OR extends BitwiseOperation {
  override val opCode: Int = 133
}

/** Boolean exclusive or between each bit in the inputs. disabled. */
case object OP_XOR extends BitwiseOperation {
  override val opCode: Int = 134
}

object BitwiseOperation extends ScriptOperationFactory[BitwiseOperation] {

  override val operations: scala.collection.immutable.Vector[
    org.bitcoins.core.script.bitwise.BitwiseOperation
      & Product
      & java.io.Serializable] =
    Vector(OP_EQUAL, OP_EQUALVERIFY, OP_INVERT, OP_AND, OP_OR, OP_XOR)
}
