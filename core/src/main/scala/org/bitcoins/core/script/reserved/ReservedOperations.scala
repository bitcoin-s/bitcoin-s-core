package org.bitcoins.core.script.reserved

import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.constant.ScriptOperation

/** Created by chris on 1/22/16.
  */
/** Reserved words Any opcode not assigned is also reserved. Using an unassigned
  * opcode makes the transaction invalid.
  * https://en.bitcoin.it/wiki/Script#Reserved_words
  */
sealed trait ReservedOperation extends ScriptOperation

/** Transaction is invalid unless occuring in an unexecuted OP_IF branch
  */
case object OP_RESERVED extends ReservedOperation {
  override val opCode: Int = 80
}

/** Transaction is invalid unless occuring in an unexecuted OP_IF branch
  */
case object OP_VER extends ReservedOperation {
  override val opCode: Int = 98
}

/** Transaction is invalid even when occuring in an unexecuted OP_IF branch
  */
case object OP_VERIF extends ReservedOperation {
  override val opCode: Int = 101
}

/** Transaction is invalid even when occuring in an unexecuted OP_IF branch
  */
case object OP_VERNOTIF extends ReservedOperation {
  override val opCode: Int = 102
}

/** Transaction is invalid unless occuring in an unexecuted OP_IF branch
  */
case object OP_RESERVED1 extends ReservedOperation {
  override val opCode: Int = 137
}

/** Transaction is invalid unless occuring in an unexecuted OP_IF branch
  */
case object OP_RESERVED2 extends ReservedOperation {
  override val opCode: Int = 138
}

/** Represents an operation that means nothing inside the Script language
  */
sealed trait NOP extends ReservedOperation

case object OP_NOP extends NOP {
  override val opCode: Int = 97
}

case object OP_NOP1 extends NOP {
  override val opCode: Int = 176
}

case object OP_NOP4 extends NOP {
  override val opCode: Int = 179
}

case object OP_NOP5 extends NOP {
  override val opCode: Int = 180
}

case object OP_NOP6 extends NOP {
  override val opCode: Int = 181
}

case object OP_NOP7 extends NOP {
  override val opCode: Int = 182
}

case object OP_NOP8 extends NOP {
  override val opCode: Int = 183
}

case object OP_NOP9 extends NOP {
  override val opCode: Int = 184
}

case object OP_NOP10 extends NOP {
  override val opCode: Int = 185
}

case class UndefinedOP_NOP(opCode: Int) extends ReservedOperation

object ReservedOperation extends ScriptOperationFactory[ReservedOperation] {
  lazy val undefinedOpCodes = for { i <- 0xbb to 0xff } yield UndefinedOP_NOP(i)

  override val operations: scala.collection.immutable.Vector[
    org.bitcoins.core.script.reserved.ReservedOperation
      with Product
      with java.io.Serializable] =
    Vector(
      OP_RESERVED,
      OP_VER,
      OP_VERIF,
      OP_VERNOTIF,
      OP_RESERVED,
      OP_RESERVED1,
      OP_RESERVED2,
      OP_NOP,
      OP_NOP1,
      OP_NOP4,
      OP_NOP5,
      OP_NOP6,
      OP_NOP7,
      OP_NOP8,
      OP_NOP9,
      OP_NOP10
    ) ++ undefinedOpCodes
}
