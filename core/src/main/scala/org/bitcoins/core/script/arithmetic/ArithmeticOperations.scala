package org.bitcoins.core.script.arithmetic

import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.constant.ScriptOperation

/** Created by chris on 1/6/16. */
sealed abstract class ArithmeticOperation extends ScriptOperation

/** 1 is added to the input. */
case object OP_1ADD extends ArithmeticOperation {
  override val opCode: Int = 139
}

/** 1 is subtracted from the input. */
case object OP_1SUB extends ArithmeticOperation {
  override val opCode: Int = 140
}

/** The sign of the input is flipped. */
case object OP_NEGATE extends ArithmeticOperation {
  override val opCode: Int = 143
}

/** The input is made positive. */
case object OP_ABS extends ArithmeticOperation {
  override val opCode: Int = 144
}

/** If the input is 0 or 1, it is flipped. Otherwise the output will be 0. */
case object OP_NOT extends ArithmeticOperation {
  override val opCode: Int = 145
}

/** Returns 0 if the input is 0. 1 otherwise. */
case object OP_0NOTEQUAL extends ArithmeticOperation {
  override val opCode: Int = 146
}

/** a is added to b. */
case object OP_ADD extends ArithmeticOperation {
  override val opCode: Int = 147
}

/** b is subtracted from a. */
case object OP_SUB extends ArithmeticOperation {
  override val opCode: Int = 148
}

/** If both a and b are not 0, the output is 1. Otherwise 0. */
case object OP_BOOLAND extends ArithmeticOperation {
  override val opCode: Int = 154
}

/** If a or b is not 0, the output is 1. Otherwise 0. */
case object OP_BOOLOR extends ArithmeticOperation {
  override val opCode: Int = 155
}

/** Returns 1 if the numbers are equal, 0 otherwise. */
case object OP_NUMEQUAL extends ArithmeticOperation {
  override val opCode: Int = 156
}

/** Same as OP_NUMEQUAL, but runs OP_VERIFY afterward. */
case object OP_NUMEQUALVERIFY extends ArithmeticOperation {
  override val opCode: Int = 157
}

/** Returns 1 if the numbers are not equal, 0 otherwise. */
case object OP_NUMNOTEQUAL extends ArithmeticOperation {
  override val opCode: Int = 158
}

/** Returns 1 if a is less than b, 0 otherwise. */
case object OP_LESSTHAN extends ArithmeticOperation {
  override val opCode: Int = 159
}

/** Returns 1 if a is greater than b, 0 otherwise. */
case object OP_GREATERTHAN extends ArithmeticOperation {
  override val opCode: Int = 160
}

/** Returns 1 if a is less than or equal to b, 0 otherwise. */
case object OP_LESSTHANOREQUAL extends ArithmeticOperation {
  override val opCode: Int = 161
}

/** Returns 1 if a is greater than or equal to b, 0 otherwise. */
case object OP_GREATERTHANOREQUAL extends ArithmeticOperation {
  override val opCode: Int = 162
}

/** Returns the smaller of a and b. */
case object OP_MIN extends ArithmeticOperation {
  override val opCode: Int = 163
}

/** Returns the larger of a and b. */
case object OP_MAX extends ArithmeticOperation {
  override val opCode: Int = 164
}

/** Returns 1 if x is within the specified range (left-inclusive), 0 otherwise.
  */
case object OP_WITHIN extends ArithmeticOperation {
  override val opCode: Int = 165
}

//currently disabled operations

/** The input is multiplied by 2. disabled. */
case object OP_2MUL extends ArithmeticOperation {
  override val opCode: Int = 141
}

/** The input is divided by 2. disabled. */
case object OP_2DIV extends ArithmeticOperation {
  override val opCode: Int = 142
}

/** a is multiplied by b. disabled. */
case object OP_MUL extends ArithmeticOperation {
  override val opCode: Int = 149
}

/** a is divided by b. disabled. */
case object OP_DIV extends ArithmeticOperation {
  override val opCode: Int = 150
}

/** Returns the remainder after dividing a by b. disabled. */
case object OP_MOD extends ArithmeticOperation {
  override val opCode: Int = 151
}

/** Shifts a left b bits, preserving sign. disabled. */
case object OP_LSHIFT extends ArithmeticOperation {
  override val opCode: Int = 152
}

/** Shifts a right b bits, preserving sign. disabled. */
case object OP_RSHIFT extends ArithmeticOperation {
  override val opCode: Int = 153
}

object ArithmeticOperation extends ScriptOperationFactory[ArithmeticOperation] {

  override val operations: scala.collection.immutable.Vector[
    org.bitcoins.core.script.arithmetic.ArithmeticOperation
      & Product
      & java.io.Serializable] = Vector(
    OP_0NOTEQUAL,
    OP_1ADD,
    OP_1SUB,
    OP_ABS,
    OP_ADD,
    OP_BOOLAND,
    OP_BOOLOR,
    OP_GREATERTHAN,
    OP_GREATERTHANOREQUAL,
    OP_LESSTHAN,
    OP_LESSTHANOREQUAL,
    OP_MAX,
    OP_MIN,
    OP_NEGATE,
    OP_NEGATE,
    OP_NOT,
    OP_NUMEQUAL,
    OP_NUMEQUALVERIFY,
    OP_NUMNOTEQUAL,
    OP_SUB,
    OP_WITHIN,
    OP_2MUL,
    OP_2DIV,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_LSHIFT,
    OP_RSHIFT
  )
}
