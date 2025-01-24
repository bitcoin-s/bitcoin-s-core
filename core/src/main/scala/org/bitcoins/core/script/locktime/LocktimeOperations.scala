package org.bitcoins.core.script.locktime

import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.constant.ScriptOperation

/** Created by chris on 1/6/16.
  */
sealed trait LocktimeOperation extends ScriptOperation

/** Marks transaction as invalid if the top stack item is greater than the
  * transaction's nLockTime field, otherwise script evaluation continues as
  * though an OP_NOP was executed. Transaction is also invalid if
  *   1. the stack is empty; or 2. the top stack item is negative; or 3. the top
  *      stack item is greater than or equal to 500000000 while the
  *      transaction's nLockTime field is less than 500000000, or vice versa; or
  *      4. the input's nSequence field is equal to 0xffffffff. The precise
  *      semantics are described in BIP 0065
  */
case object OP_CHECKLOCKTIMEVERIFY extends LocktimeOperation {
  override val opCode: Int = 177
}

/** When executed, if any of the following conditions are true, the script
  * interpreter will terminate with an error: 1.) the stack is empty; or 2.) the
  * top item on the stack is less than 0; or 3.) the top item on the stack has
  * the disable flag (1 << 31) unset; and the transaction version is less than
  * 2; or the transaction input sequence number disable flag (1 << 31) is set;
  * or the relative lock-time type is not the same; or the top stack item is
  * greater than the transaction sequence (when masked according to the BIP68);
  * Otherwise, script execution will continue as if a NOP had been executed. See
  * BIP112 for more information
  * https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki
  */
case object OP_CHECKSEQUENCEVERIFY extends LocktimeOperation {
  override val opCode: Int = 178
}

object LocktimeOperation extends ScriptOperationFactory[LocktimeOperation] {

  override val operations: scala.collection.immutable.Vector[
    org.bitcoins.core.script.locktime.LocktimeOperation
      & Product
      & java.io.Serializable] =
    Vector(OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY)
}
