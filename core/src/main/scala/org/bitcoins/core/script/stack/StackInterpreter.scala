package org.bitcoins.core.script.stack

import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.{
  ExecutionInProgressScriptProgram,
  StartedScriptProgram
}

import scala.util.{Failure, Success, Try}

/** Created by chris on 1/6/16.
  * Stack operations implemented in the script programming language
  * https://en.bitcoin.it/wiki/Script#Stack
  */
sealed abstract class StackInterpreter {

  /** Duplicates the element on top of the stack
    * expects the first element in script to be the OP_DUP operation.
    */
  def opDup(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_DUP),
            "Top of the script stack must be OP_DUP")
    program.stack match {
      case h :: _ =>
        program.updateStackAndScript(h :: program.stack, program.script.tail)
      case Nil =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** If the top stack value is not 0, duplicate it. */
  def opIfDup(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_IFDUP),
            "Top of the script stack must be OP_DUP")
    if (program.stack.nonEmpty) {
      if (program.stack.head == ScriptNumber.zero)
        return program.updateScript(program.script.tail)
      program.updateStackAndScript(program.stack.head :: program.stack,
                                   program.script.tail)
    } else {
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Puts the number of stack items onto the stack. */
  def opDepth(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_DEPTH),
            "Top of script stack must be OP_DEPTH")
    val stackSize = program.stack.size
    val numberToPush: ScriptNumber = ScriptNumber(stackSize)
    program.updateStackAndScript(numberToPush :: program.stack,
                                 program.script.tail)
  }

  /** Puts the input onto the top of the alt stack. Removes it from the main stack. */
  def opToAltStack(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_TOALTSTACK),
            "Top of script stack must be OP_TOALTSTACK")
    if (program.stack.nonEmpty) {
      program
        .updateStack(program.stack.tail)
        .updateScript(program.script.tail)
        .updateAltStack(program.stack.head :: program.altStack)
    } else {
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Puts the input onto the top of the main stack. Removes it from the alt stack. */
  def opFromAltStack(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_FROMALTSTACK),
            "Top of script stack must be OP_FROMALTSTACK")
    if (program.altStack.nonEmpty) {
      program
        .updateStack(program.altStack.head :: program.stack)
        .updateScript(program.script.tail)
        .updateAltStack(program.altStack.tail)
    } else {
      program.failExecution(ScriptErrorInvalidAltStackOperation)
    }
  }

  /** Removes the top stack item. */
  def opDrop(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_DROP),
            "Top of script stack must be OP_DROP")
    if (program.stack.nonEmpty) {
      program.updateStackAndScript(program.stack.tail, program.script.tail)
    } else {
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Removes the second-to-top stack item. */
  def opNip(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_NIP),
            "Top of script stack must be OP_NIP")
    program.stack match {
      case h :: _ :: t =>
        program.updateStackAndScript(h :: t, program.script.tail)
      case _ :: _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
      case Nil =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Copies the second-to-top stack item to the top. */
  def opOver(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_OVER),
            "Top of script stack must be OP_OVER")
    program.stack match {
      case _ :: h1 :: _ =>
        program.updateStackAndScript(h1 :: program.stack, program.script.tail)
      case _ :: _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
      case Nil =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** The item n back in the stack is copied to the top. */
  def opPick(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_PICK),
            "Top of script stack must be OP_PICK")
    executeOpWithStackTopAsNumberArg(
      program,
      { (number: ScriptNumber) =>
        //check if n is within the bound of the script
        if (program.stack.size < 2)
          program.failExecution(ScriptErrorInvalidStackOperation)
        else if (
          number.toLong >= 0 && number.toLong < program.stack.tail.size
        ) {
          val newStackTop = program.stack.tail(number.toInt)
          program.updateStackAndScript(newStackTop :: program.stack.tail,
                                       program.script.tail)
        } else {
          program.failExecution(ScriptErrorInvalidStackOperation)
        }
      }
    )
  }

  /** The item n back in the stack is moved to the top. */
  def opRoll(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_ROLL),
            "Top of script stack must be OP_ROLL")
    executeOpWithStackTopAsNumberArg(
      program,
      (number: ScriptNumber) =>
        if (program.stack.size < 2)
          program.failExecution(ScriptErrorInvalidStackOperation)
        else if (
          number.toLong >= 0 && number.toLong < program.stack.tail.size
        ) {
          val newStackTop = program.stack.tail(number.toInt)
          //removes the old instance of the stack top, appends the new index to the head
          val newStack = newStackTop :: program.stack.tail
            .diff(List(newStackTop))
          program.updateStackAndScript(newStack, program.script.tail)
        } else {
          program.failExecution(ScriptErrorInvalidStackOperation)
        }
    )
  }

  /** The top three items on the stack are rotated to the left.
    * Ex: x1 x2 x3 -> x2 x3 x1
    */
  def opRot(program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_ROT),
            "Top of script stack must be OP_ROT")
    program.stack match {
      case h :: h1 :: h2 :: t =>
        val newStack = h2 :: h :: h1 :: t
        program.updateStackAndScript(newStack, program.script.tail)
      case _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** The fifth and sixth items back are moved to the top of the stack.
    * Ex. x1 x2 x3 x4 x5 x6 -> x3 x4 x5 x6 x1 x2
    */
  def op2Rot(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_2ROT),
            "Top of script stack must be OP_2ROT")
    program.stack match {
      case h :: h1 :: h2 :: h3 :: h4 :: h5 :: t =>
        val newStack = h4 :: h5 :: h :: h1 :: h2 :: h3 :: t
        program.updateStackAndScript(newStack, program.script.tail)
      case _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Removes the top two stack items. */
  def op2Drop(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_2DROP),
            "Top of script stack must be OP_2DROP")
    if (program.stack.size > 1) {
      program.updateStackAndScript(program.stack.tail.tail, program.script.tail)
    } else {
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** The top two items on the stack are swapped. */
  def opSwap(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_SWAP),
            "Top of script stack must be OP_SWAP")
    if (program.stack.size > 1) {
      val newStack =
        program.stack.tail.head :: program.stack.head :: program.stack.tail.tail
      program.updateStackAndScript(newStack, program.script.tail)
    } else {
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** The item at the top of the stack is copied and inserted before the second-to-top item. */
  def opTuck(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_TUCK),
            "Top of script stack must be OP_TUCK")
    program.stack match {
      case h :: h1 :: t =>
        val newStack = h :: h1 :: h :: t
        program.updateStackAndScript(newStack, program.script.tail)
      case _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Duplicates the top two stack items. */
  def op2Dup(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_2DUP),
            "Top of script stack must be OP_2DUP")
    program.stack match {
      case h :: h1 :: t =>
        val newStack = h :: h1 :: h :: h1 :: t
        program.updateStackAndScript(newStack, program.script.tail)
      case _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Duplicates the top three stack items. */
  def op3Dup(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_3DUP),
            "Top of script stack must be OP_3DUP")
    program.stack match {
      case h :: h1 :: h2 :: t =>
        val newStack = h :: h1 :: h2 :: h :: h1 :: h2 :: t
        program.updateStackAndScript(newStack, program.script.tail)
      case _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Copies the pair of items two spaces back in the stack to the front. */
  def op2Over(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_2OVER),
            "Top of script stack must be OP_2OVER")
    program.stack match {
      case h :: h1 :: h2 :: h3 :: t =>
        val newStack = h2 :: h3 :: h :: h1 :: h2 :: h3 :: t
        program.updateStackAndScript(newStack, program.script.tail)
      case _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Swaps the top two pairs of items. */
  def op2Swap(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_2SWAP),
            "Top of script stack must be OP_2SWAP")
    program.stack match {
      case h :: h1 :: h2 :: h3 :: t =>
        val newStack = h2 :: h3 :: h :: h1 :: t
        program.updateStackAndScript(newStack, program.script.tail)
      case _ =>
        program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Executes an operation with the stack top inside of the program as the argument
    * @param program the program whose stack top is used as an argument for the operation
    * @param op the operation that is executed with the script number on the top of the stack
    * @return the program with the result of the op pushed onto to the top of the stack
    */
  private def executeOpWithStackTopAsNumberArg(
      program: ExecutionInProgressScriptProgram,
      op: ScriptNumber => StartedScriptProgram): StartedScriptProgram = {
    program.stack.head match {
      case scriptNum: ScriptNumber => op(scriptNum)
      case _: ScriptToken          =>
        //interpret the stack top as a number
        val number: Try[ScriptNumber] = ScriptNumber(
          program.stack.head.bytes,
          ScriptFlagUtil.requireMinimalData(program.flags))
        number match {
          case Success(n) => op(n)
          case Failure(_) =>
            program.failExecution(ScriptErrorUnknownError)
        }
    }
  }
}

object StackInterpreter extends StackInterpreter
