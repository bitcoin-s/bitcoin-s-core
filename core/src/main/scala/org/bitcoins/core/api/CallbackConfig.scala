package org.bitcoins.core.api

import org.bitcoins.core.api.callback.{CallbackFactory, ModuleCallbacks}
import org.bitcoins.core.util.Mutable

trait CallbackConfig[T <: ModuleCallbacks[T]] {

  private val atomicCallbacks: Mutable[T] = new Mutable(callbackFactory.empty)

  def isCallbackEmpty: Boolean =
    atomicCallbacks.atomicGet == callbackFactory.empty

  def addCallbacks(newCallbacks: T): T = {
    atomicCallbacks.atomicUpdate(newCallbacks) { case (t1, t2) =>
      t1.+(t2)
    }
  }

  def replaceCallbacks(newCallbacks: T): T = {
    atomicCallbacks.atomicSet(newCallbacks)
    newCallbacks
  }

  def callBacks: T = atomicCallbacks.atomicGet

  /** Clears all callbacks */
  def clearCallbacks(): Unit = {
    atomicCallbacks.atomicSet(callbackFactory.empty)
    ()
  }

  def callbackFactory: CallbackFactory[T]
}
