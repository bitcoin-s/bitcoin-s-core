package org.bitcoins.server.routes

/** HTTP errors our server knows how to handle.
  * These gets picked up by the exceptions handler
  * in Main
  */
sealed abstract class HttpError extends Error

object HttpError {

  /** The RPC method was not found */
  case class MethodNotFound(method: String) extends HttpError
}
