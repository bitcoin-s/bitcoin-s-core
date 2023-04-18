package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.crypto.StringFactory

sealed abstract class DLCState {
  def order: Int
}

object DLCState extends StringFactory[DLCState] {

  sealed abstract class InProgressState extends DLCState

  /** Means that someone has attempted to claim the DLC */
  sealed abstract class ClosedState extends DLCState

  /** A state meant to represent we are computing adaptor sigs.
    * Computing adaptor signatures can take awhile on certain hardware
    * (raspberry pi) which is why we model this state
    */
  sealed abstract class AdaptorSigComputationState extends InProgressState

  /** A state that requires an oracle outcome to be valid */
  sealed trait ClosedViaOracleOutcomeState extends ClosedState

  /** The state where an offer has been created but no
    * accept message has yet been created/received.
    */
  case object Offered extends InProgressState {
    override val order: Int = 0
  }

  case object AcceptComputingAdaptorSigs extends AdaptorSigComputationState {
    override val order: Int = 1
  }

  /** The state where an offer has been accepted but
    * no sign message has yet been created/received.
    */
  case object Accepted extends InProgressState {
    override val order: Int = 2
  }

  case object SignComputingAdaptorSigs extends AdaptorSigComputationState {
    override val order: Int = 3
  }

  /** The state where the initiating party has created
    * a sign message in response to an accept message
    * but the DLC funding transaction has not yet been
    * broadcasted to the network.
    */
  case object Signed extends InProgressState {
    override val order: Int = 4
  }

  /** The state where the accepting (non-initiating)
    * party has broadcasted the DLC funding transaction
    * to the blockchain, and it has not yet been confirmed.
    */
  case object Broadcasted extends InProgressState {
    override val order: Int = 5
  }

  /** The state where the DLC funding transaction has been
    * confirmed on-chain and no execution paths have yet been
    * initiated.
    */
  case object Confirmed extends InProgressState {
    override val order: Int = 6
  }

  /** The state where one of the CETs has been accepted by the network
    * and executed by ourselves.
    */
  case object Claimed extends ClosedViaOracleOutcomeState {
    override val order: Int = 7
  }

  /** The state where one of the CETs has been accepted by the network
    * and executed by a remote party.
    */
  case object RemoteClaimed extends ClosedViaOracleOutcomeState {
    override val order: Int = 7
  }

  /** The state where the DLC refund transaction has been
    * accepted by the network.
    */
  case object Refunded extends ClosedState {
    val order: Int = 7
  }

  val all: Vector[DLCState] = Vector(Offered,
                                     AcceptComputingAdaptorSigs,
                                     Accepted,
                                     SignComputingAdaptorSigs,
                                     Signed,
                                     Broadcasted,
                                     Confirmed,
                                     Claimed,
                                     RemoteClaimed,
                                     Refunded)

  /** The states where you can cancel a DLC in your wallet */
  val cancellableState = Vector(
    Offered,
    AcceptComputingAdaptorSigs,
    Accepted,
    SignComputingAdaptorSigs,
    Signed
  )

  val closedStates: Vector[ClosedState] = Vector(
    Claimed,
    RemoteClaimed,
    Refunded
  )

  override def fromString(str: String): DLCState = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase) match {
      case Some(state) => state
      case None =>
        throw new IllegalArgumentException(s"$str is not a valid DLCState")
    }
  }

  implicit val dlcStateOrdering: Ordering[DLCState] =
    (x: DLCState, y: DLCState) => x.order compare y.order
}
