package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

/** Represents the state of specific DLC for a given party.
  * This state is made up of all messages that have been
  * passed back and forth as well as any relevant on-chain
  * transactions and oracle signatures.
  */
sealed trait DLCStatus {
  def isInitiator: Boolean
  def offer: DLCOffer
  def state: DLCState
  val tempContractId: Sha256Digest = offer.tempContractId
  val statusString: String = state.toString
}

/** All states other than Offered contain an accept message. */
sealed trait AcceptedDLCStatus extends DLCStatus {
  def accept: DLCAccept
  // TODO: add contractId when we can calc, currently cannot because app-commons doesn't depend on DLC
}

sealed trait SignedDLCStatus extends AcceptedDLCStatus {
  def sign: DLCSign
  val contractId: ByteVector = sign.contractId
}

object DLCStatus {

  /** The state where an offer has been created but no
    * accept message has yet been created/received.
    */
  case class Offered(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer)
      extends DLCStatus {
    override def state: DLCState = DLCState.Offered

    def toAccepted(accept: DLCAccept): Accepted = {
      Accepted(eventId, isInitiator, offer, accept)
    }
  }

  /** The state where an offer has been accepted but
    * no sign message has yet been created/received.
    */
  case class Accepted(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept)
      extends AcceptedDLCStatus {
    override def state: DLCState = DLCState.Accepted

    def toSigned(sign: DLCSign): Signed = {
      Signed(eventId, isInitiator, offer, accept, sign)
    }
  }

  /** The state where the initiating party has created
    * a sign message in response to an accept message
    * but the DLC funding transaction has not yet been
    * broadcasted to the network.
    */
  case class Signed(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign)
      extends SignedDLCStatus {
    override def state: DLCState = DLCState.Signed

    def toBroadcasted(fundingTx: Transaction): Broadcasted = {
      Broadcasted(eventId, isInitiator, offer, accept, sign, fundingTx)
    }

    def toConfirmed(fundingTx: Transaction): Confirmed = {
      toBroadcasted(fundingTx).toConfirmed
    }
  }

  /** The state where the accepting (non-initiating)
    * party has broadcasted the DLC funding transaction
    * to the blockchain, and it has not yet been confirmed.
    */
  case class Broadcasted(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends SignedDLCStatus {
    override def state: DLCState = DLCState.Broadcasted

    def toConfirmed: Confirmed = {
      Confirmed(eventId, isInitiator, offer, accept, sign, fundingTx)
    }
  }

  /** The state where the DLC funding transaction has been
    * confirmed on-chain and no execution paths have yet been
    * initiated.
    */
  case class Confirmed(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends SignedDLCStatus {
    override def state: DLCState = DLCState.Confirmed

    def toClaimed(
        oracleSig: SchnorrDigitalSignature,
        cet: Transaction): Claimed = {
      Claimed(eventId,
              isInitiator,
              offer,
              accept,
              sign,
              fundingTx,
              oracleSig,
              cet)
    }

    def toRemoteClaimed(cet: Transaction): RemoteClaimed = {
      RemoteClaimed(eventId, isInitiator, offer, accept, sign, fundingTx, cet)
    }
  }

  /** The state where one of the CETs has been accepted by the network
    * and executed by ourselves.
    */
  case class Claimed(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      oracleSig: SchnorrDigitalSignature,
      cet: Transaction)
      extends SignedDLCStatus {
    override def state: DLCState = DLCState.Claimed
  }

  /** The state where one of the CETs has been accepted by the network
    * and executed by a remote party.
    */
  case class RemoteClaimed(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction)
      extends SignedDLCStatus {
    override def state: DLCState = DLCState.RemoteClaimed

    val oracleSig: SchnorrDigitalSignature = {
      val cetSigs = cet
        .asInstanceOf[WitnessTransaction]
        .witness
        .head
        .asInstanceOf[P2WSHWitnessV0]
        .signatures
      val oraclePubKey = offer.oracleInfo.pubKey
      val preCommittedR = offer.oracleInfo.rValue

      def sigFromMsgAndSigs(
          msg: Sha256Digest,
          adaptorSig: ECAdaptorSignature,
          cetSig: ECDigitalSignature): SchnorrDigitalSignature = {
        val sigPubKey = oraclePubKey.computeSigPoint(msg.bytes, preCommittedR)
        val possibleOracleS =
          sigPubKey
            .extractAdaptorSecret(adaptorSig,
                                  ECDigitalSignature(cetSig.bytes.init))
            .fieldElement
        SchnorrDigitalSignature(preCommittedR, possibleOracleS)
      }

      val outcomeValues = cet.outputs.map(_.value).sorted
      val totalCollateral = offer.totalCollateral + accept.totalCollateral

      val possibleMessages = offer.contractInfo.filter {
        case (_, amt) =>
          Vector(amt, totalCollateral - amt)
            .filter(_ >= Policy.dustThreshold)
            .sorted == outcomeValues
      }.keys

      val (offerCETSig, acceptCETSig) =
        if (
          offer.pubKeys.fundingKey.hex.compareTo(
            accept.pubKeys.fundingKey.hex) > 0
        ) {
          (cetSigs.last, cetSigs.head)
        } else {
          (cetSigs.head, cetSigs.last)
        }

      val (cetSig, outcomeSigs) = if (isInitiator) {
        val possibleOutcomeSigs = sign.cetSigs.outcomeSigs.filter {
          case (msg, _) => possibleMessages.exists(_ == msg)
        }
        (acceptCETSig, possibleOutcomeSigs)
      } else {
        val possibleOutcomeSigs = accept.cetSigs.outcomeSigs.filter {
          case (msg, _) => possibleMessages.exists(_ == msg)
        }
        (offerCETSig, possibleOutcomeSigs)
      }

      val sigOpt = outcomeSigs.find {
        case (msg, adaptorSig) =>
          val possibleOracleSig = sigFromMsgAndSigs(msg, adaptorSig, cetSig)
          oraclePubKey.verify(msg.bytes, possibleOracleSig)
      }

      sigOpt match {
        case Some((msg, adaptorSig)) =>
          sigFromMsgAndSigs(msg, adaptorSig, cetSig)
        case None =>
          throw new IllegalArgumentException(
            "No Oracle Signature found from CET")
      }
    }
  }

  /** The state where the DLC refund transaction has been
    * accepted by the network.
    */
  case class Refunded(
      eventId: Sha256Digest,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      refundTx: Transaction)
      extends SignedDLCStatus {
    override def state: DLCState = DLCState.Refunded
  }
}
