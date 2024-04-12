package org.bitcoins.node.models

import org.bitcoins.core.protocol.transaction.Transaction

/** TXs we can broadcast over the P2P network */
case class BroadcastAbleTransaction(transaction: Transaction)
