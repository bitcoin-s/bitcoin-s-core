package org.bitcoins.wallet.api

import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.crypto.ECPublicKey

/**
  * This class represents the result of querying for address info
  * from our wallet
  */
case class AddressInfo(
    pubkey: ECPublicKey,
    network: NetworkParameters,
    path: HDPath)
