---
id: version-1.9.10-server
title: Application Server
original_id: server
---


## App server

The server project is the aggregation of these three sub projects

1. [Wallet](../wallet/wallet.md)
2. [Chain](../chain/chain.md)
3. [Node](../node/node.md)

The server project provides a way to access information from these three projects via a JSON RPC.

## Building the server

### Java binary
You can build the server with the [sbt native packager](https://github.com/sbt/sbt-native-packager).
The native packager offers [numerous ways to package the project](https://github.com/sbt/sbt-native-packager#examples).

In this example we are going to use `stage` which will produce bash scripts we can easily execute. You can stage the server with the following command.

```bash
sbt appServer/universal:stage
```

This will produce a script to execute bitcoin-s which you can start with

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server
```

### Docker

The oracle server also has docker support. You can build a docker image with the following commands

#### Using an existing docker image

We publish docker images on every PR that is merged to bitcoin-s.

You can find the docker repo for the app server [here](https://hub.docker.com/r/bitcoinscala/bitcoin-s-server/tags?page=1&ordering=last_updated)

#### Building a docker image
```
sbt "appServer/docker:stage"
```

This will build a `Dockerfile` that is located in `app/server/target/docker/stage`

You can now build the docker image with

```
docker build app/server/target/docker/stage/ -t bitcoinscala/bitcoin-s-server:latest
```

Finally, let's run the image! It's important that you correctly configure port forwarding with the docker container so
you can interact with the running container with `bitcoin-s-cli` or `curl`. By default, our oracle
server listens for requests on port `9999`. By default, the server listens for websocket connections on port `19999` at `/events`.

This means we need to forward requests on the host machine to the docker container correctly.

This can be done with the following command
```
docker run -d -p 9999:9999 -p 19999:19999 -e BITCOIN_S_SERVER_RPC_PASSWORD='topsecret' bitcoinscala/bitcoin-s-server:latest
```

Now you can send requests with `bitcoin-s-cli` or `curl`.
Here is an example with `bitcoin-s-cli`
```
./bitcoin-s-cli getblockcount --password topsecret
10000
```

For more information on build configuration options with `sbt` please see the [sbt native packager docs](https://sbt-native-packager.readthedocs.io/en/latest/formats/docker.html#tasks)

## Configuration

### Java binary configuration
If you would like to pass in a custom datadir for your server, you can do

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --datadir /path/to/datadir/
```

To use a config file that is not the `bitcoin-s.conf` file in your datadir, you can do

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --conf /path/to/file.conf
```

You can also pass in a custom `rpcport` to bind to

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --rpcport 12345
```

Or set a custom `wsport` to bind to

```bash
./app/server/target/universal/stage/bin/bitcoin-s-server --wsport 54321
```

For more information on configuring the server please see our [configuration](../config/configuration.md) document

For more information on how to use our built in `cli` to interact with the server please see [cli.md](cli.md)

### Docker configuration

In this example, we are using the latest docker image published to our [docker hub](https://hub.docker.com/repository/docker/bitcoinscala/bitcoin-s-oracle-server/tags?page=1&ordering=last_updated)
which is referenced by `bitcoinscala/bitcoin-s-server:latest`

You can use bitcoin-s with docker volumes. You can also pass in a custom configuration at container runtime.

#### Using a docker volume

```basrc
docker volume create bitcoin-s
docker run -p 9999:9999 -p 19999:19999 \
--mount source=bitcoin-s,target=/home/bitcoin-s/ bitcoinscala/bitcoin-s-server:latest
```

Now you can re-use this volume across container runs. It will keep the same oracle database
and seeds directory located at `/home/bitcoin-s/.bitcoin-s/seeds` in the volume.

#### Using a custom bitcoin-s configuration with docker

You can also specify a custom bitcoin-s configuration at container runtime.
You can mount the configuration file on the docker container and that
configuration will be used in the docker container runtime rather than
the default one we provide [here](https://github.com/bitcoin-s/bitcoin-s/blob/master/app/oracle-server/src/universal/docker-application.conf)

You can do this with the following command

```bashrc
docker run -p 9999:9999 -p 19999:19999 \
--mount type=bind,source=/my/new/config/,target=/home/bitcoin-s/.bitcoin-s/ \
bitcoinscala/bitcoin-s-server:latest --conf /home/bitcoin-s/.bitcoin-s/bitcoin-s.conf
```

Note: If you adjust the `bitcoin-s.server.rpcport` setting you will need to adjust
the `-p 9999:9999` port mapping on the docker container to adjust for this.

## Server Endpoints

### Common 
 - `getversion` - The version of our application you are using
 - `zipdatadir` `location` - Backs up the datadir in a safe and consistent manner.
   - `location` - The locations of the backup zip

### Blockchain
 - `getblockcount` - Get the current block height
 - `getfiltercount` - Get the number of filters
 - `getfilterheadercount` - Get the number of filter headers
 - `getbestblockhash` - Get the best block hash
 - `getblockheader` - Returns information about block header <hash>
     - `hash` - The block hash
 - `decoderawtransaction` `tx` - `Decode the given raw hex transaction`
     - `tx` - Transaction encoded in hex to decode
 - `getmediantimepast` - Returns the median time past
 - `getinfo` returns general information about our blockchain state

### Wallet
 - `rescan` `[options]` - Rescan for wallet UTXOs
    - `--force` - Clears existing wallet records. Warning! Use with caution!
    - `--batch-size <value>` - Number of filters that can be matched in one batch
    - `--start <value>` - Start height
    - `--end <value>` - End height
    - `--ignorecreationtime` - Ignores the wallet creation date and will instead do a full rescan
 - `isempty` - Checks if the wallet contains any data
 - `walletinfo` - Returns data about the current wallet being used
 - `getbalance` `[options]` - Get the wallet balance
    - `--sats ` - Display balance in satoshis
 - `getconfirmedbalance` `[options]` - Get the wallet balance of confirmed utxos
    - `--sats ` - Display balance in satoshis
 - `getunconfirmedbalance` `[options]` - Get the wallet balance of unconfirmed utxos
    - `--sats ` - Display balance in satoshis
 - `getbalances` `[options]` - Get the wallet balance by utxo state
    - `--sats ` - Display balance in satoshis
 - `getutxos` - Returns list of all wallet utxos
 - `getaddresses` - Returns list of all wallet addresses currently being watched
 - `getspentaddresses` - Returns list of all wallet addresses that have received funds and been spent
 - `getfundedaddresses` - Returns list of all wallet addresses that are holding funds
 - `getunusedaddresses` - Returns list of all wallet addresses that have not been used
 - `getaccounts` - Returns list of all wallet accounts
 - `walletinfo` - Returns meta information about the wallet
 - `createnewaccount` `purpose` - Creates a new wallet account with the given HD purpose from [BIP43](https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki)
 - `getaddressinfo` `address` - Returns list of all wallet accounts
    - `address` - Address to get information about
 - `getnewaddress` - Get a new address
 - `listreservedutxos` - lists all utxos that are reserved in the wallet
 - `sendtoaddress` `address` `amount` `[options]` - Send money to the given address
    - `address` - Address to send to
    - `amount` - Amount to send in BTC
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `sendfromoutpoints` `outpoints` `address` `amount` `[options]` - Send money to the given address
    - `outpoints` - Out Points to send from
    - `address` - Address to send to
    - `amount` - Amount to send in BTC
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `sweepwallet` `address` `[options]` - Sends the entire wallet balance to the given address
    - `address` - Address to send to
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `sendwithalgo` `address` `amount` `algo` `[options]` - Send money to the given address using a specific coin selection algo
    - `address` - Address to send to
    - `amount` - Amount to send in BTC
    - `algo` - Coin selection algo
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `signpsbt` `psbt` - Signs the PSBT's inputs with keys that are associated with the wallet
    - `psbt` - PSBT to sign
 - `opreturncommit` `message` `[options]` - Creates OP_RETURN commitment transaction
    - `message` - message to put into OP_RETURN commitment
    - `--hashMessage` - should the message be hashed before commitment
    - `--feerate <value>` - Fee rate in sats per virtual byte
 - `bumpfeecpfp` `txid` `feerate` - Bump the fee of the given transaction id with a child tx using the given fee rate
    - `txid` - Id of transaction to bump fee
    - `feerate` - Fee rate in sats per virtual byte of the child transaction
 - `bumpfeerbf` `txid` `feerate` - Replace given transaction with one with the new fee rate
    - `txid` - Id of transaction to bump fee
    - `feerate` - New fee rate in sats per virtual byte
 - `gettransaction` `txid` - Get detailed information about in-wallet transaction <txid>
    - `txid` - The transaction id
 - `lockunspent` `unlock` `transactions` - Temporarily lock (unlock=false) or unlock (unlock=true) specified transaction outputs.
    - `unlock` - Whether to unlock (true) or lock (false) the specified transactions
    - `transactions` - The transaction outpoints to unlock/lock, empty to apply to all utxos
 - `importseed` `walletname` `words` `passphrase` - Imports a mnemonic seed as a new seed file
     - `walletname` - Name to associate with this seed
     - `words` - Mnemonic seed words, space separated
     - `passphrase` - Passphrase to encrypt this seed with
 - `importxprv` `walletname` `xprv` `passphrase` - Imports a mnemonic seed as a new seed file
     - `walletname` - Name to associate with this seed
     - `xprv` - base58 encoded extended private key
     - `passphrase` - Passphrase to encrypt this seed with
 - `exportseed` `walletname` `passphrase` - Exports the mnemonic seed phrase
    - `walletname` - Name to associate with this seed
    - `passphrase` - Passphrase to decrypt this seed with
 - `markseedasbackedup` `walletname` `passphrase` - Marks the seed as backed up. It prevents `exportseed` from returning the mnemonic phrase
    - `walletname` - Name to associate with this seed
    - `passphrase` - Passphrase to decrypt this seed with
 - `getseedbackuptime` `walletname` `passphrase` - Returns time when the seed was backed up
    - `walletname` - Name to associate with this seed
    - `passphrase` - Passphrase to decrypt this seed with
 - `keymanagerpassphrasechange` `oldpassphrase` `newpassphrase` - Changes the wallet passphrase
    - `oldpassphrase` - The current passphrase
    - `newpassphrase` - The new passphrase
 - `keymanagerpassphraseset` `passphrase` - Encrypts the wallet with the given passphrase
    - `passphrase` - The passphrase to encrypt the wallet with
 - `loadwallet` `walletname` `passphrase` - Loads a wallet 
    - `walletname` - Name to associate with this seed
    - `passphrase` - Passphrase to decrypt this seed with
 - `listwallets` - Returns a list of all wallets

### DLC
 - `createcontractinfo` `announcement` `totalCollateral` `payouts`
   - the announcement to build the contract info for
   - the total amount of collateral in the DLC
   - The payouts can be in two formats
     1. `{ "outcomes" : { "outcome0" : 0, "outcome1": 1, ... }}`. The number is the amount of sats paid to YOU for that outcome.
     2. `[{"outcome":0,"payout":0,"extraPrecision":0,"isEndpoint":true}, {"outcome":1,"payout":1,"extraPrecision":0,"isEndpoint":true}, ...]`
 - `decodecontractinfo` `contractinfo` - Decodes a contract info into json
   - `contractinfo` - Hex encoded contract info
 - `decodeoffer` `offer` - Decodes an offer message into json
    - `offer` - Hex encoded dlc offer message
 - `decodeaccept` `accept` - Decodes an accept message into json
   - `accept` - Hex encoded dlc accept message
 - `decodesign` `sign` - Decodes a sign message into json
   - `sign` - Hex encoded dlc sign message
 - `decodeannouncement` `announcement` - Decodes an oracle announcement message into json
    - `announcement` - Hex encoded oracle announcement message
 - `decodeattestments` `attestments` - Decodes an oracle attestments message into json
    - `attestments` - Hex encoded oracle attestments message
 - `getdlchostaddress` - Returns the public listening address of the DLC Node
 - `createdlcoffer` `contractInfo` `collateral` `[feerate]` `refundlocktime` `[options]` - Creates a DLC offer that another party can accept
    - `contractInfo` - Hex encoded contractInfo message
    - `collateral` - Satoshis to fund your side of the DLC
    - `feerate` - Fee rate for both funding and closing transactions, in sats/vbytes
    - `refundlocktime` - Locktime of the refund transaction
    - `cetlocktime <value>` - Should not be set unless you know what you are doing. Locktime of the contract execution transactions (defaults to current height)
    - `peer` - Peer's network address
 - `acceptdlc` `offer` `peer` - Accepts a DLC offer given from another party
    - `offer` - Hex encoded dlc offer message
    - `peer` - Peer's network address
 - `acceptdlcoffer` `offer` - Accepts a DLC offer given from another party
    - `offer` - Hex encoded offer message
 - `peer` - Peer's network address
 - `acceptdlcofferfromfile` `path` `[destination]` - Accepts a DLC offer given from another party
   - `path` - Path to dlc offer file
   - `destination` - Path to write dlc accept message
 - `contact-add` `alias` `address` `memo`
   - `alias` - alias for the address like a name
   - `address` - the tor address for the peer
   - `memo` - a memo for this contact
 - `contacts-list` - lists all contacts in the wallet
 - `contact-remove` `address`
   - `address` - the tor address for the peer to remove
 - `dlc-contact-add` `dlcid` `address` - Associated a DLC with a peer
 - `dlc-contact-remove` `dlcid` - Removes a DLC-peer association
    - `address` - the tor address for the peer to remove
 - `signdlc` `accept` - Signs a DLC
    - `accept` - Hex encoded dlc accept message
 - `signdlcfromfile` `path` `[destination]` - Signs a DLC
    - `path` - Path to dlc accept file
    - `destination` - Path to write dlc sign message
 - `adddlcsigs` `sigs` - Adds DLC Signatures into the database
    - `sigs` - Hex encoded dlc sign message
 - `adddlcsigsfromfile` `path` - Adds DLC Signatures into the database
    - `path` - Path to dlc sign file
 - `adddlcsigsandbroadcast` `sigs` - Adds DLC Signatures into the database and broadcasts the funding transaction
    - `sigs` - Hex encoded dlc sign message
 - `adddlcsigsandbroadcastfromfile` `path` - Adds DLC Signatures into the database and broadcasts the funding transaction
    - `path` - Path to dlc sign file
 - `getdlcfundingtx` `contractId` - Returns the Funding Tx corresponding to the DLC with the given contractId
    - `contractId` - ContractId of the DLC
 - `broadcastdlcfundingtx` `contractId` - Broadcasts the funding Tx corresponding to the DLC with the given contractId
    - `contractId` - ContractId of the DLC
 - `executedlc` `contractId` `oraclesigs` `[options]` - Executes the DLC with the given contractId
    - `contractId` - ContractId of the DLC
    - `oraclesigs` - Array of oracle attestations
    - `--noBroadcast` - Gives full serialized transaction instead of broadcasting
 - `executedlcrefund` `contractId` `[options]` - Executes the Refund transaction for the given DLC
    - `contractId` - ContractId of the DLC
    - `--noBroadcast` - Gives full serialized transaction instead of broadcasting
 - `canceldlc` `dlcId` - Cancels a DLC and unreserves used utxos
    - `dlcId` - Internal id of the DLC
 - `getdlcs` - Returns all dlcs in the wallet
    - `address` - optional contact address, if specified the RPC returns only DLCs associated with the given address
 - `getdlc` `dlcId` - Gets a specific dlc in the wallet
    - `dlcId` - Internal id of the DLC
 - `offer-add` `offerTLV` `peerAddress` `message` - Puts an incoming offer into the inbox
    - `offerTLV` - Offer TLV
    - `peer` - Peer URI (optional)
    - `message` - Peer's message or note (optional)
 - `"offer-remove` `hash` - Remove an incoming offer from inbox
    - `hash` - Hash of the offer TLV
 - `offer-send` `offerOrTempContractId` `peerAddress` `message` - Sends an offer to a peer. `offerOrTempContractId` is either an offer TLV or a temporary contract ID.
 - `offers-list` - List all incoming offers from the inbox
 - `getdlcoffer` `tempContractId` - Gets a DLC offer by temporary contract ID.
 - `getaddresslabel` `address` - gets all labels for an address
 - `getaddresslabels` - returns all addresses with labels in the wallet
 - `dropaddresslabel` `address` `label` - drops the label for a given address
 - `dropaddresslabels` `address` - drops all labels for the given address

### Network
 - `getconnectioncount` - Get the number of connected peers
 - `stop` - Request a graceful shutdown of Bitcoin-S
 - `sendrawtransaction` `tx` `Broadcasts the raw transaction`
    - `tx` - Transaction serialized in hex

### PSBT
 - `decodepsbt` `psbt` - Return a JSON object representing the serialized, base64-encoded partially signed Bitcoin transaction.
    - `psbt` - PSBT serialized in hex or base64 format
 - `combinepsbts` `psbts` - Combines all the given PSBTs
    - `psbts` - PSBTs serialized in hex or base64 format
 - `joinpsbts` `psbts` - Combines all the given PSBTs
    - `psbts` - PSBTs serialized in hex or base64 format
 - `finalizepsbt` `psbt` - Finalizes the given PSBT if it can
    - `psbt` - PSBT serialized in hex or base64 format
 - `extractfrompsbt` `psbt` - Extracts a transaction from the given PSBT if it can
    - `psbt` - PSBT serialized in hex or base64 format
 - `converttopsbt` `unsignedTx` - Creates an empty psbt from the given transaction
    - `unsignedTx` - serialized unsigned transaction in hex

### Util
 - `createmultisig` `nrequired` `keys` `[address_type]` - Creates a multi-signature address with n signature of m keys required.
    - `nrequired` - The number of required signatures out of the n keys.
    - `keys` - The hex-encoded public keys.
    - `address_type` -The address type to use. Options are "legacy", "p2sh-segwit", and "bech32"
 - `estimatefee` - Returns the recommended fee rate using the fee provider

## getinfo

```bash
curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "getinfo", "params": []}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":{"network":"main","blockHeight":750129,"blockHash":"00000000000000000002f7d39ca6c914329a8d87010215ef6466c17ffaba0a1f","torStarted":true,"syncing":false,"isinitialblockdownload":false},"error":null}
```

## Sign PSBT with Wallet Example

Bitcoin-S CLI:

```bash
bitcoin-s-cli signpsbt cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA
cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA
```

CURL:
```bash
curl --data-binary '{"jsonrpc": "1.0", "id": "curltest", "method": "signpsbt", "params": ["cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA"]}' -H "Content-Type: application/json" http://127.0.0.1:9999/
{"result":"cHNidP8BAP0FAQIAAAABWUWxYiPKgdGfXcIxJ6MRDxEpUecw59Gk4NpROI5oukoBAAAAAAAAAAAEPttkvdwAAAAXqRSOVAp6Qe/u2hq74e/ThB8foBKn7IfZYMgGCAAAAADbmaQ2nwAAAEdRIQLpfVqyaL9Jb/IkveatNyVeONE8Q/6TzXAWosxLo9e21SECc5G3XiK7xKLlkBG7prMx7p0fMeQwMH5e9H10mBon39JSrtgtgjjLAQAAUGMhAn2YaZnv25I6d6vbb1kw6Xp5IToDrEzl/0VBIW21gHrTZwXg5jGdALJ1IQKyNpDNiOiN6lWpYethib04+XC9bpFXrdpec+xO3U5IM2is9ckf5AABAD0CAAAAAALuiOL0rRcAABYAFPnpLByQq1Gg3vwiP6qR8FmOOjwxvVllM08DAAALBfXJH+QAsXUAAK4AAAAAAQcBAAAAAAAA","error":null}
```

## Websocket endpoints

Bitcoin-s offers websocket endpoints. By default, the endpoint is `ws://localhost:1999/events`

You can configure where how the endpoints are configured inside your `bitcoin-s.conf` 

```
bitcoin-s.server.wsbind=localhost
bitcoin-s.server.wsport=19999
```

These events are implemented using our [internal callback mechanism](../wallet/wallet-callbacks.md#wallet-callbacks).

An example event that is defined is our `blockprocess` event.
Everytime our wallet processes a block, our wallet will notify you via websockets.
Here is an example payload

The current types of events defined are 

1. `txprocessed` - when the wallet processes a transaction. Every transaction in a block will get relayed currently
2. `reservedutxos` - when the wallet reserves OR unreserves utxos
3. `newaddress` - when the wallet generates a new address 
4. `txbroadcast` - when the wallet broadcasts a tx
5. `blockprocessed` - when the wallet processes a block
```
{"type":"blockprocessed","payload":{"raw":"04e0ff2ffce8c3e866e367d305886a3f9d353e557524f61f9cf0c26c46000000000000001206d2e396387bff1c13cbe572d4646abae1ae405f4066ab5e6f5edd6d8f028008a8bb61ffff001af23dd47e","hash":"00000000000000de21f23f6945f028d5ecb47863428f6e9e035ab2fb7a3ef356","confirmations":1,"height":2131641,"version":805298180,"versionHex":"2fffe004","merkleroot":"80028f6ddd5e6f5eab66405f40aee1ba6a64d472e5cb131cff7b3896e3d20612","time":1639688200,"mediantime":1639688200,"nonce":2127838706,"bits":"1a00ffff","difficulty":1.6069135243303364E60,"chainwork":"00000000000000000000000000000000000000000000062438437ddd009e698b","previousblockhash":"00000000000000466cc2f09c1ff62475553e359d3f6a8805d367e366e8c3e8fc","nextblockhash":null}}
```
