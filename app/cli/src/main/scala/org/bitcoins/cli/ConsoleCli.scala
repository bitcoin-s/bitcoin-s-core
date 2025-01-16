package org.bitcoins.cli

import org.bitcoins.cli.CliCommand.*
import org.bitcoins.cli.CliReaders.*
import org.bitcoins.cli.ConsoleCli.RequestParam
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LockUnspentOutputParameter
import org.bitcoins.commons.rpc.*
import org.bitcoins.commons.serializers.Picklers.*
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.*
import org.bitcoins.core.currency.*
import org.bitcoins.core.hd.{AddressType, HDPurpose}
import org.bitcoins.core.hd.AddressType.SegWit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.*
import org.bitcoins.core.protocol.transaction.{
  EmptyTransaction,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.AddressLabelTag
import org.bitcoins.crypto.*
import scodec.bits.ByteVector
import scopt.OParser
import sttp.client3.logging.LogLevel
import sttp.client3.logging.slf4j.Slf4jLoggingBackend
import sttp.client3.{Identity, SttpBackend}
import sttp.model.StatusCode
import ujson.*
import upickle.default as up

import java.io.File
import java.net.InetSocketAddress
import java.nio.file.Path
import java.time.Instant
import java.util.Date
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object ConsoleCli extends BitcoinSLogger {

  def parser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]

    import builder._
    OParser.sequence(
      programName("bitcoin-s-cli"),
      opt[NetworkParameters]('n', "network")
        .action((np, conf) => conf.copy(network = Some(np)))
        .text("Select the active network."),
      opt[Int]("rpcport")
        .action((port, conf) => conf.copy(rpcPortOpt = Some(port)))
        .text(s"The port to send our rpc request to on the server"),
      opt[String]("password")
        .action((password, conf) => conf.copy(rpcPassword = password))
        .text(s"The password to send our rpc request to on the server"),
      opt[Unit]("version")
        .action((_, conf) => conf.copy(command = GetVersion))
        .hidden(),
      help('h', "help").text("Display this help message and exit"),
      note(sys.props("line.separator") + "Commands:"),
      note(sys.props("line.separator") + "===Blockchain ==="),
      cmd("getinfo")
        .action((_, conf) => conf.copy(command = GetInfo))
        .text(s"Returns basic info about the current chain"),
      cmd("getblockcount")
        .action((_, conf) => conf.copy(command = GetBlockCount))
        .text(s"Get the block height"),
      cmd("getfiltercount")
        .action((_, conf) => conf.copy(command = GetFilterCount))
        .text(s"Get the number of filters"),
      cmd("getfilterheadercount")
        .action((_, conf) => conf.copy(command = GetFilterHeaderCount))
        .text(s"Get the number of filter headers"),
      cmd("getbestblockhash")
        .action((_, conf) => conf.copy(command = GetBestBlockHash))
        .text(s"Get the best block hash"),
      cmd("getblockheader")
        .action((_, conf) =>
          conf.copy(command = GetBlockHeader(DoubleSha256DigestBE.empty)))
        .text("Returns information about block header <hash>")
        .children(
          arg[DoubleSha256DigestBE]("hash")
            .text("The block hash")
            .required()
            .action((hash, conf) =>
              conf.copy(command = conf.command match {
                case gbh: GetBlockHeader => gbh.copy(hash = hash)
                case other               => other
              }))
        ),
      cmd("getmediantimepast")
        .action((_, conf) => conf.copy(command = GetMedianTimePast))
        .text(s"Get the median time past"),
      cmd("decoderawtransaction")
        .action((_, conf) =>
          conf.copy(command = DecodeRawTransaction(EmptyTransaction)))
        .text(s"Decode the given raw hex transaction")
        .children(
          arg[Transaction]("tx")
            .text("Transaction encoded in hex to decode")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case decode: DecodeRawTransaction =>
                  decode.copy(tx = tx)
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== Wallet ==="),
      cmd("rescan")
        .action((_, conf) =>
          conf.copy(
            command = Rescan(
              batchSize = Option.empty,
              startBlock = Option.empty,
              endBlock = Option.empty,
              force = false,
              ignoreCreationTime = false
            )
          ))
        .text(s"Rescan for wallet UTXOs")
        .children(
          opt[Unit]("force")
            .text("Clears existing wallet records. Warning! Use with caution!")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(force = true)
                case other => other
              })),
          opt[Int]("batch-size")
            .text("Number of filters that can be matched in one batch")
            .optional()
            .action((batchSize, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(batchSize = Option(batchSize))
                case other => other
              })),
          opt[BlockStamp]("start")
            .text("Start height")
            .optional()
            .action((start, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  // Need to ignoreCreationTime so we try to call
                  // rescan with rescanNeutrinoWallet with a block
                  // and a creation time
                  rescan
                    .copy(startBlock = Option(start), ignoreCreationTime = true)
                case other => other
              })),
          opt[BlockStamp]("end")
            .text("End height")
            .optional()
            .action((end, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(endBlock = Option(end))
                case other => other
              })),
          opt[Unit]("ignorecreationtime")
            .text(
              "Ignores the wallet creation date and will instead do a full rescan"
            )
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(ignoreCreationTime = true)
                case other => other
              }))
        ),
      cmd("isempty")
        .action((_, conf) => conf.copy(command = IsEmpty))
        .text("Checks if the wallet contains any data"),
      cmd("walletinfo")
        .action((_, conf) => conf.copy(command = WalletInfo))
        .text("Returns data about the current wallet being used"),
      cmd("listwallets")
        .action((_, conf) => conf.copy(command = ListWallets))
        .text("Returns all the bitcoin-s wallets"),
      cmd("getbalance")
        .action((_, conf) => conf.copy(command = GetBalance(false)))
        .text("Get the wallet balance")
        .children(
          opt[Unit]("sats")
            .optional()
            .text("Display balance in satoshis")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case getBalance: GetBalance =>
                  getBalance.copy(isSats = true)
                case other => other
              }))
        ),
      cmd("getconfirmedbalance")
        .action((_, conf) => conf.copy(command = GetConfirmedBalance(false)))
        .text("Get the wallet balance of confirmed utxos")
        .children(
          opt[Unit]("sats")
            .optional()
            .text("Display balance in satoshis")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case getBalance: GetConfirmedBalance =>
                  getBalance.copy(isSats = true)
                case other => other
              }))
        ),
      cmd("getunconfirmedbalance")
        .action((_, conf) => conf.copy(command = GetUnconfirmedBalance(false)))
        .text("Get the wallet balance of unconfirmed utxos")
        .children(
          opt[Unit]("sats")
            .optional()
            .text("Display balance in satoshis")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case getBalance: GetUnconfirmedBalance =>
                  getBalance.copy(isSats = true)
                case other => other
              }))
        ),
      cmd("getbalances")
        .action((_, conf) => conf.copy(command = GetBalances(false)))
        .text("Get the wallet balance by utxo state")
        .children(
          opt[Unit]("sats")
            .optional()
            .text("Display balance in satoshis")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case getBalances: GetBalances =>
                  getBalances.copy(isSats = true)
                case other => other
              }))
        ),
      cmd("getutxos")
        .action((_, conf) => conf.copy(command = GetUtxos))
        .text("Returns list of all wallet utxos"),
      cmd("getreservedutxos")
        .action((_, conf) => conf.copy(command = GetReservedUtxos))
        .text("Returns list of all reserved wallet utxos"),
      cmd("getaddresses")
        .action((_, conf) => conf.copy(command = GetAddresses))
        .text("Returns list of all wallet addresses currently being watched"),
      cmd("getspentaddresses")
        .action((_, conf) => conf.copy(command = GetSpentAddresses))
        .text(
          "Returns list of all wallet addresses that have received funds and been spent"
        ),
      cmd("getfundedaddresses")
        .action((_, conf) => conf.copy(command = GetFundedAddresses))
        .text("Returns list of all wallet addresses that are holding funds"),
      cmd("getunusedaddresses")
        .action((_, conf) => conf.copy(command = GetUnusedAddresses))
        .text("Returns list of all wallet addresses that have not been used"),
      cmd("getaccounts")
        .action((_, conf) => conf.copy(command = GetAccounts))
        .text("Returns list of all wallet accounts"),
      cmd("createnewaccount")
        .action((_, conf) => conf.copy(command = CreateNewAccount(null)))
        .text("Creates a new wallet account")
        .children(
          arg[String]("hd_purpose")
            .text("hd_purpose according to BIP43")
            .required()
            .action((purpose, conf) =>
              conf.copy(command = conf.command match {
                case c: CreateNewAccount =>
                  c.copy(purpose = HDPurpose.fromString(purpose))
                case other => other
              }))
        ),
      cmd("getaddressinfo")
        .action((_, conf) => conf.copy(command = GetAddressInfo(null)))
        .text("Returns list of all wallet accounts")
        .children(
          arg[BitcoinAddress]("address")
            .text("Address to get information about")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case getAddressInfo: GetAddressInfo =>
                  getAddressInfo.copy(address = addr)
                case other => other
              }))
        ),
      cmd("getnewaddress")
        .action((_, conf) => conf.copy(command = GetNewAddress(None)))
        .text("Get a new address")
        .children(
          opt[AddressLabelTag]("label")
            .text("The label name for the address to be linked to")
            .action((label, conf) =>
              conf.copy(command = conf.command match {
                case getNewAddress: GetNewAddress =>
                  getNewAddress.copy(labelOpt = Some(label))
                case other => other
              }))
        ),
      cmd("labeladdress")
        .action((_, conf) =>
          conf.copy(command = LabelAddress(null, AddressLabelTag(""))))
        .text("Add a label to the wallet address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to label")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case labelAddress: LabelAddress =>
                  labelAddress.copy(address = addr)
                case other => other
              })),
          arg[AddressLabelTag]("label")
            .text("The label name for the address to be linked to")
            .required()
            .action((label, conf) =>
              conf.copy(command = conf.command match {
                case labelAddress: LabelAddress =>
                  labelAddress.copy(label = label)
                case other => other
              }))
        ),
      cmd("getaddresstags")
        .action((_, conf) => conf.copy(command = GetAddressTags(null)))
        .text("Get all the tags associated with this address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to get with the associated tags")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case getAddressTags: GetAddressTags =>
                  getAddressTags.copy(address = addr)
                case other => other
              }))
        ),
      cmd("getaddresslabel")
        .action((_, conf) => conf.copy(command = GetAddressLabel(null)))
        .text("Get all the labels associated with this address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to get with the associated labels")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case getAddressLabels: GetAddressLabel =>
                  getAddressLabels.copy(address = addr)
                case other => other
              }))
        ),
      cmd("getaddresslabels")
        .action((_, conf) => conf.copy(command = GetAddressLabels))
        .text("Returns all labels in wallet"),
      cmd("dropaddresslabels")
        .action((_, conf) => conf.copy(command = DropAddressLabels(null)))
        .text("Drop the label associated with the address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to drop the associated labels of")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case dropAddressLabels: DropAddressLabels =>
                  dropAddressLabels.copy(address = addr)
                case other => other
              }))
        ),
      cmd("dropaddresslabel")
        .action((_, conf) => conf.copy(command = DropAddressLabel(null, null)))
        .text("Drop all the labels associated with this address")
        .children(
          arg[BitcoinAddress]("address")
            .text("The address to drop the associated labels of")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case dropAddressLabel: DropAddressLabel =>
                  dropAddressLabel.copy(address = addr)
                case other => other
              })),
          arg[String]("label")
            .text("The label to drop")
            .required()
            .action((label, conf) =>
              conf.copy(command = conf.command match {
                case dropAddressLabel: DropAddressLabel =>
                  dropAddressLabel.copy(label = label)
                case other => other
              }))
        ),
      cmd("sendtoaddress")
        .action(
          // TODO how to handle null here?
          (_, conf) =>
            conf.copy(command =
              SendToAddress(null, 0.bitcoin, None, noBroadcast = false)))
        .text("Send money to the given address")
        .children(
          arg[BitcoinAddress]("address")
            .text("Address to send to")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(destination = addr)
                case other => other
              })),
          arg[Bitcoins]("amount")
            .text("amount to send in BTC")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(amount = btc)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(satoshisPerVirtualByte = Some(feeRate))
                case other => other
              })),
          opt[Unit]("noBroadcast")
            .text("Gives full serialized transaction instead of broadcasting")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("sendfromoutpoints")
        .action((_, conf) =>
          conf.copy(
            command = SendFromOutPoints(Vector.empty, null, 0.bitcoin, None)
          ))
        .text("Send money to the given address")
        .children(
          arg[Seq[TransactionOutPoint]]("outpoints")
            .text("Out Points to send from")
            .required()
            .action((outPoints, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(outPoints = outPoints.toVector)
                case other => other
              })),
          arg[BitcoinAddress]("address")
            .text("Address to send to")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(destination = addr)
                case other => other
              })),
          arg[Bitcoins]("amount")
            .text("amount to send in BTC")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(amount = btc)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case send: SendFromOutPoints =>
                  send.copy(feeRateOpt = Some(feeRate))
                case other => other
              }))
        ),
      cmd("sendwithalgo")
        .action((_, conf) =>
          conf.copy(command = SendWithAlgo(null, 0.bitcoin, None, null)))
        .text(
          "Send money to the given address using a specific coin selection algo"
        )
        .children(
          arg[BitcoinAddress]("address")
            .text("Address to send to")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(destination = addr)
                case other => other
              })),
          arg[Bitcoins]("amount")
            .text("amount to send in BTC")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(amount = btc)
                case other => other
              })),
          arg[CoinSelectionAlgo]("algo")
            .text("Coin selection algo")
            .optional()
            .action((algo, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(algo = algo)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case send: SendWithAlgo =>
                  send.copy(feeRateOpt = Some(feeRate))
                case other => other
              }))
        ),
      cmd("sweepwallet")
        .action((_, conf) => conf.copy(command = SweepWallet(null, None)))
        .text("Sends the entire wallet balance to the given address")
        .children(
          arg[BitcoinAddress]("address")
            .text("Address to send to")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case sweep: SweepWallet =>
                  sweep.copy(destination = addr)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case sweep: SweepWallet =>
                  sweep.copy(feeRateOpt = Some(feeRate))
                case other => other
              }))
        ),
      cmd("signpsbt")
        .action((_, conf) => conf.copy(command = SignPSBT(PSBT.empty)))
        .text(
          "Signs the PSBT's inputs with keys that are associated with the wallet"
        )
        .children(
          arg[PSBT]("psbt")
            .text("PSBT to sign")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case signPSBT: SignPSBT =>
                  signPSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("opreturncommit")
        .action((_, conf) =>
          conf.copy(command = OpReturnCommit("", hashMessage = false, None)))
        .text("Creates OP_RETURN commitment transaction")
        .children(
          arg[String]("message")
            .text("message to put into OP_RETURN commitment")
            .required()
            .action((message, conf) =>
              conf.copy(command = conf.command match {
                case opReturnCommit: OpReturnCommit =>
                  opReturnCommit.copy(message = message)
                case other => other
              })),
          opt[Unit]("hashMessage")
            .text("should the message be hashed before commitment")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case opReturnCommit: OpReturnCommit =>
                  opReturnCommit.copy(hashMessage = true)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case opReturnCommit: OpReturnCommit =>
                  opReturnCommit.copy(feeRateOpt = Some(feeRate))
                case other => other
              }))
        ),
      cmd("bumpfeecpfp")
        .action((_, conf) =>
          conf.copy(command = BumpFeeCPFP(DoubleSha256DigestBE.empty,
                                          SatoshisPerVirtualByte.zero)))
        .text(
          "Bump the fee of the given transaction id with a child tx using the given fee rate"
        )
        .children(
          arg[DoubleSha256DigestBE]("txid")
            .text("Id of transaction to bump fee")
            .required()
            .action((txid, conf) =>
              conf.copy(command = conf.command match {
                case cpfp: BumpFeeCPFP =>
                  cpfp.copy(txId = txid)
                case other => other
              })),
          arg[SatoshisPerVirtualByte]("feerate")
            .text("Fee rate in sats per virtual byte of the child transaction")
            .required()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case cpfp: BumpFeeCPFP =>
                  cpfp.copy(feeRate = feeRate)
                case other => other
              }))
        ),
      cmd("bumpfeerbf")
        .action((_, conf) =>
          conf.copy(command = BumpFeeRBF(DoubleSha256DigestBE.empty,
                                         SatoshisPerVirtualByte.zero)))
        .text("Replace given transaction with one with the new fee rate")
        .children(
          arg[DoubleSha256DigestBE]("txid")
            .text("Id of transaction to bump fee")
            .required()
            .action((txid, conf) =>
              conf.copy(command = conf.command match {
                case rbf: BumpFeeRBF =>
                  rbf.copy(txId = txid)
                case other => other
              })),
          arg[SatoshisPerVirtualByte]("feerate")
            .text("New fee rate in sats per virtual byte")
            .required()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case rbf: BumpFeeRBF =>
                  rbf.copy(feeRate = feeRate)
                case other => other
              }))
        ),
      cmd("gettransaction")
        .action((_, conf) =>
          conf.copy(command = GetTransaction(DoubleSha256DigestBE.empty)))
        .text("Get detailed information about in-wallet transaction <txid>")
        .children(
          arg[DoubleSha256DigestBE]("txid")
            .text("The transaction id")
            .required()
            .action((txid, conf) =>
              conf.copy(command = conf.command match {
                case getTx: GetTransaction =>
                  getTx.copy(txId = txid)
                case other => other
              }))
        ),
      cmd("lockunspent")
        .action((_, conf) =>
          conf.copy(command = LockUnspent(unlock = false, Vector.empty)))
        .text(
          "Temporarily lock (unlock=false) or unlock (unlock=true) specified transaction outputs." +
            "\nIf no transaction outputs are specified when unlocking then all current locked transaction outputs are unlocked."
        )
        .children(
          arg[Boolean]("unlock")
            .text(
              "Whether to unlock (true) or lock (false) the specified transactions"
            )
            .required()
            .action((unlock, conf) =>
              conf.copy(command = conf.command match {
                case lockUnspent: LockUnspent =>
                  lockUnspent.copy(unlock = unlock)
                case other => other
              })),
          arg[Vector[LockUnspentOutputParameter]]("transactions")
            .text(
              "The transaction outpoints to unlock/lock, empty to apply to all utxos"
            )
            .optional()
            .action((outputParam, conf) =>
              conf.copy(command = conf.command match {
                case lockUnspent: LockUnspent =>
                  lockUnspent.copy(outputParam = outputParam)
                case other => other
              }))
        ),
      cmd("importseed")
        .action((_, conf) => conf.copy(command = ImportSeed(None, null, None)))
        .text("Imports a mnemonic seed as a new seed file")
        .children(
          opt[String]("walletname")
            .text("Name to associate with this seed")
            .optional()
            .action((walletName, conf) =>
              conf.copy(command = conf.command match {
                case is: ImportSeed =>
                  is.copy(walletNameOpt = Some(walletName))
                case other => other
              })),
          opt[MnemonicCode]("words")
            .text("Mnemonic seed words, space separated")
            .required()
            .action((mnemonic, conf) =>
              conf.copy(command = conf.command match {
                case is: ImportSeed =>
                  is.copy(mnemonic = mnemonic)
                case other => other
              })),
          opt[AesPassword]("passphrase")
            .text("Passphrase to encrypt the seed with")
            .optional()
            .action((password, conf) =>
              conf.copy(command = conf.command match {
                case is: ImportSeed =>
                  is.copy(passwordOpt = Some(password))
                case other => other
              }))
        ),
      cmd("exportseed")
        .action((_, conf) => conf.copy(command = ExportSeed(None, None)))
        .text("Exports the wallet's mnemonic seed")
        .children(
          opt[String]("walletname")
            .text("Name to associate with this seed (optional)")
            .optional()
            .action((walletName, conf) =>
              conf.copy(command = conf.command match {
                case es: ExportSeed =>
                  es.copy(walletNameOpt = Some(walletName))
                case other => other
              })),
          opt[AesPassword]("passphrase")
            .text("Passphrase to encrypt the seed with")
            .optional()
            .action((password, conf) =>
              conf.copy(command = conf.command match {
                case es: ExportSeed =>
                  es.copy(passwordOpt = Some(password))
                case other => other
              }))
        ),
      cmd("markseedasbackedup")
        .action((_, conf) =>
          conf.copy(command = MarkSeedAsBackedUp(None, None)))
        .text("Marks the seed as backed up")
        .children(
          opt[String]("walletname")
            .text("Name to associate with this seed (optional)")
            .optional()
            .action((walletName, conf) =>
              conf.copy(command = conf.command match {
                case ms: MarkSeedAsBackedUp =>
                  ms.copy(walletNameOpt = Some(walletName))
                case other => other
              })),
          opt[AesPassword]("passphrase")
            .text("Passphrase to encrypt the seed with")
            .optional()
            .action((password, conf) =>
              conf.copy(command = conf.command match {
                case ms: MarkSeedAsBackedUp =>
                  ms.copy(passwordOpt = Some(password))
                case other => other
              }))
        ),
      cmd("getseedbackuptime")
        .action((_, conf) => conf.copy(command = GetSeedBackupTime(None, None)))
        .text("Returns the wallet's mnemonic seed backup time")
        .children(
          opt[String]("walletname")
            .text("Name to associate with this seed (optional)")
            .optional()
            .action((walletName, conf) =>
              conf.copy(command = conf.command match {
                case gs: GetSeedBackupTime =>
                  gs.copy(walletNameOpt = Some(walletName))
                case other => other
              })),
          opt[AesPassword]("passphrase")
            .text("Passphrase to encrypt the seed with")
            .optional()
            .action((password, conf) =>
              conf.copy(command = conf.command match {
                case gs: GetSeedBackupTime =>
                  gs.copy(passwordOpt = Some(password))
                case other => other
              }))
        ),
      cmd("importxprv")
        .action((_, conf) => conf.copy(command = ImportXprv(None, null, None)))
        .text("Imports a xprv as a new seed file")
        .children(
          opt[String]("walletname")
            .text("What name to associate with this seed")
            .optional()
            .action((walletName, conf) =>
              conf.copy(command = conf.command match {
                case ix: ImportXprv =>
                  ix.copy(walletNameOpt = Some(walletName))
                case other => other
              })),
          opt[ExtPrivateKey]("xprv")
            .text("base58 encoded extended private key")
            .required()
            .action((xprv, conf) =>
              conf.copy(command = conf.command match {
                case ix: ImportXprv =>
                  ix.copy(xprv = xprv)
                case other => other
              })),
          opt[AesPassword]("passphrase")
            .text("Passphrase to encrypt this seed with")
            .optional()
            .action((password, conf) =>
              conf.copy(command = conf.command match {
                case ix: ImportXprv =>
                  ix.copy(passwordOpt = Some(password))
                case other => other
              }))
        ),
      cmd("keymanagerpassphrasechange")
        .action((_, conf) =>
          conf.copy(command = KeyManagerPassphraseChange(null, null)))
        .text("Changes the wallet passphrase")
        .children(
          arg[AesPassword]("oldpassphrase")
            .text("The current passphrase")
            .required()
            .action((oldPass, conf) =>
              conf.copy(command = conf.command match {
                case wpc: KeyManagerPassphraseChange =>
                  wpc.copy(oldPassword = oldPass)
                case other => other
              })),
          arg[AesPassword]("newpassphrase")
            .text("The new passphrase")
            .required()
            .action((newPass, conf) =>
              conf.copy(command = conf.command match {
                case wpc: KeyManagerPassphraseChange =>
                  wpc.copy(newPassword = newPass)
                case other => other
              }))
        ),
      cmd("keymanagerpassphraseset")
        .action((_, conf) => conf.copy(command = KeyManagerPassphraseSet(null)))
        .text("Encrypts the wallet with the given passphrase")
        .children(
          arg[AesPassword]("passphrase")
            .text("The passphrase to encrypt the wallet with")
            .required()
            .action((pass, conf) =>
              conf.copy(command = conf.command match {
                case wps: KeyManagerPassphraseSet =>
                  wps.copy(password = pass)
                case other => other
              }))
        ),
      cmd("loadwallet")
        .action((_, conf) => conf.copy(command = LoadWallet(None, None, None)))
        .text("Load a wallet")
        .children(
          opt[String]("walletname")
            .text(
              "Wallet's name (the default wallet will be loaded if omitted)"
            )
            .optional()
            .action((walletName, conf) =>
              conf.copy(command = conf.command match {
                case lw: LoadWallet =>
                  lw.copy(walletNameOpt = Some(walletName))
                case other => other
              })),
          opt[AesPassword]("passphrase")
            .text("Passphrase to decrypt the seed with")
            .optional()
            .action((password, conf) =>
              conf.copy(command = conf.command match {
                case lw: LoadWallet =>
                  lw.copy(passwordOpt = Some(password))
                case other => other
              })),
          opt[String]("bip39passphrase")
            .text("BIP39 passphrase")
            .optional()
            .action((bip39Password, conf) =>
              conf.copy(command = conf.command match {
                case lw: LoadWallet =>
                  lw.copy(bip39PasswordOpt = Some(bip39Password))
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== DLC ==="),
      cmd("decodecontractinfo")
        .action((_, conf) => conf.copy(command = DecodeContractInfo(null)))
        .text("Decodes a contract info into json")
        .children(
          arg[ContractInfoV0TLV]("contractinfo")
            .text("Hex encoded contract info")
            .required()
            .action((contractInfo, conf) =>
              conf.copy(command = conf.command match {
                case decode: DecodeContractInfo =>
                  decode.copy(contractInfo = contractInfo)
                case other => other
              }))
        ),
      cmd("decodeoffer")
        .action((_, conf) => conf.copy(command = DecodeOffer(null)))
        .text("Decodes an offer message into json")
        .children(
          arg[DLCOfferTLV]("offer")
            .text("Hex encoded dlc offer message")
            .required()
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case decode: DecodeOffer =>
                  decode.copy(offer = offer)
                case other => other
              }))
        ),
      cmd("decodeannouncement")
        .action((_, conf) => conf.copy(command = DecodeAnnouncement(null)))
        .text("Decodes an oracle announcement message into json")
        .children(
          arg[OracleAnnouncementV0TLV]("announcement")
            .text("Hex encoded oracle announcement message")
            .required()
            .action((ann, conf) =>
              conf.copy(command = conf.command match {
                case decode: DecodeAnnouncement =>
                  decode.copy(announcement = ann)
                case other => other
              }))
        ),
      cmd("decodeattestments")
        .action((_, conf) => conf.copy(command = DecodeAttestments(null)))
        .text("Decodes an oracle attestments message into json")
        .children(
          arg[OracleAttestmentV0TLV]("attestments")
            .text("Hex encoded oracle attestments message")
            .required()
            .action((attestments, conf) =>
              conf.copy(command = conf.command match {
                case decode: DecodeAttestments =>
                  decode.copy(sigs = attestments)
                case other => other
              }))
        ),
      cmd("getdlchostaddress")
        .action((_, conf) => conf.copy(command = GetDLCHostAddress))
        .text("Returns the public listening address of the DLC Node"),
      cmd("createdlcoffer")
        .action((_, conf) =>
          conf.copy(
            command = CreateDLCOffer(
              ContractInfoV0TLV.dummy,
              Satoshis.zero,
              None,
              None,
              UInt32.zero,
              None,
              None,
              None
            )
          ))
        .text("Creates a DLC offer that another party can accept")
        .children(
          arg[ContractInfoV0TLV]("contractInfo")
            .text("Hex encoded contractInfo message")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(contractInfoTLV = info)
                case other => other
              })),
          arg[Satoshis]("collateral")
            .text("Satoshis to fund your side of the DLC")
            .required()
            .action((collateral, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(collateral = collateral)
                case other => other
              })),
          arg[SatoshisPerVirtualByte]("feerate")
            .text(
              "Fee rate for both funding and closing transactions, in sats/vbytes"
            )
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(feeRateOpt = Some(feeRate))
                case other => other
              })),
          arg[UInt32]("refundlocktime")
            .text("Locktime of the refund transaction")
            .required()
            .action((refundLocktime, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(refundLocktime = refundLocktime)
                case other => other
              })),
          opt[UInt32]("cetlocktime")
            .text("Locktime of the contract execution transactions")
            .optional()
            .action((locktime, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(locktimeOpt = Some(locktime))
                case other => other
              }))
        ),
      cmd("acceptdlc")
        .action((_, conf) =>
          conf.copy(command = AcceptDLC(
            null,
            InetSocketAddress.createUnresolved("localhost", 0),
            None,
            None
          )))
        .text("Accepts a DLC offer given from another party")
        .children(
          arg[LnMessage[DLCOfferTLV]]("offer")
            .text("Hex encoded dlc offer message")
            .required()
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLC =>
                  accept.copy(offer = offer)
                case other => other
              })),
          arg[InetSocketAddress]("peer")
            .text("Peer's network address")
            .required()
            .action((peer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLC =>
                  accept.copy(peerAddr = peer)
                case other => other
              }))
        ),
      cmd("acceptdlcoffer")
        .action((_, conf) =>
          conf.copy(command = AcceptDLCOffer(null, None, None, None)))
        .text("Accepts a DLC offer given from another party")
        .children(
          arg[LnMessage[DLCOfferTLV]]("offer")
            .text("Hex encoded dlc offer message")
            .required()
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(offer = offer)
                case other => other
              }))
        ),
      cmd("acceptdlcofferfromfile")
        .action((_, conf) =>
          conf.copy(command =
            AcceptDLCOfferFromFile(new File("").toPath, None)))
        .text("Accepts a DLC offer given from another party")
        .children(
          arg[Path]("path")
            .text("Path to dlc offer file")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOfferFromFile =>
                  accept.copy(path = path)
                case other => other
              })),
          arg[Path]("destination")
            .text("Path to write dlc accept message")
            .optional()
            .action((dest, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOfferFromFile =>
                  accept.copy(destination = Some(dest))
                case other => other
              }))
        ),
      cmd("signdlc")
        .action((_, conf) => conf.copy(command = SignDLC(null)))
        .text("Signs a DLC")
        .children(
          arg[LnMessage[DLCAcceptTLV]]("accept")
            .text("Hex encoded dlc accept message")
            .required()
            .action((accept, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(accept = accept)
                case other => other
              }))
        ),
      cmd("signdlcfromfile")
        .action((_, conf) =>
          conf.copy(command = SignDLCFromFile(new File("").toPath, None)))
        .text("Signs a DLC")
        .children(
          arg[Path]("path")
            .text("Path to dlc accept file")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLCFromFile =>
                  signDLC.copy(path = path)
                case other => other
              })),
          arg[Path]("destination")
            .text("Path to write dlc sign message")
            .optional()
            .action((dest, conf) =>
              conf.copy(command = conf.command match {
                case accept: SignDLCFromFile =>
                  accept.copy(destination = Some(dest))
                case other => other
              }))
        ),
      cmd("adddlcsigs")
        .action((_, conf) => conf.copy(command = AddDLCSigs(null)))
        .text("Adds DLC Signatures into the database")
        .children(
          arg[LnMessage[DLCSignTLV]]("sigs")
            .text("Hex encoded dlc sign message")
            .required()
            .action((sigs, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigs =>
                  addDLCSigs.copy(sigs = sigs)
                case other => other
              }))
        ),
      cmd("adddlcsigsfromfile")
        .action((_, conf) =>
          conf.copy(command = AddDLCSigsFromFile(new File("").toPath)))
        .text("Adds DLC Signatures into the database")
        .children(
          arg[Path]("path")
            .text("Path to dlc sign file")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigsFromFile =>
                  addDLCSigs.copy(path = path)
                case other => other
              }))
        ),
      cmd("adddlcsigsandbroadcast")
        .action((_, conf) => conf.copy(command = AddDLCSigsAndBroadcast(null)))
        .text(
          "Adds DLC Signatures into the database and broadcasts the funding transaction"
        )
        .children(
          arg[LnMessage[DLCSignTLV]]("sigs")
            .text("Hex encoded dlc sign message")
            .required()
            .action((sigs, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigsAndBroadcast =>
                  addDLCSigs.copy(sigs = sigs)
                case other => other
              }))
        ),
      cmd("adddlcsigsandbroadcastfromfile")
        .action((_, conf) =>
          conf.copy(command =
            AddDLCSigsAndBroadcastFromFile(new File("").toPath)))
        .text(
          "Adds DLC Signatures into the database and broadcasts the funding transaction"
        )
        .children(
          arg[Path]("path")
            .text("Path to dlc sign file")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigsAndBroadcastFromFile =>
                  addDLCSigs.copy(path = path)
                case other => other
              }))
        ),
      cmd("getdlcfundingtx")
        .action((_, conf) => conf.copy(command = GetDLCFundingTx(null)))
        .text(
          "Returns the Funding Tx corresponding to the DLC with the given contractId"
        )
        .children(
          arg[ByteVector]("contractId")
            .text("ContractId of the DLC")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case getDLCFundingTx: GetDLCFundingTx =>
                  getDLCFundingTx.copy(contractId = contractId)
                case other => other
              }))
        ),
      cmd("broadcastdlcfundingtx")
        .action((_, conf) => conf.copy(command = BroadcastDLCFundingTx(null)))
        .text(
          "Broadcasts the funding Tx corresponding to the DLC with the given contractId"
        )
        .children(
          arg[ByteVector]("contractId")
            .text("ContractId of the DLC")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case broadcastDLCFundingTx: BroadcastDLCFundingTx =>
                  broadcastDLCFundingTx.copy(contractId = contractId)
                case other => other
              }))
        ),
      cmd("executedlc")
        .action((_, conf) =>
          conf.copy(command =
            ExecuteDLC(ByteVector.empty, Vector.empty, noBroadcast = false)))
        .text("Executes the DLC with the given contractId")
        .children(
          arg[ByteVector]("contractId")
            .text("ContractId of the DLC")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLC: ExecuteDLC =>
                  executeDLC.copy(contractId = contractId)
                case other => other
              })),
          arg[Seq[OracleAttestmentTLV]]("oraclesigs")
            .text("Array of oracle attestations")
            .required()
            .action((sigs, conf) =>
              conf.copy(command = conf.command match {
                case executeDLC: ExecuteDLC =>
                  executeDLC.copy(oracleSigs = sigs.toVector)
                case other => other
              })),
          opt[Unit]("noBroadcast")
            .text("Gives full serialized transaction instead of broadcasting")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLC: ExecuteDLC =>
                  executeDLC.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("executedlcrefund")
        .action((_, conf) =>
          conf.copy(command = ExecuteDLCRefund(null, noBroadcast = false)))
        .text("Executes the Refund transaction for the given DLC")
        .children(
          arg[ByteVector]("contractId")
            .text("ContractId of the DLC")
            .required()
            .action((contractId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRefund: ExecuteDLCRefund =>
                  executeDLCRefund.copy(contractId = contractId)
                case other => other
              })),
          opt[Unit]("noBroadcast")
            .text("Gives full serialized transaction instead of broadcasting")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRefund: ExecuteDLCRefund =>
                  executeDLCRefund.copy(noBroadcast = true)
                case other => other
              }))
        ),
      cmd("canceldlc")
        .action((_, conf) => conf.copy(command = CancelDLC(Sha256Digest.empty)))
        .text("Cancels a DLC and unreserves used utxos")
        .children(
          arg[Sha256Digest]("dlcId")
            .text("Internal id of the DLC")
            .required()
            .action((dlcId, conf) =>
              conf.copy(command = conf.command match {
                case cancelDLC: CancelDLC =>
                  cancelDLC.copy(dlcId = dlcId)
                case other => other
              }))
        ),
      cmd("getdlcs")
        .action((_, conf) => conf.copy(command = GetDLCs))
        .text("Returns all dlcs in the wallet"),
      cmd("getdlc")
        .action((_, conf) => conf.copy(command = GetDLC(Sha256Digest.empty)))
        .text("Gets a specific dlc in the wallet")
        .children(
          arg[Sha256Digest]("dlcId")
            .text("Internal id of the DLC")
            .required()
            .action((dlcId, conf) =>
              conf.copy(command = conf.command match {
                case _: GetDLC => GetDLC(dlcId)
                case other     => other
              }))
        ),
      cmd("contact-add")
        .action((_, conf) => conf.copy(command = ContactAdd.empty))
        .text("Add a contact to your DLC wallet")
        .children(
          arg[String]("alias")
            .text("Alias for the contact")
            .required()
            .action((alias, conf) =>
              conf.copy(command = conf.command match {
                case contactAdd: ContactAdd =>
                  contactAdd.copy(alias = alias)
                case other => other
              })),
          arg[InetSocketAddress]("address")
            .text("Peer's network address")
            .required()
            .action((peer, conf) =>
              conf.copy(command = conf.command match {
                case contactAdd: ContactAdd =>
                  contactAdd.copy(address = peer)
                case other => other
              })),
          arg[String]("memo")
            .text("A memo")
            .required()
            .action((memo, conf) =>
              conf.copy(command = conf.command match {
                case contactAdd: ContactAdd =>
                  contactAdd.copy(memo = memo)
                case other => other
              }))
        ),
      cmd("contacts-list")
        .action((_, conf) => conf.copy(command = ContactsList))
        .text("Returns all contacts in the wallet"),
      cmd("contact-remove")
        .action((_, conf) => conf.copy(command = ContactRemove(null)))
        .text("Remove a contact from the wallet")
        .children(
          arg[InetSocketAddress]("address")
            .text(
              "The address of the contact we want to remove from the wallet"
            )
            .required()
            .action((address, conf) =>
              conf.copy(command = conf.command match {
                case remove: ContactRemove =>
                  remove.copy(address = address)
                case other => other
              }))
        ),
      cmd("createcontractinfo")
        .action((_, conf) => conf.copy(command = CreateContractInfo.empty))
        .text(
          "Create a contract info from an announcement, total collateral, and contract descriptor"
        )
        .children(
          arg[OracleAnnouncementTLV]("announcement")
            .text("The announcement we are creating a contract info for")
            .required()
            .action((ann, conf) =>
              conf.copy(command = conf.command match {
                case createContractInfo: CreateContractInfo =>
                  createContractInfo.copy(announcementTLV = ann)
                case other => other
              })),
          arg[Satoshis]("totalCollateral")
            .text(
              "The total collateral in the DLC. This is your collateral + counterparty collateral"
            )
            .required()
            .action((totalCollateral, conf) =>
              conf.copy(command = conf.command match {
                case create: CreateContractInfo =>
                  create.copy(totalCollateral = totalCollateral)
                case other => other
              })),
          arg[ContractDescriptorTLV]("contractDescriptor")
            .text(
              "The contract descriptor in the DLC. This is expected to be of format [[outcome1, payout1], [outcome2, payout2], ...]"
            )
            .required()
            .action((contractDescriptor, conf) =>
              conf.copy(command = conf.command match {
                case create: CreateContractInfo =>
                  create.copy(contractDescriptor = contractDescriptor)
                case other => other
              }))
        ),
      cmd("addoffer")
        .action((_, conf) => conf.copy(command = AddDLCOffer(null, null, null)))
        .text("Adds a DLC offer into the inbox")
        .children(
          arg[LnMessage[DLCOfferTLV]]("offer")
            .text("Hex encoded dlc offer message")
            .required()
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(offer = offer)
                case other => other
              })),
          arg[String]("message")
            .text("Text message or note")
            .required()
            .action((message, conf) =>
              conf.copy(command = conf.command match {
                case create: AddDLCOffer =>
                  create.copy(message = message)
                case other => other
              })),
          arg[String]("peer")
            .text("Peer URI")
            .required()
            .action((peer, conf) =>
              conf.copy(command = conf.command match {
                case create: AddDLCOffer =>
                  create.copy(peer = peer)
                case other => other
              }))
        ),
      cmd("removeoffer")
        .action((_, conf) => conf.copy(command = RemoveDLCOffer(null)))
        .text("Removes a DLC offer from the inbox")
        .children(
          arg[Sha256Digest]("hash")
            .text("Hex encoded dlc offer hash")
            .required()
            .action((hash, conf) =>
              conf.copy(command = conf.command match {
                case accept: RemoveDLCOffer =>
                  accept.copy(offerHash = hash)
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== Network ==="),
      cmd("getconnectioncount")
        .action((_, conf) => conf.copy(command = GetConnectionCount))
        .text(s"List the connected peers"),
      cmd("stop")
        .action((_, conf) => conf.copy(command = Stop))
        .text("Request a graceful shutdown of Bitcoin-S"),
      cmd("sendrawtransaction")
        .action((_, conf) =>
          conf.copy(command = SendRawTransaction(EmptyTransaction)))
        .text("Broadcasts the raw transaction")
        .children(
          arg[Transaction]("tx")
            .text("Transaction serialized in hex")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case sendRawTransaction: SendRawTransaction =>
                  sendRawTransaction.copy(tx = tx)
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== PSBT ==="),
      cmd("decodepsbt")
        .action((_, conf) => conf.copy(command = DecodePSBT(PSBT.empty)))
        .text(
          "Return a JSON object representing the serialized, base64-encoded partially signed Bitcoin transaction."
        )
        .children(
          arg[PSBT]("psbt")
            .text("PSBT serialized in hex or base64 format")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case decode: DecodePSBT =>
                  decode.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("analyzepsbt")
        .action((_, conf) => conf.copy(command = AnalyzePSBT(PSBT.empty)))
        .text(
          "Analyzes and provides information about the current status of a PSBT and its inputs"
        )
        .children(
          arg[PSBT]("psbt")
            .text("PSBT serialized in hex or base64 format")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case analyzePSBT: AnalyzePSBT =>
                  analyzePSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("combinepsbts")
        .action((_, conf) => conf.copy(command = CombinePSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          arg[Seq[PSBT]]("psbts")
            .text("PSBTs serialized in hex or base64 format")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case combinePSBTs: CombinePSBTs =>
                  combinePSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("joinpsbts")
        .action((_, conf) => conf.copy(command = JoinPSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          arg[Seq[PSBT]]("psbts")
            .text("PSBTs serialized in hex or base64 format")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case joinPSBTs: JoinPSBTs =>
                  joinPSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("finalizepsbt")
        .action((_, conf) => conf.copy(command = FinalizePSBT(PSBT.empty)))
        .text("Finalizes the given PSBT if it can")
        .children(
          arg[PSBT]("psbt")
            .text("PSBT serialized in hex or base64 format")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case finalizePSBT: FinalizePSBT =>
                  finalizePSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("extractfrompsbt")
        .action((_, conf) => conf.copy(command = ExtractFromPSBT(PSBT.empty)))
        .text("Extracts a transaction from the given PSBT if it can")
        .children(
          arg[PSBT]("psbt")
            .text("PSBT serialized in hex or base64 format")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case extractFromPSBT: ExtractFromPSBT =>
                  extractFromPSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("converttopsbt")
        .action((_, conf) =>
          conf.copy(command = ConvertToPSBT(EmptyTransaction)))
        .text("Creates an empty psbt from the given transaction")
        .children(
          arg[Transaction]("unsignedTx")
            .text("serialized unsigned transaction in hex")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case convertToPSBT: ConvertToPSBT =>
                  convertToPSBT.copy(tx = tx)
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== Oracle ==="),
      cmd("getpublickey")
        .action((_, conf) => conf.copy(command = GetPublicKey))
        .text(s"Get oracle's public key"),
      cmd("getstakingaddress")
        .action((_, conf) => conf.copy(command = GetStakingAddress))
        .text(s"Get oracle's staking address"),
      cmd("exportstakingaddresswif")
        .action((_, conf) => conf.copy(command = ExportStakingAddressWif))
        .text(s"Export the private key for the wallet's staking address"),
      cmd("listannouncements")
        .action((_, conf) => conf.copy(command = ListAnnouncements))
        .text(s"Lists all announcement names"),
      cmd("createenumannouncement")
        .action((_, conf) =>
          conf.copy(command =
            CreateEnumAnnouncement("", new Date(), Seq.empty)))
        .text("Registers an oracle enum announcement")
        .children(
          arg[String]("label")
            .text("Label for this announcement")
            .required()
            .action((label, conf) =>
              conf.copy(command = conf.command match {
                case createEvent: CreateEnumAnnouncement =>
                  createEvent.copy(label = label)
                case other => other
              })),
          arg[Date]("maturationtime")
            .text(
              "The earliest expected time an outcome will be signed, given in ISO 8601 format"
            )
            .required()
            .action((date, conf) =>
              conf.copy(command = conf.command match {
                case createEvent: CreateEnumAnnouncement =>
                  createEvent.copy(maturationTime = date)
                case other => other
              })),
          arg[Seq[String]]("outcomes")
            .text("Possible outcomes for this event")
            .required()
            .action((outcomes, conf) =>
              conf.copy(command = conf.command match {
                case createEvent: CreateEnumAnnouncement =>
                  createEvent.copy(outcomes = outcomes)
                case other => other
              }))
        ),
      cmd("createnumericannouncement")
        .action((_, conf) =>
          conf.copy(command = CreateNumericAnnouncement(
            eventName = "",
            maturationTime = new Date(),
            minValue = 0,
            maxValue = 0,
            unit = "",
            precision = 0
          )))
        .text(
          "Registers an oracle announcement that uses digit decomposition when signing the number"
        )
        .children(
          arg[String]("name")
            .text("Name for this announcement")
            .required()
            .action((name, conf) =>
              conf.copy(command = conf.command match {
                case createNumericEvent: CreateNumericAnnouncement =>
                  createNumericEvent.copy(eventName = name)
                case other => other
              })),
          arg[Date]("maturationtime")
            .text(
              "The earliest expected time an outcome will be signed, given in ISO 8601 format"
            )
            .required()
            .action((date, conf) =>
              conf.copy(command = conf.command match {
                case createNumericEvent: CreateNumericAnnouncement =>
                  createNumericEvent.copy(maturationTime = date)
                case other => other
              })),
          arg[Long]("minvalue")
            .text("Minimum value of this announcement")
            .required()
            .action((min, conf) =>
              conf.copy(command = conf.command match {
                case createNumericEvent: CreateNumericAnnouncement =>
                  createNumericEvent.copy(minValue = min)
                case other => other
              })),
          arg[Long]("maxvalue")
            .text("Maximum value of this announcement")
            .required()
            .action((max, conf) =>
              conf.copy(command = conf.command match {
                case createNumericEvent: CreateNumericAnnouncement =>
                  createNumericEvent.copy(maxValue = max)
                case other => other
              })),
          arg[String]("unit")
            .text("The unit denomination of the outcome value")
            .action((unit, conf) =>
              conf.copy(command = conf.command match {
                case createNumericEvent: CreateNumericAnnouncement =>
                  createNumericEvent.copy(unit = unit)
                case other => other
              })),
          arg[Int]("precision")
            .text(
              "The precision of the outcome representing the " +
                "base exponent by which to multiply the number represented by " +
                "the composition of the digits to obtain the actual outcome value."
            )
            .action((precision, conf) =>
              conf.copy(command = conf.command match {
                case createNumericEvent: CreateNumericAnnouncement =>
                  createNumericEvent.copy(precision = precision)
                case other => other
              }))
        ),
      cmd("createdigitdecompannouncement")
        .action((_, conf) =>
          conf.copy(command = CreateDigitDecompAnnouncement(
            "",
            Instant.MIN,
            0,
            isSigned = false,
            0,
            "",
            0
          )))
        .text(
          "Registers an oracle announcement that uses digit decomposition when signing the number"
        )
        .children(
          arg[String]("name")
            .text("Name for this announcement")
            .required()
            .action((name, conf) =>
              conf.copy(command = conf.command match {
                case createLargeRangedEvent: CreateDigitDecompAnnouncement =>
                  createLargeRangedEvent.copy(eventName = name)
                case other => other
              })),
          arg[Instant]("maturationtime")
            .text(
              "The earliest expected time an outcome will be signed, given in epoch second"
            )
            .required()
            .action((time, conf) =>
              conf.copy(command = conf.command match {
                case createLargeRangedEvent: CreateDigitDecompAnnouncement =>
                  createLargeRangedEvent.copy(maturationTime = time)
                case other => other
              })),
          arg[Int]("base")
            .text("The base in which the outcome value is decomposed")
            .required()
            .action((base, conf) =>
              conf.copy(command = conf.command match {
                case createLargeRangedEvent: CreateDigitDecompAnnouncement =>
                  createLargeRangedEvent.copy(base = base)
                case other => other
              })),
          arg[Int]("numdigits")
            .text("The max number of digits the outcome can have")
            .action((num, conf) =>
              conf.copy(command = conf.command match {
                case createLargeRangedEvent: CreateDigitDecompAnnouncement =>
                  createLargeRangedEvent.copy(numDigits = num)
                case other => other
              })),
          opt[Unit]("signed")
            .text("Whether the outcomes can be negative")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case createLargeRangedEvent: CreateDigitDecompAnnouncement =>
                  createLargeRangedEvent.copy(isSigned = true)
                case other => other
              })),
          arg[String]("unit")
            .text("The unit denomination of the outcome value")
            .action((unit, conf) =>
              conf.copy(command = conf.command match {
                case createRangedEvent: CreateDigitDecompAnnouncement =>
                  createRangedEvent.copy(unit = unit)
                case other => other
              })),
          arg[Int]("precision")
            .text(
              "The precision of the outcome representing the " +
                "base exponent by which to multiply the number represented by " +
                "the composition of the digits to obtain the actual outcome value."
            )
            .action((precision, conf) =>
              conf.copy(command = conf.command match {
                case createLargeRangedEvent: CreateDigitDecompAnnouncement =>
                  createLargeRangedEvent.copy(precision = precision)
                case other => other
              }))
        ),
      cmd("deleteannouncement")
        .action((_, conf) => conf.copy(command = DeleteAnnouncement("")))
        .text(
          "Delete an announcement. WARNING: THIS CAN LEAD TO DLCs NOT SETTLING IF USERS HAVE BUILT DLCS OFF OF THIS ANNOUNCEMENT. USE WITH CARE."
        )
        .children(
          arg[String]("eventName")
            .text("The event's name")
            .required()
            .action((eventName, conf) =>
              conf.copy(command = conf.command match {
                case delete: DeleteAnnouncement =>
                  delete.copy(eventName = eventName)
                case other => other
              }))
        ),
      cmd("deleteattestation")
        .action((_, conf) => conf.copy(command = DeleteAttestation("")))
        .text(
          "Delete an announcement. WARNING THIS CAN LEAD TO PRIVATE KEY LEAK IF YOU SIGN ANOTHER ATTESTATION AFTER DELETING A PREVIOUS ONE. USE WITH CARE."
        )
        .children(
          arg[String]("eventName")
            .text("The event's name")
            .required()
            .action((eventName, conf) =>
              conf.copy(command = conf.command match {
                case delete: DeleteAttestation =>
                  delete.copy(eventName = eventName)
                case other => other
              }))
        ),
      cmd("getannouncement")
        .action((_, conf) => conf.copy(command = GetAnnouncement("")))
        .text("Get an announcement's details")
        .children(
          arg[String]("announcementName")
            .text("The announcement's name")
            .required()
            .action((eventName, conf) =>
              conf.copy(command = conf.command match {
                case getEvent: GetAnnouncement =>
                  getEvent.copy(eventName = eventName)
                case other => other
              }))
        ),
      cmd("signenum")
        .action((_, conf) => conf.copy(command = SignEnum("", "")))
        .text("Creates attestations for an announcement")
        .children(
          arg[String]("announcementName")
            .text("The announcement's name")
            .required()
            .action((eventName, conf) =>
              conf.copy(command = conf.command match {
                case signEvent: SignEnum =>
                  signEvent.copy(eventName = eventName)
                case other => other
              })),
          arg[String]("outcome")
            .text("Outcome to sign for this event")
            .required()
            .action((outcome, conf) =>
              conf.copy(command = conf.command match {
                case signEvent: SignEnum =>
                  signEvent.copy(outcome = outcome)
                case other => other
              }))
        ),
      cmd("signdigits")
        .action((_, conf) => conf.copy(command = SignDigits("", 0)))
        .text("Signs the digits of a numeric announcement")
        .children(
          arg[String]("announcementName")
            .text("The announcement's name")
            .required()
            .action((eventName, conf) =>
              conf.copy(command = conf.command match {
                case signDigits: SignDigits =>
                  signDigits.copy(eventName = eventName)
                case other => other
              })),
          arg[Long]("outcome")
            .text("The number to sign")
            .required()
            .action((num, conf) =>
              conf.copy(command = conf.command match {
                case signDigits: SignDigits =>
                  signDigits.copy(num = num)
                case other => other
              }))
        ),
      cmd("getsignatures")
        .action((_, conf) => conf.copy(command = GetSignatures("")))
        .text("Get the signatures from a signed event")
        .children(
          arg[String]("announcementName")
            .text("The event's name")
            .required()
            .action((eventName, conf) =>
              conf.copy(command = conf.command match {
                case getSignature: GetSignatures =>
                  getSignature.copy(eventName = eventName)
                case other => other
              }))
        ),
      cmd("signmessage")
        .action((_, conf) => conf.copy(command = SignMessage("")))
        .text(
          "Signs the SHA256 hash of the given string using the oracle's signing key"
        )
        .children(
          arg[String]("message")
            .text("Message to hash and sign")
            .required()
            .action((msg, conf) =>
              conf.copy(command = conf.command match {
                case signMessage: SignMessage =>
                  signMessage.copy(message = msg)
                case other => other
              }))
        ),
      cmd("getoraclename")
        .action((_, conf) => conf.copy(command = GetOracleName))
        .text("Returns the oracle's name"),
      cmd("setoraclename")
        .action((_, conf) => conf.copy(command = SetOracleName(null)))
        .text("Sets the oracle's name")
        .children(
          arg[String]("name")
            .text("Oracle name")
            .required()
            .action((name, conf) =>
              conf.copy(command = conf.command match {
                case wps: SetOracleName =>
                  wps.copy(name = name)
                case other => other
              }))
        ),
      cmd("backuporacle")
        .action((_, conf) => conf.copy(command = BackupOracle(null)))
        .text("Backs up the oracle SQLite database")
        .children(
          arg[String]("dest")
            .text("Destination file name")
            .required()
            .action((dest, conf) =>
              conf.copy(command = conf.command match {
                case wps: BackupOracle =>
                  wps.copy(destination = dest)
                case other => other
              }))
        ),
      note(sys.props("line.separator") + "=== Util ==="),
      cmd("createmultisig")
        .action((_, conf) =>
          conf.copy(command = CreateMultisig(0, Vector.empty, SegWit)))
        .text(
          "Creates a multi-signature address with n signature of m keys required."
        )
        .children(
          arg[Int]("nrequired")
            .text("The number of required signatures out of the n keys.")
            .required()
            .action((nRequired, conf) =>
              conf.copy(command = conf.command match {
                case createMultisig: CreateMultisig =>
                  createMultisig.copy(requiredKeys = nRequired)
                case other => other
              })),
          arg[Seq[ECPublicKey]]("keys")
            .text("The hex-encoded public keys.")
            .required()
            .action((keys, conf) =>
              conf.copy(command = conf.command match {
                case createMultisig: CreateMultisig =>
                  createMultisig.copy(keys = keys.toVector)
                case other => other
              })),
          arg[AddressType]("address_type")
            .text(
              "The address type to use. Options are \"legacy\", \"p2sh-segwit\", and \"bech32\""
            )
            .optional()
            .action((addrType, conf) =>
              conf.copy(command = conf.command match {
                case createMultisig: CreateMultisig =>
                  createMultisig.copy(addressType = addrType)
                case other => other
              }))
        ),
      cmd("estimatefee")
        .action((_, conf) => conf.copy(command = EstimateFee))
        .text("Returns the recommended fee rate using the fee provider"),
      cmd("zipdatadir")
        .action((_, conf) =>
          conf.copy(command = ZipDataDir(new File("").toPath)))
        .text(
          "zips the bitcoin-s datadir and places the file at the given path"
        )
        .children(
          arg[Path]("path")
            .text("Location of final zip file")
            .required()
            .action((path, conf) =>
              conf.copy(command = conf.command match {
                case zipDataDir: ZipDataDir =>
                  zipDataDir.copy(path = path)
                case other => other
              }))
        ),
      checkConfig {
        case Config(org.bitcoins.commons.rpc.CliCommand.NoCommand, _, _, _) =>
          failure("You need to provide a command!")
        case _ => success
      }
    )
  }

  def exec(args: Vector[String]): Try[String] = {
    val config = OParser.parse(parser, args, Config()) match {
      case None       => sys.exit(1)
      case Some(conf) => conf
    }

    exec(config.command, config)
  }

  /** Prints the given message to stderr and exist */
  private def error[T](message: String): Failure[T] = {
    Failure(new RuntimeException(message))
  }

  /** Gets the given key from jsObj if it exists and is not null
    */
  private def getKey(
      key: String,
      jsObjT: Try[mutable.LinkedHashMap[String, ujson.Value]]
  ): Option[ujson.Value] = {
    jsObjT.toOption.flatMap(
      _.get(key).flatMap(result => if (result.isNull) None else Some(result))
    )
  }

  /** Converts a `ujson.Value` to String, making an effort to avoid preceding
    * and trailing `"` s
    */
  private def jsValueToString(value: ujson.Value): String = {
    value match {
      case Str(string)             => string
      case Num(num) if num.isWhole => num.toLong.toString
      case Num(num)                => num.toString
      case rest: ujson.Value       => rest.render(2)
    }
  }

  private val backend: SttpBackend[Identity, Any] =
    Slf4jLoggingBackend(
      sttp.client3.HttpURLConnectionBackend(),
      responseExceptionLogLevel = LogLevel.Warn
    )

  def exec(
      command: org.bitcoins.commons.rpc.CliCommand,
      config: Config
  ): Try[String] = {

    val requestParam = CliCommand.buildRequest(command)
    import sttp.client3._

    val request =
      sttp.client3.basicRequest
        .post(uri"http://$host:${config.rpcPort}/")
        .contentType("application/json")
        .auth
        .basic("bitcoins", config.rpcPassword)
        .body {
          val uuid = java.util.UUID.randomUUID.toString
          val paramsWithID: Map[String, ujson.Value] =
            requestParam.toJsonMap + ("id" -> up
              .writeJs(uuid))
          up.write(paramsWithID)
        }
    logger.debug(s"HTTP request: $request")
    val response: Response[Either[String, String]] =
      try { backend.send(request) }
      catch {
        case scala.util.control.NonFatal(err) =>
          Response.apply(Left(err.getMessage), StatusCode.BadRequest)
      }

    logger.debug(s"HTTP response:" + response)
    // in order to mimic Bitcoin Core we always send
    // an object looking like {"result": ..., "error": ...}
    val rawBody = response.body match {
      case Left(err)       => err
      case Right(response) => response
    }
    Iterator.apply()
    val jsObjT
        : Try[scala.collection.mutable.LinkedHashMap[String, ujson.Value]] = {
      Try(ujson.read(rawBody).obj)
        .transform[scala.collection.mutable.LinkedHashMap[String, ujson.Value]](
          { case v: upickle.core.LinkedHashMap[String, ujson.Value] =>
            Success(
              scala.collection.mutable.LinkedHashMap.from(v.iterator.toVector)
            )
          },
          _ =>
            Success(
              scala.collection.mutable.LinkedHashMap[String, ujson.Value](
                "error" -> Str(rawBody)
              )
            )
        )
    }

    (getKey("result", jsObjT), getKey("error", jsObjT)) match {
      case (Some(result), None) =>
        Success(jsValueToString(result))
      case (None, Some(err)) =>
        val msg = jsValueToString(err)
        error(msg)
      case (None, None) => Success("")
      case (None, None) | (Some(_), Some(_)) =>
        error(s"Got unexpected response: $rawBody")
    }
  }

  def host = "localhost"

  case class RequestParam(
      method: String,
      params: Seq[ujson.Value.Value] = Nil
  ) {

    lazy val toJsonMap: Map[String, ujson.Value] = {
      if (params.isEmpty)
        Map("method" -> method)
      else
        Map("method" -> method, "params" -> params)
    }
  }
}

case class Config(
    command: org.bitcoins.commons.rpc.CliCommand =
      org.bitcoins.commons.rpc.CliCommand.NoCommand,
    network: Option[NetworkParameters] = None,
    rpcPortOpt: Option[Int] = None,
    rpcPassword: String = ""
) {

  val rpcPort: Int = rpcPortOpt match {
    case Some(port) => port
    case None       => command.defaultPort
  }
}

object Config {
  val empty: Config = Config()
}

object CliCommand {

  def buildRequest(command: CliCommand): RequestParam = {
    val requestParam: RequestParam = command match {
      case GetInfo =>
        RequestParam("getinfo")
      case GetMedianTimePast =>
        RequestParam("getmediantimepast")
      case GetUtxos =>
        RequestParam("getutxos")
      case GetReservedUtxos =>
        RequestParam("getreservedutxos")
      case GetAddresses =>
        RequestParam("getaddresses")
      case GetSpentAddresses =>
        RequestParam("getspentaddresses")
      case GetFundedAddresses =>
        RequestParam("getfundedaddresses")
      case GetUnusedAddresses =>
        RequestParam("getunusedaddresses")
      case GetAccounts =>
        RequestParam("getaccounts")
      case CreateNewAccount(purpose) =>
        RequestParam("createnewaccount", Seq(up.writeJs(purpose)))
      case IsEmpty =>
        RequestParam("isempty")
      case WalletInfo =>
        RequestParam("walletinfo")
      case ListWallets =>
        RequestParam("listwallets")
      // DLCs
      case ContactAdd(alias, address, memo) =>
        RequestParam(
          "contact-add",
          Seq(up.writeJs(alias), up.writeJs(address), up.writeJs(memo))
        )
      case ContactsList =>
        RequestParam("contacts-list", Seq.empty)
      case ContactRemove(address) =>
        RequestParam("contact-remove", Seq(up.writeJs(address)))
      case GetDLCHostAddress => RequestParam("getdlchostaddress")

      case DecodeContractInfo(contractInfo) =>
        RequestParam("decodecontractinfo", Seq(up.writeJs(contractInfo)))

      case DecodeOffer(offer) =>
        RequestParam("decodeoffer", Seq(up.writeJs(offer)))
      case DecodeAnnouncement(announcement) =>
        RequestParam("decodeannouncement", Seq(up.writeJs(announcement)))

      case DecodeAttestments(attestments) =>
        RequestParam("decodeattestments", Seq(up.writeJs(attestments)))

      case GetDLCs => RequestParam("getdlcs")
      case GetDLC(dlcId) =>
        RequestParam("getdlc", Seq(up.writeJs(dlcId)))
      case CreateDLCOffer(
            contractInfoTLV,
            collateral,
            feeRateOpt,
            locktimeOpt,
            refundLocktime,
            externalPayoutAddressOpt,
            externalChangeAddressOpt,
            peerAddressOpt
          ) =>
        RequestParam(
          "createdlcoffer",
          Seq(
            up.writeJs(contractInfoTLV),
            up.writeJs(collateral),
            up.writeJs(feeRateOpt),
            up.writeJs(locktimeOpt),
            up.writeJs(refundLocktime),
            up.writeJs(externalPayoutAddressOpt),
            up.writeJs(externalChangeAddressOpt),
            up.writeJs(peerAddressOpt)
          )
        )
      case AcceptDLC(
            offer,
            peerAddr,
            externalPayoutAddressOpt,
            externalChangeAddressOpt
          ) =>
        RequestParam(
          "acceptdlc",
          Seq(
            up.writeJs(offer),
            up.writeJs(peerAddr),
            up.writeJs(externalPayoutAddressOpt),
            up.writeJs(externalChangeAddressOpt)
          )
        )
      case AcceptDLCOffer(
            offer,
            peerAddr,
            externalPayoutAddressOpt,
            externalChangeAddressOpt
          ) =>
        RequestParam(
          "acceptdlcoffer",
          Seq(
            up.writeJs(offer),
            up.writeJs(peerAddr),
            up.writeJs(externalPayoutAddressOpt),
            up.writeJs(externalChangeAddressOpt)
          )
        )
      case AcceptDLCOfferFromFile(path, dest) =>
        RequestParam(
          "acceptdlcofferfromfile",
          Seq(up.writeJs(path), up.writeJs(dest))
        )
      case SignDLC(accept) =>
        RequestParam("signdlc", Seq(up.writeJs(accept)))
      case SignDLCFromFile(path, dest) =>
        RequestParam("signdlcfromfile", Seq(up.writeJs(path), up.writeJs(dest)))
      case AddDLCSigs(sigs) =>
        RequestParam("adddlcsigs", Seq(up.writeJs(sigs)))
      case AddDLCSigsFromFile(path) =>
        RequestParam("adddlcsigsfromfile", Seq(up.writeJs(path)))
      case AddDLCSigsAndBroadcast(sigs) =>
        RequestParam("adddlcsigsandbroadcast", Seq(up.writeJs(sigs)))
      case AddDLCSigsAndBroadcastFromFile(path) =>
        RequestParam("adddlcsigsandbroadcastfromfile", Seq(up.writeJs(path)))
      case ExecuteDLC(contractId, oracleSigs, noBroadcast) =>
        RequestParam(
          "executedlc",
          Seq(
            up.writeJs(contractId),
            up.writeJs(oracleSigs),
            up.writeJs(noBroadcast)
          )
        )
      case GetDLCFundingTx(contractId) =>
        RequestParam("getdlcfundingtx", Seq(up.writeJs(contractId)))
      case BroadcastDLCFundingTx(contractId) =>
        RequestParam("broadcastdlcfundingtx", Seq(up.writeJs(contractId)))
      case ExecuteDLCRefund(contractId, noBroadcast) =>
        RequestParam(
          "executedlcrefund",
          Seq(up.writeJs(contractId), up.writeJs(noBroadcast))
        )
      case CancelDLC(dlcId) =>
        RequestParam("canceldlc", Seq(up.writeJs(dlcId)))
      // Wallet
      case GetBalance(isSats) =>
        RequestParam("getbalance", Seq(up.writeJs(isSats)))
      case GetBalances(isSats) =>
        RequestParam("getbalances", Seq(up.writeJs(isSats)))
      case GetConfirmedBalance(isSats) =>
        RequestParam("getconfirmedbalance", Seq(up.writeJs(isSats)))
      case GetUnconfirmedBalance(isSats) =>
        RequestParam("getunconfirmedbalance", Seq(up.writeJs(isSats)))
      case GetAddressInfo(address) =>
        RequestParam("getaddressinfo", Seq(up.writeJs(address)))
      case GetNewAddress(labelOpt) =>
        RequestParam("getnewaddress", Seq(up.writeJs(labelOpt)))
      case LockUnspent(unlock, outPoints) =>
        RequestParam(
          "lockunspent",
          Seq(up.writeJs(unlock), up.writeJs(outPoints))
        )
      case LabelAddress(address, label) =>
        RequestParam(
          "labeladdress",
          Seq(up.writeJs(address), up.writeJs(label))
        )
      case GetAddressTags(address) =>
        RequestParam("getaddresstags", Seq(up.writeJs(address)))
      case GetAddressLabel(address) =>
        RequestParam("getaddresslabel", Seq(up.writeJs(address)))
      case GetAddressLabels =>
        RequestParam("getaddresslabels")
      case DropAddressLabel(address, label) =>
        RequestParam(
          "dropaddresslabel",
          Seq(up.writeJs(address), ujson.Str(label))
        )
      case DropAddressLabels(address) =>
        RequestParam("dropaddresslabels", Seq(up.writeJs(address)))
      case Rescan(
            addressBatchSize,
            startBlock,
            endBlock,
            force,
            ignoreCreationTime
          ) =>
        RequestParam(
          "rescan",
          Seq(
            up.writeJs(addressBatchSize),
            up.writeJs(startBlock),
            up.writeJs(endBlock),
            up.writeJs(force),
            up.writeJs(ignoreCreationTime)
          )
        )

      case GetTransaction(txId) =>
        RequestParam("gettransaction", Seq(up.writeJs(txId)))

      case SendToAddress(
            address,
            bitcoins,
            satoshisPerVirtualByte,
            noBroadcast
          ) =>
        RequestParam(
          "sendtoaddress",
          Seq(
            up.writeJs(address),
            up.writeJs(bitcoins),
            up.writeJs(satoshisPerVirtualByte),
            up.writeJs(noBroadcast)
          )
        )
      case SendFromOutPoints(outPoints, address, bitcoins, feeRateOpt) =>
        RequestParam(
          "sendfromoutpoints",
          Seq(
            up.writeJs(outPoints),
            up.writeJs(address),
            up.writeJs(bitcoins),
            up.writeJs(feeRateOpt)
          )
        )
      case SweepWallet(address, feeRateOpt) =>
        RequestParam(
          "sweepwallet",
          Seq(up.writeJs(address), up.writeJs(feeRateOpt))
        )
      case SendWithAlgo(address, bitcoins, feeRateOpt, algo) =>
        RequestParam(
          "sendwithalgo",
          Seq(
            up.writeJs(address),
            up.writeJs(bitcoins),
            up.writeJs(feeRateOpt),
            up.writeJs(algo)
          )
        )
      case BumpFeeCPFP(txId, feeRate) =>
        RequestParam("bumpfeecpfp", Seq(up.writeJs(txId), up.writeJs(feeRate)))
      case BumpFeeRBF(txId, feeRate) =>
        RequestParam("bumpfeerbf", Seq(up.writeJs(txId), up.writeJs(feeRate)))
      case OpReturnCommit(message, hashMessage, satoshisPerVirtualByte) =>
        RequestParam(
          "opreturncommit",
          Seq(
            up.writeJs(message),
            up.writeJs(hashMessage),
            up.writeJs(satoshisPerVirtualByte)
          )
        )
      case SignPSBT(psbt) =>
        RequestParam("signpsbt", Seq(up.writeJs(psbt)))

      case KeyManagerPassphraseChange(oldPassword, newPassword) =>
        RequestParam(
          "keymanagerpassphrasechange",
          Seq(up.writeJs(oldPassword), up.writeJs(newPassword))
        )

      case KeyManagerPassphraseSet(password) =>
        RequestParam("keymanagerpassphraseset", Seq(up.writeJs(password)))

      case ImportSeed(walletNameOpt, mnemonic, passwordOpt) =>
        RequestParam(
          "importseed",
          Seq(
            walletNameOpt.map(w => up.writeJs(w)).getOrElse(Null),
            up.writeJs(mnemonic),
            passwordOpt.map(p => up.writeJs(p)).getOrElse(Null)
          )
        )

      case ExportSeed(walletNameOpt, passwordOpt) =>
        RequestParam(
          "exportseed",
          Seq(
            walletNameOpt.map(w => up.writeJs(w)).getOrElse(Null),
            passwordOpt.map(p => up.writeJs(p)).getOrElse(Null)
          )
        )

      case MarkSeedAsBackedUp(walletNameOpt, passwordOpt) =>
        RequestParam(
          "markseedasbackedup",
          Seq(
            walletNameOpt.map(w => up.writeJs(w)).getOrElse(Null),
            passwordOpt.map(p => up.writeJs(p)).getOrElse(Null)
          )
        )

      case GetSeedBackupTime(walletNameOpt, passwordOpt) =>
        RequestParam(
          "getseedbackuptime",
          Seq(
            walletNameOpt.map(w => up.writeJs(w)).getOrElse(Null),
            passwordOpt.map(p => up.writeJs(p)).getOrElse(Null)
          )
        )

      case ImportXprv(walletNameOpt, xprv, passwordOpt) =>
        RequestParam(
          "importxprv",
          Seq(
            walletNameOpt.map(w => up.writeJs(w)).getOrElse(Null),
            up.writeJs(xprv),
            passwordOpt.map(p => up.writeJs(p)).getOrElse(Null)
          )
        )

      case LoadWallet(walletNameOpt, passwordOpt, bip39PasswordOpt) =>
        RequestParam(
          "loadwallet",
          Seq(
            walletNameOpt.map(w => up.writeJs(w)).getOrElse(Null),
            passwordOpt.map(p => up.writeJs(p)).getOrElse(Null),
            bip39PasswordOpt.map(p => up.writeJs(p)).getOrElse(Null)
          )
        )

      case GetBlockHeader(hash) =>
        RequestParam("getblockheader", Seq(up.writeJs(hash)))
      // height
      case GetBlockCount => RequestParam("getblockcount")
      // filter count
      case GetFilterCount => RequestParam("getfiltercount")
      // filter header count
      case GetFilterHeaderCount => RequestParam("getfilterheadercount")
      // besthash
      case GetBestBlockHash => RequestParam("getbestblockhash")
      // peers
      case GetConnectionCount => RequestParam("getconnectioncount")
      case Stop               => RequestParam("stop")
      case SendRawTransaction(tx) =>
        RequestParam("sendrawtransaction", Seq(up.writeJs(tx)))
      // PSBTs
      case DecodePSBT(psbt) =>
        RequestParam("decodepsbt", Seq(up.writeJs(psbt)))
      case CombinePSBTs(psbts) =>
        RequestParam("combinepsbts", Seq(up.writeJs(psbts)))
      case JoinPSBTs(psbts) =>
        RequestParam("joinpsbts", Seq(up.writeJs(psbts)))
      case FinalizePSBT(psbt) =>
        RequestParam("finalizepsbt", Seq(up.writeJs(psbt)))
      case ExtractFromPSBT(psbt) =>
        RequestParam("extractfrompsbt", Seq(up.writeJs(psbt)))
      case ConvertToPSBT(tx) =>
        RequestParam("converttopsbt", Seq(up.writeJs(tx)))

      case DecodeRawTransaction(tx) =>
        RequestParam("decoderawtransaction", Seq(up.writeJs(tx)))

      case AnalyzePSBT(psbt) =>
        RequestParam("analyzepsbt", Seq(up.writeJs(psbt)))

      // Oracle
      case GetPublicKey =>
        RequestParam("getpublickey")
      case GetOracleName =>
        RequestParam("getoraclename")
      case SetOracleName(oracleName) =>
        RequestParam("setoraclename", Seq(up.writeJs(oracleName)))
      case GetStakingAddress =>
        RequestParam("getstakingaddress")
      case ExportStakingAddressWif =>
        RequestParam("exportstakingaddresswif")
      case ListAnnouncements =>
        RequestParam("listannouncements")
      case GetAnnouncement(eventName) =>
        RequestParam("getannouncement", Seq(up.writeJs(eventName)))
      case CreateEnumAnnouncement(label, time, outcomes) =>
        RequestParam(
          "createenumannouncement",
          Seq(up.writeJs(label), up.writeJs(time), up.writeJs(outcomes))
        )
      case CreateNumericAnnouncement(
            eventName,
            time,
            minValue,
            maxValue,
            unit,
            precision
          ) =>
        RequestParam(
          "createnumericannouncement",
          Seq(
            up.writeJs(eventName),
            up.writeJs(time),
            up.writeJs(minValue),
            up.writeJs(maxValue),
            up.writeJs(unit),
            up.writeJs(precision)
          )
        )
      case CreateDigitDecompAnnouncement(
            eventName,
            time,
            base,
            isSigned,
            numDigits,
            unit,
            precision
          ) =>
        RequestParam(
          "createdigitdecompannouncement",
          Seq(
            up.writeJs(eventName),
            up.writeJs(time),
            up.writeJs(base),
            up.writeJs(isSigned),
            up.writeJs(numDigits),
            up.writeJs(unit),
            up.writeJs(precision)
          )
        )
      case SignEnum(eventName, outcome) =>
        RequestParam(
          "signenum",
          Seq(up.writeJs(eventName), up.writeJs(outcome))
        )
      case SignDigits(eventName, num) =>
        RequestParam("signdigits", Seq(up.writeJs(eventName), up.writeJs(num)))
      case GetSignatures(eventName) =>
        RequestParam("getsignatures", Seq(up.writeJs(eventName)))

      case SignMessage(message) =>
        RequestParam("signmessage", Seq(up.writeJs(message)))

      case DeleteAnnouncement(eventName) =>
        RequestParam("deleteannouncement", Seq(up.writeJs(eventName)))
      case DeleteAttestation(eventName) =>
        RequestParam("deleteattestation", Seq(up.writeJs(eventName)))
      case BackupOracle(dest) =>
        RequestParam("backuporacle", Seq(up.writeJs(dest)))

      case CreateMultisig(requiredKeys, keys, addressType) =>
        RequestParam(
          "createmultisig",
          Seq(
            up.writeJs(requiredKeys),
            up.writeJs(keys),
            up.writeJs(addressType)
          )
        )
      case EstimateFee => RequestParam("estimatefee")
      case ZipDataDir(path) =>
        RequestParam("zipdatadir", Seq(up.writeJs(path)))

      case GetDLCWalletAccounting =>
        RequestParam("getdlcwalletaccounting")
      case GetVersion =>
        RequestParam("getversion", Seq.empty)
      case CreateContractInfo(ann, totalCollateral, contractDescriptor) =>
        val args = Seq(
          up.writeJs(ann),
          up.writeJs(totalCollateral),
          up.writeJs(contractDescriptor)
        )
        RequestParam("createcontractinfo", args)

      case AddDLCOffer(offer, peer, message) =>
        val args = Seq(up.writeJs(offer), up.writeJs(peer), up.writeJs(message))
        RequestParam("offer-add", args)

      case RemoveDLCOffer(offerHash) =>
        val args = Seq(up.writeJs(offerHash))
        RequestParam("offer-remove", args)

      case cmd @ (_: ServerlessCliCommand | _: AppServerCliCommand |
          _: Broadcastable | _: OracleServerCliCommand) =>
        sys.error(s"Command $cmd unsupported")
      case org.bitcoins.commons.rpc.CliCommand.NoCommand => ???
    }
    requestParam
  }

  case object GetVersion extends ServerlessCliCommand

  case object GetInfo extends AppServerCliCommand

  // DLC
  case object GetDLCHostAddress extends AppServerCliCommand

  case class DecodeAttestments(sigs: OracleAttestmentV0TLV)
      extends AppServerCliCommand

  sealed trait AcceptDLCCliCommand extends AppServerCliCommand

  case class AcceptDLCOfferFromFile(path: Path, destination: Option[Path])
      extends AcceptDLCCliCommand

  sealed trait SignDLCCliCommand extends AppServerCliCommand

  sealed trait AddDLCSigsCliCommand extends AppServerCliCommand

  case class AddDLCSigsFromFile(path: Path) extends AddDLCSigsCliCommand

  sealed trait AddDLCSigsAndBroadcastCliCommand extends AddDLCSigsCliCommand

  case class AddDLCSigsAndBroadcast(sigs: LnMessage[DLCSignTLV])
      extends AddDLCSigsAndBroadcastCliCommand

  case class AddDLCSigsAndBroadcastFromFile(path: Path)
      extends AddDLCSigsAndBroadcastCliCommand

  case class CancelDLC(dlcId: Sha256Digest) extends AppServerCliCommand

  case class AddDLCOffer(
      offer: LnMessage[DLCOfferTLV],
      peer: String,
      message: String
  ) extends AppServerCliCommand

  case class RemoveDLCOffer(offerHash: Sha256Digest) extends AppServerCliCommand

  // Wallet

  case object GetAddressLabels extends AppServerCliCommand
  case object GetUtxos extends AppServerCliCommand
  case object GetReservedUtxos extends AppServerCliCommand
  case object GetAddresses extends AppServerCliCommand
  case object GetSpentAddresses extends AppServerCliCommand
  case object GetFundedAddresses extends AppServerCliCommand
  case object GetUnusedAddresses extends AppServerCliCommand
  case object GetAccounts extends AppServerCliCommand
  case object IsEmpty extends AppServerCliCommand
  case object WalletInfo extends AppServerCliCommand
  case object ListWallets extends AppServerCliCommand

  case class GetBalances(isSats: Boolean) extends AppServerCliCommand

  case object GetDLCWalletAccounting extends AppServerCliCommand

  // Node
  case object GetConnectionCount extends AppServerCliCommand
  case object Stop extends AppServerCliCommand

  // Chain
  case object GetBestBlockHash extends AppServerCliCommand
  case object GetBlockCount extends AppServerCliCommand
  case object GetFilterCount extends AppServerCliCommand
  case object GetFilterHeaderCount extends AppServerCliCommand

  case object GetMedianTimePast extends AppServerCliCommand

  // PSBT

  // Util

  case class ZipDataDir(path: Path) extends AppServerCliCommand

  case object EstimateFee extends AppServerCliCommand

  // Oracle
  case object GetPublicKey extends OracleServerCliCommand
  case object GetStakingAddress extends OracleServerCliCommand

  case object ExportStakingAddressWif extends OracleServerCliCommand
  case object ListAnnouncements extends OracleServerCliCommand

  case class GetAnnouncement(eventName: String) extends OracleServerCliCommand

  case class CreateEnumAnnouncement(
      label: String,
      maturationTime: Date,
      outcomes: Seq[String]
  ) extends OracleServerCliCommand

  case class CreateNumericAnnouncement(
      eventName: String,
      maturationTime: Date,
      minValue: Long,
      maxValue: Long,
      unit: String,
      precision: Int
  ) extends OracleServerCliCommand

  case class CreateDigitDecompAnnouncement(
      eventName: String,
      maturationTime: Instant,
      base: Int,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int
  ) extends OracleServerCliCommand

  case class SignEnum(eventName: String, outcome: String)
      extends OracleServerCliCommand

  case class SignDigits(eventName: String, num: Long)
      extends OracleServerCliCommand

  case class GetSignatures(eventName: String) extends OracleServerCliCommand

  case class SignMessage(message: String) extends OracleServerCliCommand

  case class DeleteAnnouncement(eventName: String)
      extends OracleServerCliCommand

  case class DeleteAttestation(eventName: String) extends OracleServerCliCommand

  case class BackupOracle(destination: String) extends OracleServerCliCommand

  case object GetOracleName extends OracleServerCliCommand

  case class SetOracleName(name: String) extends OracleServerCliCommand
}
