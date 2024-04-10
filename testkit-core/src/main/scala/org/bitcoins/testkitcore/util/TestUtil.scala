package org.bitcoins.testkitcore.util

import org.bitcoins.core.crypto.BaseTxSigComponent
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2SHScriptSignature,
  ScriptPubKey,
  ScriptSignature
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionInput,
  TransactionOutput
}
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.{
  OP_CHECKMULTISIG,
  OP_CHECKSIG,
  OP_HASH160
}
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.core.serializers.script.RawScriptPubKeyParser
import org.bitcoins.core.serializers.transaction.RawTransactionInputParser

/** Created by chris on 12/2/15.
  */
object TestUtil {

  def testBitcoinAddress = BitcoinAddress("n3p1ct69ao3qxWvEvzLhLtWG2zJGTjN3EV")
  def testP2SHAddress = BitcoinAddress("2MzYbQdkSVp5wVyMRp6A5PHPuQNHpiaTbCj")

  val bech32Address: Bech32Address =
    Bech32Address.fromString("bcrt1qq6w6pu6zq90az9krn53zlkvgyzkyeglzukyepf")
  def bitcoinAddress = BitcoinAddress("1C4kYhyLftmkn48YarSoLupxHfYFo8kp64")
  def multiSigAddress = BitcoinAddress("342ftSRCvFHfCeFFBuz4xwbeqnDw6BGUey")

  val p2pkhInputScript =
    "69473044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac012102af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"
  def p2pkhScriptSig = ScriptSignature(p2pkhInputScript)

  val p2pkhInputScriptNotParsedAsm =
    "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01" +
      " 02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652"

  val p2pkhInputScriptAsm: List[ScriptToken] = List(
    BytesToPushOntoStack(71),
    ScriptConstant(
      "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01"),
    BytesToPushOntoStack(33),
    ScriptConstant(
      "02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652")
  )

  val p2pkhOutputScript = "1976a914e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b88ac"

  val p2pkhOutputScriptNotParsedAsm =
    "OP_DUP OP_HASH160 e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b OP_EQUALVERIFY OP_CHECKSIG"

  val p2pkhOutputScriptAsm = List(
    OP_DUP,
    OP_HASH160,
    BytesToPushOntoStack(20),
    ScriptConstant("e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b"),
    OP_EQUALVERIFY,
    OP_CHECKSIG)

  //tx id for p2sh inputs/outputs cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val p2shInputScriptNotParsedAsm =
    "0 304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001 512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae"

  val rawP2shInputScript =
    "6f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae"
  val p2shInputScript = ScriptSignature(rawP2shInputScript)

  val p2shInputScriptAsm = List(
    OP_0,
    BytesToPushOntoStack(71),
    ScriptConstant(
      "304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001"),
    BytesToPushOntoStack(37),
    ScriptConstant(
      "512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae")
  )

  val p2shOutputScript = "17a914eda8ae08b5c9f973f49543e90a7c292367b3337c87"

  val p2shOutputScriptNotParsedAsm =
    "OP_HASH160 eda8ae08b5c9f973f49543e90a7c292367b3337c OP_EQUAL"

  val p2shOutputScriptAsm = List(
    OP_HASH160,
    BytesToPushOntoStack(20),
    ScriptConstant("eda8ae08b5c9f973f49543e90a7c292367b3337c"),
    OP_EQUAL)

  //https://btc.blockr.io/api/v1/tx/raw/791fe035d312dcf9196b48649a5c9a027198f623c0a5f5bd4cc311b8864dd0cf
  val rawP2shInputScriptSigHashSingle =
    "fdfd0000483045022100dfcfafcea73d83e1c54d444a19fb30d17317f922c19e2ff92dcda65ad09cba24022001e7a805c5672c49b222c5f2f1e67bb01f87215fb69df184e7c16f66c1f87c290347304402204a657ab8358a2edb8fd5ed8a45f846989a43655d2e8f80566b385b8f5a70dab402207362f870ce40f942437d43b6b99343419b14fb18fa69bee801d696a39b3410b8034c695221023927b5cd7facefa7b85d02f73d1e1632b3aaf8dd15d4f9f359e37e39f05611962103d2c0e82979b8aba4591fe39cffbf255b3b9c67b3d24f94de79c5013420c67b802103ec010970aae2e3d75eef0b44eaa31d7a0d13392513cd0614ff1c136b3b1020df53ae"

  def p2shInputScriptSigHashSingle =
    ScriptSignature(rawP2shInputScriptSigHashSingle)

  //p2sh script for a 2 of 2
  //https://tbtc.blockr.io/api/v1/tx/raw/2f18c646a2b2ee8ee1f295bb5a0f5cc51c5e820a123a14b0c0e170f9777518bb
  val rawP2shInputScript2Of2 =
    "da0047304402207d764cb90c9fd84b74d33a47cf3a0ffead9ded98333776becd6acd32c4426dac02203905a0d064e7f53d07793e86136571b6e4f700c1cfb888174e84d78638335b8101483045022100906aaca39f022acd8b7a38fd2f92aca9e9f35cfeaee69a6f13e1d083ae18222602204c9ed96fc6c4de56fd85c679fc59c16ee1ccc80c42563b86174e1a506fc007c8014752210369d26ebd086523384a0f89f293d4c327a65fa73332d8efd1097cb35231295b832102480863e5c4a4e9763f5380c44fcfe6a3b7787397076cf9ea1049303a9d34f72152ae"

  val rawP2SH2Of2Tx =
    "0100000001d69b8ece3059c429a83707cde2db9d8a76897b5d418c4a784a5f52d40063518f00000000da0047304402207d764cb90c9fd84b74d33a47cf3a0ffead9ded98333776becd6acd32c4426dac02203905a0d064e7f53d07793e86136571b6e4f700c1cfb888174e84d78638335b8101483045022100906aaca39f022acd8b7a38fd2f92aca9e9f35cfeaee69a6f13e1d083ae18222602204c9ed96fc6c4de56fd85c679fc59c16ee1ccc80c42563b86174e1a506fc007c8014752210369d26ebd086523384a0f89f293d4c327a65fa73332d8efd1097cb35231295b832102480863e5c4a4e9763f5380c44fcfe6a3b7787397076cf9ea1049303a9d34f72152ae0000000001b7e4ca00000000001976a914c59529a317559cfa818ddd625b7b980435b333dc88acb6917056"
  def p2shInputScript2Of2 = ScriptSignature(rawP2shInputScript2Of2)
  def p2sh2Of2Tx = Transaction(rawP2SH2Of2Tx)

  def p2shInputScript2Of2Asm =
    Seq(
      OP_0,
      BytesToPushOntoStack(71),
      ScriptConstant(
        "304402207d764cb90c9fd84b74d33a47cf3a0ffead9ded98333776becd6acd32c4426dac02203905a0d064e7f53d07793e86136571b6e4f700c1cfb888174e84d78638335b8101"),
      BytesToPushOntoStack(72),
      ScriptConstant(
        "3045022100906aaca39f022acd8b7a38fd2f92aca9e9f35cfeaee69a6f13e1d083ae18222602204c9ed96fc6c4de56fd85c679fc59c16ee1ccc80c42563b86174e1a506fc007c801"),
      BytesToPushOntoStack(71),
      OP_2,
      BytesToPushOntoStack(33),
      ScriptConstant(
        "0369d26ebd086523384a0f89f293d4c327a65fa73332d8efd1097cb35231295b83"),
      BytesToPushOntoStack(33),
      ScriptConstant(
        "02480863e5c4a4e9763f5380c44fcfe6a3b7787397076cf9ea1049303a9d34f721"),
      OP_2,
      OP_CHECKMULTISIG
    )

  //https://tbtc.blockr.io/api/v1/tx/raw/8f516300d4525f4a784a8c415d7b89768a9ddbe2cd0737a829c45930ce8e9bd6
  def rawP2SH2Of2CreditingTx =
    "0100000001fef43d6ed62f34bd1502ae0569c0c125cb484d183f887a2857ec112e548b5ba8000000006a473044022010a8b76add9224782f2ac741e32b0467c523d27632b594ae679da991975b882d022019138d13753bc7713a72c8a32f9cec8fee8c9475d6b2e386dc31e70936099c93012103f62a2ce04da197ba3efa3bcd9ae0a4021227d75f854c22bdf49c65107c3e1e7fffffffff01c80bcb000000000017a9148bee4cf71fbefe568b173dc69ec951ea3f7a05278700000000"
  def p2sh2Of2CreditingTx = Transaction(rawP2SH2Of2CreditingTx)

  //p2sh input with large amount of signatures
  //https://tbtc.blockr.io/api/v1/tx/raw/5d254a872c9197c683ea9111fb5c0e2e0f49280a89961c45b9fea76834d335fe
  val rawP2shInputScriptLargeSignature =
    "fd5e0200483045022100a077d4fe9a81411ecb796c254d8b4e0bc73ff86a42288bc3b3ecfa1ef26c00dd02202389bf96cf38c14c3a6ccb8c688339f3fd880b724322862547a8ee3b547a9df90147304402207c0692464998e7f3869f8501cdd25bbcd9d32b6fd34ae8aeae643b422a8dfd42022057eb16f8ca1f34e88babc9f8beb4c2521eb5c4dea41f8902a70d045f1c132a4401473044022024233923253c73569f4b34723a5495698bc124b099c5542a5997d13fba7d18a802203c317bddc070276c6f6c79cb3415413e608af30e4759e31b0d53eab3ca0acd4e014830450221009b9f0d8b945717d2fca3685093d547a3928d122b8894903ed51e2248303213bc022008b376422c9f2cd713b9d10b5b106d1c56c5893dcc01ae300253ed2234bdb63f014730440220257b57cb09386d82c4328461f8fe200c2f381d6b635e2a2f4ea40c8d945e9ec102201ec67d58d51a309af4d8896e9147a42944e9f9833a456f733ea5fa6954ed2fed014cf155210269992fb441ae56968e5b77d46a3e53b69f136444ae65a94041fc937bdb28d93321021df31471281d4478df85bfce08a10aab82601dca949a79950f8ddf7002bd915a2102174c82021492c2c6dfcbfa4187d10d38bed06afb7fdcd72c880179fddd641ea121033f96e43d72c33327b6a4631ccaa6ea07f0b106c88b9dc71c9000bb6044d5e88a210313d8748790f2a86fb524579b46ce3c68fedd58d2a738716249a9f7d5458a15c221030b632eeb079eb83648886122a04c7bf6d98ab5dfb94cf353ee3e9382a4c2fab02102fb54a7fcaa73c307cfd70f3fa66a2e4247a71858ca731396343ad30c7c4009ce57ae"

  def p2shInputScriptLargeSignature =
    ScriptSignature(rawP2shInputScriptLargeSignature)

  def rawP2sh2Of3ScriptSig =
    "fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae"

  def p2sh2Of3ScriptSig: P2SHScriptSignature =
    ScriptSignature(rawP2sh2Of3ScriptSig) match {
      case p2sh: P2SHScriptSignature => p2sh
      case _: ScriptSignature        => throw new RuntimeException
    }

  //txid on testnet 44e504f5b7649d215be05ad9f09026dee95201244a3b218013c504a6a49a26ff
  //this tx has multiple inputs and outputs
  val rawTransaction = "01000000" +
    "02df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd040011b0000006a473044022040f91c48f4011bf2e2edb6621bfa8fb802241de939cb86f1872c99c580ef0fe402204fc27388bc525e1b655b5f5b35f9d601d28602432dd5672f29e0a47f5b8bbb26012102c114f376c98d12a0540c3a81ab99bb1c5234245c05e8239d09f48229f9ebf011ffffffff" +
    "df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd04001340000006b483045022100cf317c320d078c5b884c44e7488825dab5bcdf3f88c66314ac925770cd8773a7022033fde60d33cc2842ea73fce5d9cf4f8da6fadf414a75b7085efdcd300407f438012102605c23537b27b80157c770cd23e066cd11db3800d3066a38b9b592fc08ae9c70ffffffff" +
    "02c02b00000000000017a914b0b06365c482eb4eabe6e0630029fb8328ea098487e81c0000000000001976a914938da2b50fd6d8acdfa20e30df0e7d8092f0bc7588ac00000000"
  def transaction = Transaction(rawTransaction)

  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawTxInput =
    "85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000" +
      "6f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae" +
      "ffffffff"
  def txInput: TransactionInput = RawTransactionInputParser.read(rawTxInput)

  //simple raw transaction with only one input and two outputs
  //txid 92efdd5abb43efd4fe4f89bd080bcddd287a630e8cb6920388dd7880acf4c964
  val simpleRawTransaction =
    "0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000"
  def simpleTransaction = Transaction(simpleRawTransaction)

  //parent to the 'simpleRawTransaction' val in this file. It is referenced by the input,
  //which needs to have access to this tx to view the scriptPubKey
  //txid b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  val parentSimpleRawTransaction =
    "0100000001cda741646fada7272b900719f7ac9d68d633d0e8aa9501eed3c90afbd323bd65" +
      "010000006a4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d" +
      "158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612" +
      "457b2774509bffffffff026c405d05000000001976a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac809698000000000" +
      "017a914af575bd77c5ce7eba3bd9ce6f89774713ae62c798700000000"
  def parentSimpleTransaction = Transaction(parentSimpleRawTransaction)

  //scriptPubKey taken from https://bitcoin.org/en/developer-reference#raw-transaction-format
  def rawScriptPubKey = rawP2PKHScriptPubKey
  def scriptPubKey = RawScriptPubKeyParser.read(rawScriptPubKey)

  //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
  //ouptut is index 0
  val rawP2PKHScriptPubKey =
    "1976a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac"
  def p2pkhScriptPubKey = ScriptPubKey(rawP2PKHScriptPubKey)

  val rawP2SHScriptPubKey = "a9145780b80be32e117f675d6e0ada13ba799bf248e987"
  def p2shScriptPubKey = ScriptPubKey(rawP2SHScriptPubKey)

  //https://tbtc.blockr.io/api/v1/tx/raw/bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74
  val rawScriptSig =
    "8b483045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f283014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e"
  def scriptSig = ScriptSignature(rawScriptSig)

  def testProgram: PreExecutionScriptProgram = {
    val t = BaseTxSigComponent(
      transaction = TransactionTestUtil.testTransaction,
      inputIndex = UInt32.zero,
      output = TransactionOutput(CurrencyUnits.zero, EmptyScriptPubKey),
      flags = Policy.standardFlags
    )
    PreExecutionScriptProgram(t)
  }

  def testProgramExecutionInProgress =
    testProgram.toExecutionInProgress

  val rawP2PKScriptSig =
    "4847304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b742001"
  def p2pkScriptSig = ScriptSignature(rawP2PKScriptSig)

  val rawP2PKScriptPubKey =
    "43410479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8ac"
  def p2pkScriptPubKey = ScriptPubKey(rawP2PKScriptPubKey)

  /** This is a script sig that doesn't have a signature strictly der encoded
    * Zero-length R is correctly encoded
    */
  def rawScriptSigNotStrictDerEncoded =
    "173014020002107777777777777777777777777777777701"

  def scriptSigNotStrictDerEncoded =
    ScriptSignature(rawScriptSigNotStrictDerEncoded)

  def p2pkhScriptSigNotStrictDerEncoded =
    ScriptSignature.fromAsm(
      List(
        BytesToPushOntoStack(71),
        ScriptConstant("173014020002107777777777777777777777777777777701"),
        BytesToPushOntoStack(33),
        ScriptConstant(
          "02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652")
      ))

  def multiSigScriptPubKeyHex =
    "695221025878e270211662a27181cf4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd050869166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"

}
