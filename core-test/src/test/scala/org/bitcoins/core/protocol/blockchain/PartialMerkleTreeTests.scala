package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.bloom.{BloomFilter, BloomUpdateAll}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.{
  BytesUtil,
  LeafDoubleSha256Digest,
  NodeDoubleSha256Digest
}
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkitcore.gen.MerkleGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.BitVector

/** Created by chris on 8/9/16.
  */
class PartialMerkleTreeTests extends BitcoinSUnitTest {
  "PartialMerkleTree" must "from a list of txs and a bit indicating if the tx matched the filter" in {
    // https://github.com/bitcoin/bitcoin/blob/master/src/test/bloom_tests.cpp#L185
    val block = Block(
      "0100000090f0a9f110702f808219ebea1173056042a714bad51b916cb68000000000000052752895" +
        "58f51c9966699404ae2294730c3c9f9bda53523ce50e9b95e558da2fdb261b4d4c86041b1ab1bf930901000000010000" +
        "000000000000000000000000000000000000000000000000000000000000ffffffff07044c86041b0146ffffffff0100" +
        "f2052a01000000434104e18f7afbe4721580e81e8414fc8c24d7cfacf254bb5c7b949450c3e997c2dc1242487a816950" +
        "7b631eb3771f2b425483fb13102c4eb5d858eef260fe70fbfae0ac00000000010000000196608ccbafa16abada902780" +
        "da4dc35dafd7af05fa0da08cf833575f8cf9e836000000004a493046022100dab24889213caf43ae6adc41cf1c9396c0" +
        "8240c199f5225acf45416330fd7dbd022100fe37900e0644bf574493a07fc5edba06dbc07c311b947520c2d514bc5725" +
        "dcb401ffffffff0100f2052a010000001976a914f15d1921f52e4007b146dfa60f369ed2fc393ce288ac000000000100" +
        "000001fb766c1288458c2bafcfec81e48b24d98ec706de6b8af7c4e3c29419bfacb56d000000008c493046022100f268" +
        "ba165ce0ad2e6d93f089cfcd3785de5c963bb5ea6b8c1b23f1ce3e517b9f022100da7c0f21adc6c401887f2bfd1922f1" +
        "1d76159cbc597fbd756a23dcbb00f4d7290141042b4e8625a96127826915a5b109852636ad0da753c9e1d5606a50480c" +
        "d0c40f1f8b8d898235e571fe9357d9ec842bc4bba1827daaf4de06d71844d0057707966affffffff0280969800000000" +
        "001976a9146963907531db72d0ed1a0cfb471ccb63923446f388ac80d6e34c000000001976a914f0688ba1c0d1ce182c" +
        "7af6741e02658c7d4dfcd388ac000000000100000002c40297f730dd7b5a99567eb8d27b78758f607507c52292d02d40" +
        "31895b52f2ff010000008b483045022100f7edfd4b0aac404e5bab4fd3889e0c6c41aa8d0e6fa122316f68eddd0a6501" +
        "3902205b09cc8b2d56e1cd1f7f2fafd60a129ed94504c4ac7bdc67b56fe67512658b3e014104732012cb962afa90d31b" +
        "25d8fb0e32c94e513ab7a17805c14ca4c3423e18b4fb5d0e676841733cb83abaf975845c9f6f2a8097b7d04f4908b183" +
        "68d6fc2d68ecffffffffca5065ff9617cbcba45eb23726df6498a9b9cafed4f54cbab9d227b0035ddefb000000008a47" +
        "3044022068010362a13c7f9919fa832b2dee4e788f61f6f5d344a7c2a0da6ae740605658022006d1af525b9a14a35c00" +
        "3b78b72bd59738cd676f845d1ff3fc25049e01003614014104732012cb962afa90d31b25d8fb0e32c94e513ab7a17805" +
        "c14ca4c3423e18b4fb5d0e676841733cb83abaf975845c9f6f2a8097b7d04f4908b18368d6fc2d68ecffffffff01001e" +
        "c4110200000043410469ab4181eceb28985b9b4e895c13fa5e68d85761b7eee311db5addef76fa8621865134a221bd01" +
        "f28ec9999ee3e021e60766e9d1f3458c115fb28650605f11c9ac000000000100000001cdaf2f758e91c514655e2dc506" +
        "33d1e4c84989f8aa90a0dbc883f0d23ed5c2fa010000008b48304502207ab51be6f12a1962ba0aaaf24a20e0b69b27a9" +
        "4fac5adf45aa7d2d18ffd9236102210086ae728b370e5329eead9accd880d0cb070aea0c96255fae6c4f1ddcce1fd56e" +
        "014104462e76fd4067b3a0aa42070082dcb0bf2f388b6495cf33d789904f07d0f55c40fbd4b82963c69b3dc31895d0c7" +
        "72c812b1d5fbcade15312ef1c0e8ebbb12dcd4ffffffff02404b4c00000000001976a9142b6ba7c9d796b75eef7942fc" +
        "9288edd37c32f5c388ac002d3101000000001976a9141befba0cdc1ad56529371864d9f6cb042faa06b588ac00000000" +
        "0100000001b4a47603e71b61bc3326efd90111bf02d2f549b067f4c4a8fa183b57a0f800cb010000008a4730440220177c37f9a505c3f1a1f0ce2da777c339bd8339ffa02c7cb41f0a5804f473c9230220585b25a2ee80eb59292e52b987dad92acb0c64eced92ed9ee105ad153cdb12d001410443bd44f683467e549dae7d20d1d79cbdb6df985c6e9c029c8d0c6cb46cc1a4d3cf7923c5021b27f7a0b562ada113bc85d5fda5a1b41e87fe6e8802817cf69996ffffffff0280651406000000001976a9145505614859643ab7b547cd7f1f5e7e2a12322d3788ac00aa0271000000001976a914ea4720a7a52fc166c55ff2298e07baf70ae67e1b88ac00000000010000000586c62cd602d219bb60edb14a3e204de0705176f9022fe49a538054fb14abb49e010000008c493046022100f2bc2aba2534becbdf062eb993853a42bbbc282083d0daf9b4b585bd401aa8c9022100b1d7fd7ee0b95600db8535bbf331b19eed8d961f7a8e54159c53675d5f69df8c014104462e76fd4067b3a0aa42070082dcb0bf2f388b6495cf33d789904f07d0f55c40fbd4b82963c69b3dc31895d0c772c812b1d5fbcade15312ef1c0e8ebbb12dcd4ffffffff03ad0e58ccdac3df9dc28a218bcf6f1997b0a93306faaa4b3a28ae83447b2179010000008b483045022100be12b2937179da88599e27bb31c3525097a07cdb52422d165b3ca2f2020ffcf702200971b51f853a53d644ebae9ec8f3512e442b1bcb6c315a5b491d119d10624c83014104462e76fd4067b3a0aa42070082dcb0bf2f388b6495cf33d789904f07d0f55c40fbd4b82963c69b3dc31895d0c772c812b1d5fbcade15312ef1c0e8ebbb12dcd4ffffffff2acfcab629bbc8685792603762c921580030ba144af553d271716a95089e107b010000008b483045022100fa579a840ac258871365dd48cd7552f96c8eea69bd00d84f05b283a0dab311e102207e3c0ee9234814cfbb1b659b83671618f45abc1326b9edcc77d552a4f2a805c0014104462e76fd4067b3a0aa42070082dcb0bf2f388b6495cf33d789904f07d0f55c40fbd4b82963c69b3dc31895d0c772c812b1d5fbcade15312ef1c0e8ebbb12dcd4ffffffffdcdc6023bbc9944a658ddc588e61eacb737ddf0a3cd24f113b5a8634c517fcd2000000008b4830450221008d6df731df5d32267954bd7d2dda2302b74c6c2a6aa5c0ca64ecbabc1af03c75022010e55c571d65da7701ae2da1956c442df81bbf076cdbac25133f99d98a9ed34c014104462e76fd4067b3a0aa42070082dcb0bf2f388b6495cf33d789904f07d0f55c40fbd4b82963c69b3dc31895d0c772c812b1d5fbcade15312ef1c0e8ebbb12dcd4ffffffffe15557cd5ce258f479dfd6dc6514edf6d7ed5b21fcfa4a038fd69f06b83ac76e010000008b483045022023b3e0ab071eb11de2eb1cc3a67261b866f86bf6867d4558165f7c8c8aca2d86022100dc6e1f53a91de3efe8f63512850811f26284b62f850c70ca73ed5de8771fb451014104462e76fd4067b3a0aa42070082dcb0bf2f388b6495cf33d789904f07d0f55c40fbd4b82963c69b3dc31895d0c772c812b1d5fbcade15312ef1c0e8ebbb12dcd4ffffffff01404b4c00000000001976a9142b6ba7c9d796b75eef7942fc9288edd37c32f5c388ac00000000010000000166d7577163c932b4f9690ca6a80b6e4eb001f0a2fa9023df5595602aae96ed8d000000008a4730440220262b42546302dfb654a229cefc86432b89628ff259dc87edd1154535b16a67e102207b4634c020a97c3e7bbd0d4d19da6aa2269ad9dded4026e896b213d73ca4b63f014104979b82d02226b3a4597523845754d44f13639e3bf2df5e82c6aab2bdc79687368b01b1ab8b19875ae3c90d661a3d0a33161dab29934edeb36aa01976be3baf8affffffff02404b4c00000000001976a9144854e695a02af0aeacb823ccbc272134561e0a1688ac40420f00000000001976a914abee93376d6b37b5c2940655a6fcaf1c8e74237988ac0000000001000000014e3f8ef2e91349a9059cb4f01e54ab2597c1387161d3da89919f7ea6acdbb371010000008c49304602210081f3183471a5ca22307c0800226f3ef9c353069e0773ac76bb580654d56aa523022100d4c56465bdc069060846f4fbf2f6b20520b2a80b08b168b31e66ddb9c694e240014104976c79848e18251612f8940875b2b08d06e6dc73b9840e8860c066b7e87432c477e9a59a453e71e6d76d5fe34058b800a098fc1740ce3012e8fc8a00c96af966ffffffff02c0e1e400000000001976a9144134e75a6fcb6042034aab5e18570cf1f844f54788ac404b4c00000000001976a9142b6ba7c9d796b75eef7942fc9288edd37c32f5c388ac00000000"
    )

    val hash1 = DoubleSha256Digest(
      BytesUtil.flipEndianness(
        "74d681e0e03bafa802c8aa084379aa98d9fcd632ddc2ed9782b586ec87451f20"
      )
    )
    val hash2 = DoubleSha256Digest(
      BytesUtil.flipEndianness(
        "dd1fd2a6fc16404faf339881a90adbde7f4f728691ac62e8f168809cdfae1053"
      )
    )

    // 5ef2374cf1cd1c0b8c1b795950c077fc571cca44866c375a1ed17ace665cfaaa
    // d403489f6b1a2f7de72c6e4573e1cc7ac15745518e42a0bf884f58dc48f45533
    // 8719e60a59869e70a7a7a5d4ff6ceb979cd5abe60721d4402aaf365719ebd221
    // 5310aedf9c8068f1e862ac9186724f7fdedb0aa9819833af4f4016fca6d21fdd
    // 201f4587ec86b58297edc2dd32d6fcd998aa794308aac802a8af3be0e081d674

    val filter = BloomFilter(10, 0.000001, UInt32.zero, BloomUpdateAll)
      .insert(hash1)
      .insert(hash2)
    val (merkleBlock, _) = MerkleBlock(block, filter)
    val partialMerkleTree = merkleBlock.partialMerkleTree
    partialMerkleTree.bits.slice(0, 8).toIndexedSeq must be(
      Seq(true, true, false, true, false, true, false, true)
    )
    partialMerkleTree.bits
      .slice(8, partialMerkleTree.bits.size)
      .toIndexedSeq must be(
      Seq(true, true, true, true, false, false, false, false)
    )
    partialMerkleTree.hashes must be(
      Seq(
        DoubleSha256Digest(
          "5ef2374cf1cd1c0b8c1b795950c077fc571cca44866c375a1ed17ace665cfaaa"
        ),
        DoubleSha256Digest(
          "d403489f6b1a2f7de72c6e4573e1cc7ac15745518e42a0bf884f58dc48f45533"
        ),
        DoubleSha256Digest(
          "8719e60a59869e70a7a7a5d4ff6ceb979cd5abe60721d4402aaf365719ebd221"
        ),
        DoubleSha256Digest(
          "5310aedf9c8068f1e862ac9186724f7fdedb0aa9819833af4f4016fca6d21fdd"
        ),
        DoubleSha256Digest(
          "201f4587ec86b58297edc2dd32d6fcd998aa794308aac802a8af3be0e081d674"
        )
      )
    )

    partialMerkleTree.extractMatches must be(Seq(hash2, hash1))

  }

  it must "detect if a node at a given height and position matches a tx that the bloom filter matched" in {
    // these values are related to this test case for merkle block inside of core
    // https://github.com/bitcoin/bitcoin/blob/f17032f703288d43a76cffe8fa89b87ade9e3074/src/test/bloom_tests.cpp#L185
    val maxHeight = 4
    val matchedTxs = List(
      (
        false,
        DoubleSha256Digest(
          "a3f3ac605d5e4727f4ea72e9346a5d586f0231460fd52ad9895bc8240d871def"
        )
      ),
      (
        false,
        DoubleSha256Digest(
          "076d0317ee70ee36cf396a9871ab3bf6f8e6d538d7f8a9062437dcb71c75fcf9"
        )
      ),
      (
        false,
        DoubleSha256Digest(
          "2ee1e12587e497ada70d9bd10d31e83f0a924825b96cb8d04e8936d793fb60db"
        )
      ),
      (
        false,
        DoubleSha256Digest(
          "7ad8b910d0c7ba2369bc7f18bb53d80e1869ba2c32274996cebe1ae264bc0e22"
        )
      ),
      (
        false,
        DoubleSha256Digest(
          "4e3f8ef2e91349a9059cb4f01e54ab2597c1387161d3da89919f7ea6acdbb371"
        )
      ),
      (
        false,
        DoubleSha256Digest(
          "e0c28dbf9f266a8997e1a02ef44af3a1ee48202253d86161d71282d01e5e30fe"
        )
      ),
      (
        false,
        DoubleSha256Digest(
          "8719e60a59869e70a7a7a5d4ff6ceb979cd5abe60721d4402aaf365719ebd221"
        )
      ),
      (
        true,
        DoubleSha256Digest(
          "5310aedf9c8068f1e862ac9186724f7fdedb0aa9819833af4f4016fca6d21fdd"
        )
      ),
      (
        true,
        DoubleSha256Digest(
          "201f4587ec86b58297edc2dd32d6fcd998aa794308aac802a8af3be0e081d674"
        )
      )
    )

    // it must trivially match the merkle root
    PartialMerkleTree.matchesTx(maxHeight, 0, 0, matchedTxs) must be(true)

    // it must match both of the child nodes to the merkle root
    PartialMerkleTree.matchesTx(maxHeight, 1, 0, matchedTxs) must be(true)
    PartialMerkleTree.matchesTx(maxHeight, 1, 1, matchedTxs) must be(true)

    // it must match the 2nd and 3rd grandchild of the merkle root, and must NOT match the 1st and 4th grand child
    PartialMerkleTree.matchesTx(maxHeight, 2, 0, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 2, 1, matchedTxs) must be(true)

    PartialMerkleTree.matchesTx(maxHeight, 2, 2, matchedTxs) must be(true)
    PartialMerkleTree.matchesTx(maxHeight, 2, 3, matchedTxs) must be(false)

    // it must match the match the 4th and 5th great grand child merkle root, an must NOT  match the 1st,2nd,3rd,6th
    PartialMerkleTree.matchesTx(maxHeight, 3, 0, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 3, 1, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 3, 2, matchedTxs) must be(false)

    PartialMerkleTree.matchesTx(maxHeight, 3, 3, matchedTxs) must be(true)
    PartialMerkleTree.matchesTx(maxHeight, 3, 4, matchedTxs) must be(true)

    PartialMerkleTree.matchesTx(maxHeight, 3, 5, matchedTxs) must be(false)

    // it must match the correct leaf nodes (great-great-grand-children), which are indexes 7 and 8
    PartialMerkleTree.matchesTx(maxHeight, 4, 0, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 4, 1, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 4, 2, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 4, 3, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 4, 4, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 4, 5, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 4, 6, matchedTxs) must be(false)

    PartialMerkleTree.matchesTx(maxHeight, 4, 7, matchedTxs) must be(true)
    PartialMerkleTree.matchesTx(maxHeight, 4, 8, matchedTxs) must be(true)

  }

  it must "match no transactions if we give a list of no transactions" in {
    val maxHeight = 1
    val matchedTxs = Seq(
      (
        false,
        DoubleSha256Digest(
          "a3f3ac605d5e4727f4ea72e9346a5d586f0231460fd52ad9895bc8240d871def"
        )
      ),
      (
        false,
        DoubleSha256Digest(
          "076d0317ee70ee36cf396a9871ab3bf6f8e6d538d7f8a9062437dcb71c75fcf9"
        )
      )
    )
    PartialMerkleTree.matchesTx(maxHeight, 0, 0, matchedTxs) must be(false)

    PartialMerkleTree.matchesTx(maxHeight, 1, 0, matchedTxs) must be(false)
    PartialMerkleTree.matchesTx(maxHeight, 1, 1, matchedTxs) must be(false)
  }

  it must "build a partial merkle tree with no matches and 1 transaction in the original block" in {
    val txMatches = Vector(
      (
        false,
        DoubleSha256Digest(
          "01272b2b1c8c33a1b4e9ab111db41c9ac275e686fbd9c5d482e586d03e9e0552"
        )
      )
    )
    val partialMerkleTree = PartialMerkleTree(txMatches)
    partialMerkleTree.bits.toIndexedSeq must be(
      Seq(false, false, false, false, false, false, false, false)
    )
    partialMerkleTree.tree must be(
      LeafDoubleSha256Digest(
        DoubleSha256Digest(
          "01272b2b1c8c33a1b4e9ab111db41c9ac275e686fbd9c5d482e586d03e9e0552"
        )
      )
    )
    partialMerkleTree.transactionCount must be(UInt32(1))
    partialMerkleTree.extractMatches.isEmpty must be(true)
  }

  it must "build a partial merkle tree with 1 match and 2 transactions inside the original block" in {
    val txMatches = Vector(
      (
        false,
        DoubleSha256Digest(
          "01272b2b1c8c33a1b4e9ab111db41c9ac275e686fbd9c5d482e586d03e9e0552"
        )
      ),
      (
        true,
        DoubleSha256Digest(
          "076d0317ee70ee36cf396a9871ab3bf6f8e6d538d7f8a9062437dcb71c75fcf9"
        )
      )
    )
    val partialMerkleTree = PartialMerkleTree(txMatches)
    partialMerkleTree.bits.toIndexedSeq must be(
      Seq(true, false, true, false, false, false, false, false)
    )
    partialMerkleTree.tree must be(
      NodeDoubleSha256Digest(
        DoubleSha256Digest(
          "b130d701e65ac8c65f30dc4b20aabf349036b7c87f11f012f4f3f53f666791e6"
        ),
        LeafDoubleSha256Digest(
          DoubleSha256Digest(
            "01272b2b1c8c33a1b4e9ab111db41c9ac275e686fbd9c5d482e586d03e9e0552"
          )
        ),
        LeafDoubleSha256Digest(
          DoubleSha256Digest(
            "076d0317ee70ee36cf396a9871ab3bf6f8e6d538d7f8a9062437dcb71c75fcf9"
          )
        )
      )
    )
    partialMerkleTree.extractMatches must be(
      Seq(
        DoubleSha256Digest(
          "076d0317ee70ee36cf396a9871ab3bf6f8e6d538d7f8a9062437dcb71c75fcf9"
        )
      )
    )
  }

  it must "calculate bits correctly for a tree of height 1" in {
    val matches = Vector(
      (
        true,
        DoubleSha256Digest(
          "caa02f1194fb44dea407a7cf713ddcf30e69f49c297f9275f9236fec42d945b2"
        )
      )
    )
    val partialMerkleTree = PartialMerkleTree(matches)
    partialMerkleTree.tree must be(
      LeafDoubleSha256Digest(
        DoubleSha256Digest(
          "caa02f1194fb44dea407a7cf713ddcf30e69f49c297f9275f9236fec42d945b2"
        )
      )
    )
    partialMerkleTree.bits.toIndexedSeq must be(
      Seq(true, false, false, false, false, false, false, false)
    )
  }

  it must "correctly compute a merkle tree that has an odd amount of txids on the merkle tree" in {
    // this test is meant to prevent these failures on travis ci
    // https://travis-ci.org/bitcoin-s/bitcoin-s-core/builds/205812075#L2774
    val hashes: Vector[DoubleSha256Digest] = Vector(
      DoubleSha256Digest(
        "1563b82f187da1067f5000dabe3a4f4ae8650e207aa163e1d25ded8175e2bae1"
      ),
      DoubleSha256Digest(
        "151cfc67334a38be8abdb5752f2346f8989c33336275d385b5c61af3edfa0a51"
      ),
      DoubleSha256Digest(
        "f3309f2f4697a95be63e61eae9946b9f9e143909faafeec6fdda01284f5192f8"
      ),
      DoubleSha256Digest(
        "0b38edce452a4ea17b5d76346c9e81ad03e9ecaa887d9a76edeb4da807ce8439"
      ),
      DoubleSha256Digest(
        "ae2caf79fc55e3e887ddb39bcd7f8436f0423e1e8f1f37872ee84779ebdf28c1"
      ),
      DoubleSha256Digest(
        "4c5db2bdd85cef0a37fe4e675b0c342c9b92df90fabe19c5258aa164bdcfddbc"
      ),
      DoubleSha256Digest(
        "ec35eda50230cef0b04be44a070b3fcdbf1766f512cd0432f26a34df6249002a"
      ),
      DoubleSha256Digest(
        "aee82970ace34438dc1d0f3655c62b60c5a28d5754a7d9903706f841fe760dcf"
      ),
      DoubleSha256Digest(
        "584a8cd477057c6ddd3f17be8d47e02210e26a48ef77f3a35bd46903d29b47f8"
      ),
      DoubleSha256Digest(
        "0b55b03bde4e82068f869f4f7f9560fb987f14c15b45b19eff352957ea7a5101"
      ),
      DoubleSha256Digest(
        "f185de9349b800e635ec1dfa06fe0be3ae7cd694f2dbaeeff2d39c58dbebcfd2"
      ),
      DoubleSha256Digest(
        "023175e3a4f7fa993b45a9ca3604b17835866e6af1aff740fa28bae82b8bc71e"
      ),
      DoubleSha256Digest(
        "be6ce29d3dd0f7f777f188b94a5ae514bd763a5a0ff81055fea090596d3acc0b"
      ),
      DoubleSha256Digest(
        "7f1f476304d88a844ff5f5ae2dc0b17e704aa560619fba3753744ca03dea029b"
      ),
      DoubleSha256Digest(
        "9d6453d3573dfadbf93532953ef3e63f4731fc9543705af9988bb8038f060e30"
      ),
      DoubleSha256Digest(
        "9d19dd76defcb7bf991ad62e3a468423ddca09ff3181b8f03d57dbc4e25a1596"
      ),
      DoubleSha256Digest(
        "5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456"
      ),
      DoubleSha256Digest(
        "5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456"
      )
    )
    val numTransactions = UInt32(20)
    val bits = BitVector.bits(
      List(true, true, true, true, true, true, true, false, true, true, false,
           true, true, true, false, true, true, true, true, false, true, true,
           true, true, true, true, true, false, true, true, true, true, false,
           true, true, true, true, false, false, false)
    )
    val tree = PartialMerkleTree(numTransactions, hashes, bits)
    tree.extractMatches.contains(
      DoubleSha256Digest(
        "5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456"
      )
    ) must be(true)
  }

  it must "be able to extract all of the txids we indicated to be matches" in {
    forAll(MerkleGenerator.partialMerkleTree) {
      case (
            partialMerkleTree: PartialMerkleTree,
            txMatches: Seq[(Boolean, DoubleSha256Digest)]
          ) =>
        val matchedTxs = txMatches.filter(_._1).map(_._2)
        assert(partialMerkleTree.extractMatches == matchedTxs)
    }
  }

  it must "generate the same partial merkle tree from the same parameters" in {
    forAll(MerkleGenerator.partialMerkleTree) {
      case (partialMerkleTree: PartialMerkleTree, _) =>
        val partialMerkleTree2 =
          PartialMerkleTree(
            partialMerkleTree.transactionCount,
            partialMerkleTree.hashes,
            partialMerkleTree.bits
          )
        assert(partialMerkleTree2 == partialMerkleTree)
    }
  }
}
