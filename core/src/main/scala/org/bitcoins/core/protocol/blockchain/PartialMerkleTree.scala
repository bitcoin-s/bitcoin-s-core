package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util._
import org.bitcoins.crypto.{CryptoUtil, DoubleSha256Digest}
import scodec.bits.BitVector

import scala.annotation.tailrec
import scala.math._

/** Created by chris on 8/7/16.
  * Represents a subset of known txids inside of a [[org.bitcoins.core.protocol.blockchain.Block Block]]
  * in a way that allows recovery of the txids & merkle root
  * without having to store them all explicitly.
  * See
  * [[https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki#partial-merkle-branch-format BIP37]]
  * for more details
  *
  * Encoding procedure:
  * [[https://bitcoin.org/en/developer-reference#creating-a-merkleblock-message "Bitcoin.org: creating a merkleblock message"]]
  * [[https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L78 "Bitcoin Core: merkleblock.cpp"]]
  * Traverse the tree in depth first order, storing a bit for each traversal.
  * This bit signifies if the node is a parent of at least one
  * matched leaf txid (or a matched leaf txid) itself.
  * In case we are the leaf level, or this bit is 0, it's merkle
  * node hash is stored and it's children are not explored any further.
  * Otherwise no hash is stored, but we recurse all of this node's child branches.
  *
  * Decoding procedure:
  * [[https://bitcoin.org/en/developer-reference#parsing-a-merkleblock-message "Bitcoin.org: parsing a merkleblock message"]]
  * [[https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L96 "Bitcoin Core: merkleblock.cpp"]]
  * The same depth first decoding procedure is performed, but we consume the
  * bits and hashes that we used during encoding
  */
sealed trait PartialMerkleTree {

  /** The total number of transactions in this block */
  def transactionCount: UInt32

  /** The actual scala integer representation for `transactionCount` */
  private def numTransactions: Int = transactionCount.toInt

  /** Maximum height of the `tree` */
  private def maxHeight = PartialMerkleTree.calcMaxHeight(numTransactions)

  /** The actual tree used to represent this partial merkle tree */
  def tree: BinaryTreeDoubleSha256Digest

  /** A sequence representing if this node is the parent of another node that matched a txid */
  def bits: BitVector

  /** The hashes used to create the binary tree */
  def hashes: Seq[DoubleSha256Digest]

  /** Extracts the txids that were matched inside of the bloom filter used to create this partial merkle tree */
  def extractMatches: Seq[DoubleSha256Digest] = {
    //TODO: This is some really ugly that isn't tail recursive, try to clean this up eventually
    def loop(
        subTree: BinaryTreeDoubleSha256Digest,
        remainingBits: BitVector,
        height: Int,
        pos: Int,
        accumMatches: Seq[DoubleSha256Digest]): (
        Seq[DoubleSha256Digest],
        BitVector) = {
      if (height == maxHeight)
        extractLeafMatch(accumMatches, remainingBits, subTree)
      else {
        //means we have a nontxid node
        if (remainingBits.head) {
          //means we have a match underneath this node
          subTree match {
            case n: NodeDoubleSha256Digest =>
              //since we are just trying to extract bloom filter matches, recurse into the two subtrees
              val (leftTreeMatches, leftRemainingBits) = loop(
                n.l,
                remainingBits.tail,
                height + 1,
                (2 * pos),
                accumMatches)
              //check to see if we have a right subtree
              if (
                PartialMerkleTree.existsRightSubTree(pos,
                                                     numTransactions,
                                                     maxHeight,
                                                     height)
              ) {
                val (rightTreeMatches, rightRemainingBits) =
                  loop(n.r,
                       leftRemainingBits,
                       height + 1,
                       (2 * pos) + 1,
                       leftTreeMatches)
                (rightTreeMatches, rightRemainingBits)
              } else (leftTreeMatches, leftRemainingBits)
            case _: LeafDoubleSha256Digest =>
              (accumMatches, remainingBits.tail)
            case EmptyTreeDoubleSha256Digest =>
              throw new IllegalArgumentException(
                "We cannot have an empty node when we supposedly have a match underneath this node since it has no children")
          }
        } else (accumMatches, remainingBits.tail)
      }
    }
    val (matches, remainingBits) = loop(tree, bits, 0, 0, Nil)
    require(
      PartialMerkleTree.usedAllBits(bits, remainingBits),
      "We should not have any remaining matches " +
        "except for those that pad our byte after building our partial merkle tree, got: " + remainingBits
    )
    matches.reverse
  }

  /** Handles a leaf match when we are extracting matches from the partial merkle tree */
  private def extractLeafMatch(
      accumMatches: Seq[DoubleSha256Digest],
      remainingBits: BitVector,
      subTree: BinaryTreeDoubleSha256Digest): (
      Seq[DoubleSha256Digest],
      BitVector) = {
    if (remainingBits.head) {
      //means we have a txid node that matched the filter
      subTree match {
        case l: LeafDoubleSha256Digest =>
          val newAccumMatches = l.v +: accumMatches
          (newAccumMatches, remainingBits.tail)
        case x @ (_: NodeDoubleSha256Digest | EmptyTreeDoubleSha256Digest) =>
          throw new IllegalArgumentException("We cannot have a " +
            "Node or Empty node when we supposedly have a txid node -- txid nodes should always be leaves, got: " + x)
      }
    } else {
      //means we have a txid node, but it did not match the filter
      (accumMatches, remainingBits.tail)
    }
  }
}

object PartialMerkleTree {

  private case class PartialMerkleTreeImpl(
      tree: BinaryTreeDoubleSha256Digest,
      transactionCount: UInt32,
      bits: BitVector,
      hashes: Seq[DoubleSha256Digest])
      extends PartialMerkleTree {
    require(bits.size % 8 == 0,
            "As per BIP37, bits must be padded to the nearest byte")
  }

  def apply(
      txMatches: Seq[(Boolean, DoubleSha256Digest)]): PartialMerkleTree = {
    val txIds = txMatches.map(_._2)
    val (bits, hashes) = build(txMatches)
    val tree = reconstruct(txIds.size, hashes, bits)
    PartialMerkleTree(tree, UInt32(txIds.size), bits, hashes)
  }

  /** @param txMatches indicates whether the given txid matches the bloom filter, the full merkle branch needs
    *                  to be included inside of the
    *                  [[org.bitcoins.core.protocol.blockchain.PartialMerkleTree PartialMerkleTree]]
    * @return the binary tree that represents the partial merkle tree, the bits needed to reconstruct this partial
    *         merkle tree, and the hashes needed to be inserted
    *         according to the flags inside of bits
    */
  private def build(txMatches: Seq[(Boolean, DoubleSha256Digest)]): (
      BitVector,
      Seq[DoubleSha256Digest]) = {
    val maxHeight = calcMaxHeight(txMatches.size)

    /** This loops through our merkle tree building `bits` so we can instruct another node how to create the partial merkle tree
      * @see [[https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L78]]
      * @param bits the accumulator for bits indicating how to reconsctruct the partial merkle tree
      * @param hashes the relevant hashes used with bits to reconstruct the merkle tree
      * @param height the transaction index we are currently looking at -- if it was matched in our bloom filter we need the entire merkle branch
      * @param pos the position in the loop represented as an int
      * @return the binary tree that represents the partial merkle tree, the bits needed to reconstruct this partial merkle tree, and the hashes needed to be inserted
      *         according to the flags inside of bits
      */
    def loop(
        bits: BitVector,
        hashes: Seq[DoubleSha256Digest],
        height: Int,
        pos: Int): (BitVector, Seq[DoubleSha256Digest]) = {
      val parentOfMatch =
        matchesTx(maxHeight, maxHeight - height, pos, txMatches)
      val newBits = parentOfMatch +: bits
      if (height == 0 || !parentOfMatch) {
        //means that we are either at the root of the merkle tree or there is nothing interesting below
        //this node in our binary tree
        val nodeHash = calcHash(height, pos, txMatches.map(_._2))
        val newHashes = nodeHash +: hashes
        (newBits, newHashes)
      } else {
        //process the left node
        val (leftBits, leftHashes) = loop(newBits, hashes, height - 1, pos * 2)
        if (existsRightSubTree(pos, txMatches.size, height)) {
          //process the right node if the tree's width is larger than the position we are looking at
          loop(leftBits, leftHashes, height - 1, (pos * 2) + 1)
        } else (leftBits, leftHashes)
      }
    }
    val (bits, hashes) = loop(BitVector.empty, Nil, maxHeight, 0)
    //pad the bit array to the nearest byte as required by BIP37
    val bitsNeeded =
      if (bits.size % 8 == 0) 0 else (8 - (bits.size % 8)) + bits.size
    val paddedBits = if (bitsNeeded == 0) bits else bits.padLeft(bitsNeeded)
    (paddedBits.reverse, hashes.reverse)
  }

  /** Checks if a node at given the given height and position matches a transaction in the sequence */
  def matchesTx(
      maxHeight: Int,
      height: Int,
      pos: Int,
      matchedTx: Seq[(Boolean, DoubleSha256Digest)]): Boolean = {
    //mimics this functionality inside of bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L78
    val inverseHeight = maxHeight - height
    @tailrec
    def loop(p: Int): Boolean = {
      if ((p < ((pos + 1) << inverseHeight)) && p < matchedTx.size) {
        if (matchedTx(p)._1) return true
        else loop(p + 1)
      } else false
    }
    val startingPos = pos << inverseHeight
    loop(startingPos)
  }

  /** Simple way to calculate the maximum width of a binary tree */
  private def calcTreeWidth(numTransactions: Int, height: Int) =
    (numTransactions + (1 << height) - 1) >> height

  /** Calculates the hash of a node in the merkle tree */
  private def calcHash(
      height: Int,
      pos: Int,
      txIds: Seq[DoubleSha256Digest]): DoubleSha256Digest = {
    //TODO: Optimize this to tailrec function
    //follows this function inside of bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/master/src/merkleblock.cpp#L63
    if (height == 0) txIds(pos)
    else {
      val leftHash = calcHash(height - 1, pos * 2, txIds)
      val rightHash = if (existsRightSubTree(pos, txIds.size, height)) {
        calcHash(height - 1, (pos * 2) + 1, txIds)
      } else leftHash
      CryptoUtil.doubleSHA256(leftHash.bytes ++ rightHash.bytes)
    }
  }

  /** Function to reconstruct a partial merkle tree
    * @param transactionCount the number of transactions inside of the partial merkle tree
    * @param hashes the hashes used to reconstruct the partial merkle tree
    * @param bits the bits used indicate the structure of the partial merkle tree
    * @return
    */
  def apply(
      transactionCount: UInt32,
      hashes: Seq[DoubleSha256Digest],
      bits: BitVector): PartialMerkleTree = {
    val tree = reconstruct(transactionCount.toInt, hashes, bits)
    PartialMerkleTree(tree, transactionCount, bits, hashes)
  }

  /** This constructor creates a partial from this given [[org.bitcoins.core.util.BinaryTree BinaryTree]]
    * You probably don't want to use this constructor, unless you manually constructed `bits` and the `tree`
    * by hand
    * @param tree the partial merkle tree -- note this is NOT the full merkle tree
    * @param transactionCount the number of transactions there initially was in the full merkle tree
    * @param bits the path to the matches in the partial merkle tree
    * @param hashes the hashes used to reconstruct the binary tree according to `bits`
    */
  def apply(
      tree: BinaryTreeDoubleSha256Digest,
      transactionCount: UInt32,
      bits: BitVector,
      hashes: Seq[DoubleSha256Digest]): PartialMerkleTree = {
    PartialMerkleTreeImpl(tree, transactionCount, bits, hashes)
  }

  /** Builds a partial merkle tree
    * [[https://bitcoin.org/en/developer-reference#parsing-a-merkleblock-message]]
    * [[https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L96]]
    */
  private def reconstruct(
      numTransaction: Int,
      hashes: Seq[DoubleSha256Digest],
      bits: BitVector): BinaryTreeDoubleSha256Digest = {
    val maxHeight = calcMaxHeight(numTransaction)
    //TODO: Optimize to tailrec function
    def loop(
        remainingHashes: Seq[DoubleSha256Digest],
        remainingMatches: BitVector,
        height: Int,
        pos: Int): (
        BinaryTreeDoubleSha256Digest,
        Seq[DoubleSha256Digest],
        BitVector) = {
      if (height == maxHeight) {
        //means we have a txid node
        (LeafDoubleSha256Digest(remainingHashes.head),
         remainingHashes.tail,
         remainingMatches.tail)
      } else {
        //means we have a non txid node
        if (remainingMatches.head) {
          val nextHeight = height + 1
          val leftNodePos = pos * 2
          val rightNodePos = (pos * 2) + 1
          val (leftNode, leftRemainingHashes, leftRemainingBits) = loop(
            remainingHashes,
            remainingMatches.tail,
            nextHeight,
            leftNodePos)
          val (rightNode, rightRemainingHashes, rightRemainingBits) = {
            if (existsRightSubTree(pos, numTransaction, maxHeight, height)) {
              val (rightNode, rightRemainingHashes, rightRemainingBits) =
                loop(leftRemainingHashes,
                     leftRemainingBits,
                     nextHeight,
                     rightNodePos)
              //https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L121-L125
              if (nextHeight != maxHeight) {
                //we cannot the same two hashes as child nodes UNLESS we are the max height in the binary tree
                require(
                  leftNode.value.get != rightNode.value.get,
                  "Cannot have the same hashes in two child nodes, got: " + leftNode +
                    ", \nleftRemainingHashes: " + leftRemainingHashes + " \nrightRemainingHashes: " + rightRemainingHashes +
                    "\nnumTransactions: " + numTransaction + "\nhashes: " + hashes + "\nbits: " + bits
                )
              }
              (rightNode, rightRemainingHashes, rightRemainingBits)
            } else (leftNode, leftRemainingHashes, leftRemainingBits)
          }
          val nodeHash = CryptoUtil.doubleSHA256(
            leftNode.value.get.bytes ++ rightNode.value.get.bytes)
          val node = NodeDoubleSha256Digest(nodeHash, leftNode, rightNode)
          (node, rightRemainingHashes, rightRemainingBits)
        } else {
          (LeafDoubleSha256Digest(remainingHashes.head),
           remainingHashes.tail,
           remainingMatches.tail)
        }
      }
    }
    val (tree, remainingHashes, remainingBits) = loop(hashes, bits, 0, 0)

    //require(remainingBits.isEmpty, s"Remainging bits should be empty, got ${remainingBits}")
    //we must have used all the hashes provided to us to reconstruct the partial merkle tree as per BIP37
    require(
      remainingHashes.isEmpty,
      "We should not have any left over hashes after building our partial merkle tree, got: " + remainingHashes)
    //we must not have any matches remaining, unless the remaining bits were use to pad our byte vector to 8 bits
    //for instance, we could have had 5 bits to indicate how to build the merkle tree, but we need to pad it with 3 bits
    //to give us a full byte to serialize and send over the network
    //https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L174-L175
    require(
      usedAllBits(bits, remainingBits),
      "We should not have any remaining matches except for those that pad our byte after building our partial merkle tree, got: " + remainingBits
    )
    tree
  }

  /** Calculates the maximum height for a binary tree with the number of transactions specified */
  def calcMaxHeight(numTransactions: Int): Int =
    Math.ceil((log(numTransactions) / log(2))).toInt

  /** Determines if the right sub tree can exists inside of the partial merkle tree
    * This function should only be used to determine if a right sub tree exists when we
    * are building a partial merkle tree from bottom up, NOT TOP DOWN. If we are building a
    * tree from top down use it's counterpart that does NOT take a maxHeight parameter
    */
  private def existsRightSubTree(
      pos: Int,
      numTransaction: Int,
      maxHeight: Int,
      height: Int): Boolean = {
    (pos * 2) + 1 < calcTreeWidth(numTransaction, maxHeight - height - 1)
  }

  /** Determines if the right sub tree can exist inside of the partial merkle tree */
  private def existsRightSubTree(
      pos: Int,
      numTransaction: Int,
      height: Int): Boolean = {
    (pos * 2) + 1 < calcTreeWidth(numTransaction, height - 1)
  }

  /** Enforces the invariant inside of bitcoin core saying we must use all bits
    * in a byte array when reconstruction a partial merkle tree
    * [[https://github.com/bitcoin/bitcoin/blob/b7b48c8bbdf7a90861610b035d8b0a247ef78c45/src/merkleblock.cpp#L174-L175]]
    */
  private def usedAllBits(
      bits: BitVector,
      remainingBits: BitVector): Boolean = {
    val bitsUsed = bits.size - remainingBits.size
    ((bitsUsed + 7) / 8) == ((bits.size + 7) / 8)
  }
}
