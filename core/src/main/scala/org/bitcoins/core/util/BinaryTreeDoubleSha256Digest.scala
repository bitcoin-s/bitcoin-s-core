package org.bitcoins.core.util

import org.bitcoins.crypto.DoubleSha256Digest

sealed trait BinaryTreeDoubleSha256Digest extends BinaryTree[DoubleSha256Digest]

case class NodeDoubleSha256Digest(
    v: DoubleSha256Digest,
    l: BinaryTreeDoubleSha256Digest,
    r: BinaryTreeDoubleSha256Digest)
    extends BinaryTreeDoubleSha256Digest

case class LeafDoubleSha256Digest(v: DoubleSha256Digest)
    extends BinaryTreeDoubleSha256Digest

case object EmptyTreeDoubleSha256Digest extends BinaryTreeDoubleSha256Digest
