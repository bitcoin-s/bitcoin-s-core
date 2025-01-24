---
id: version-1.9.10-rpc-clients-intro
title: Introduction
original_id: rpc-clients-intro
---

When working with Bitcoin applications, a common task to
accomplish is connecting to a service like Bitcoin Core,
and use that for tasks like generating addresses,
verifying payments and
monitoring the blockchain. This typically happens through
tools like `bitcoin-cli`, or the Bitcoin Core HTTP RPC
server interface. One big drawback to this, is that you
lose all type-safety in your application. Even if you
have a custom type that represents a Bitcoin transaction,
how do you get that to play nicely with the result that
Bitcoin Core gives you after signing a transaction? A
random hexadecimal string in a HTTP response could be
anything from a public key, a transaction or a block
header.

We've done all the mundane work of wiring requests and
responses from Bitcoin Core to the powerful and safe types
found in Bitcoin-S. We've also written a bunch of tests,
that verify that all of this actually works.
You'll know for sure that you're sending
a valid public key to `importmulti`, and you when doing
RPC calls like `getblockheader` we'll even parse the
hexadecimal string into a complete header that you can
interact with without goofing around with bits and bytes.

We currently have RPC clients for Bitcoin Core, Eclair, Lnd.
