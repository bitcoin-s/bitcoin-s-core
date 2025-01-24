---
id: version-1.9.10-hd-keys
title: HD Key Generation
original_id: hd-keys
---

In modern Bitcoin wallets, users only need to write down
a sequence of words, and that sequence is a complete backup
of their wallet. This is thanks to what's called Hierarchical
Deterministic key generation. In short, every wallet using HD
key generation has a root seed for each wallet, and this
seed can be used to generate an arbitrary amount of later
private and public keys. This is done in a standardized manner,
so different wallets can operate with the same standard.

> If you want to jump into the details of how this work,
> you should check out
> [BIP 32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki).

Bitcoin-S supports generating keys in this fashion. Here's a
full example of how to obtain a wallet seed, and then
use that to generate further private and public keys:

```scala
import scodec.bits._
import org.bitcoins.core.crypto._
import org.bitcoins.core.hd._

// the length of the entropy bit vector determine
// how long our phrase ends up being
// 256 bits of entropy results in 24 words
val entropy: BitVector = MnemonicCode.getEntropy256Bits
// entropy: BitVector = BitVector(256 bits, 0x92dda856d94d051b66b0acac09cf7e3e7091952acfda71756f8ef5a9d2a9af4a)

val mnemonicCode = MnemonicCode.fromEntropy(entropy)
// mnemonicCode: MnemonicCode = Masked(MnemonicCodeImpl)

mnemonicCode.words // the phrase the user should write down
// res0: Vector[String] = Vector(name, unhappy, between, raw, space, misery, onion, better, project, example, test, language, animal, skin, private, win, imitate, problem, together, twice, outdoor, feel, rule, dress)

// the password argument is an optional, extra security
// measure. all MnemonicCode instances will give you a
// valid BIP39 seed, but different passwords will give
// you different seeds. So you could have as many wallets
// from the same seed as you'd like, by simply giving them
// different passwords.
val bip39Seed = BIP39Seed.fromMnemonic(mnemonicCode,
                                       password = "secret password")
// bip39Seed: BIP39Seed = Masked(BIP39SeedImpl)

val xpriv = ExtPrivateKey.fromBIP39Seed(ExtKeyVersion.SegWitMainNetPriv,
                                        bip39Seed)
// xpriv: ExtPrivateKey = Masked(ExtPrivateKeyImpl)
val xpub = xpriv.extPublicKey
// xpub: ExtPublicKey = zpub6jftahH18ngZwx1H2FNYh5Dv7cuQfC6CtTPaz5aQjhiwQUmKhztg4qGzF3kFfFKXMT5hyh47w3h511JYPjJM25Q4u91xgwXMWTvK9LPg5Dt

// you can now use the generated xpriv to derive further
// private or public keys

// this can be done with BIP89 paths (called SegWitHDPath in bitcoin-s)
val segwitPath = SegWitHDPath.fromString("m/84'/0'/0'/0/0")
// segwitPath: SegWitHDPath = m/84'/0'/0'/0/0

// alternatively:
val otherSegwitPath =
SegWitHDPath(HDCoinType.Bitcoin,
             accountIndex = 0,
             HDChainType.External,
             addressIndex = 0)
// otherSegwitPath: SegWitHDPath = m/84'/0'/0'/0/0

segwitPath == otherSegwitPath
// res1: Boolean = true

// there's also paths available for legacy
// addresses (LegacyHDPath) as well as nested
// segwit paths (NestedSegWitPath)
```

## Generating new addresses without having access to the private key

One the coolest features of HD wallets is that it's possible
to generate addresses offline, without having access to the
private keys. This feature is commonly called watch-only
wallets, where a wallet can import information about all
your past and future transactions, without being able to
spend or steal any of your money.

Let's see an example of this:

```scala
import scala.util.Success
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.config.TestNet3

//                          first account -------┐
//                          bitcoin ----------┐  |
//                          segwit --------┐  |  |
val accountPath = BIP32Path.fromString("m/84'/0'/0'")
// accountPath: BIP32Path = m/84'/0'/0'
val accountXpub = {
    // this key is sensitive, keep away from prying eyes!
    val accountXpriv = xpriv.deriveChildPrivKey(accountPath)

    // this key is not sufficient to spend from, but we
    // can generate addresses with it!
    accountXpriv.extPublicKey
}
// accountXpub: ExtPublicKey = zpub6qnWaNaxm8fMGNEXmKQTCJqRmH1bEPAZUSRGK2gYbou4gQGAYpmGNCEomWx2VZ4WW4hYhj3uoesT7YaBdVvw6KxmkhN6f9c5akiSXaSGtwp

                              // address no. 0 ---------------┐
                              // external address ----------┐ |
val firstAddressPath = SegWitHDPath.fromString("m/84'/0'/0'/0/0")
// firstAddressPath: SegWitHDPath = m/84'/0'/0'/0/0
val firstAccountAddress = {
    // this is a bit quirky, but we're not interesting in
    // deriving the complete path from our account xpub
    // instead, we're only interested in the part after
    // the account level (3rd level). the .diff() method
    // achieves that
    val Some(pathDiff) = accountPath.diff(firstAddressPath)

    // deriving public keys from hardened extended keys
    // is not possible, that's why .deriveChildPubKey()
    // returns a Try[ExtPublicKey]. A hardened key is marked
    // by a ' after the number in the notation we use above.
    val Success(extPubKey) = accountXpub.deriveChildPubKey(pathDiff)
    val pubkey = extPubKey.key
    val scriptPubKey = P2WPKHWitnessSPKV0(pubkey)
    Bech32Address(scriptPubKey, TestNet3)
}
// firstAccountAddress: Bech32Address = tb1ql9q7nl3pqpmkeepzan8gvt4ce6lnar9g25aemm

// tada! We just generated an address you can send money to,
// without having access to the private key!
firstAccountAddress.value
// res2: String = tb1ql9q7nl3pqpmkeepzan8gvt4ce6lnar9g25aemm

// you can now continue deriving addresses from the same public
// key, by imitating what we did above. To get the next
// HD path to generate an address at:
val nextAddressPath: SegWitHDPath = firstAddressPath.next
// nextAddressPath: SegWitHDPath = m/84'/0'/0'/0/1
```

### Signing things with HD keys

Please see [sign.md](../crypto/sign.md) for information on how to sign things with HD keys.