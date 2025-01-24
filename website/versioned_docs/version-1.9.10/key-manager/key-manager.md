---
id: version-1.9.10-key-manager
title: Key Manager
original_id: key-manager
---


### Key Manager

The key manager module's goal is to encapsulate all private key interactions with the [wallet](../wallet/wallet.md) project.

As of this writing, there is only one type of `KeyManager` - [`BIP39KeyManager`](/api/org/bitcoins/keymanager/bip39/BIP39KeyManager).

The [`BIP39KeyManager`](/api/org/bitcoins/keymanager/bip39/BIP39KeyManager) stores a [`MnemonicCode`](/api/org/bitcoins/core/crypto/MnemonicCode) on disk which can be decrypted and used as a hot wallet.
 
Over the long run, we want to make it so that the wallet project needs to communicate with the key-manager to access private keys.

This means that ALL SIGNING should be done inside of the key-manager, and private keys should not leave the key manager.

This makes it easier to reason about the security characteristics of our private keys, and a way to provide a uniform interface for alternative key storage systems (hsm, cloud based key storage, etc) to be plugged into the bitcoin-s library.

#### Creating a key manager

The first thing you need create a key manager is some entropy.

A popular way for bitcoin wallet's to represent entropy is [BIP39](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) which you [can use in bitcoin-s](/api/org/bitcoins/core/crypto/BIP39Seed)

You can generate a `MnemonicCode` in bitcoin-s with the following code

```scala
import org.bitcoins.core.crypto._

//get 256 bits of random entropy
val entropy = MnemonicCode.getEntropy256Bits
// entropy: scodec.bits.BitVector = BitVector(256 bits, 0x9ba4b4807f51e44896476132619b17344aa9ca6e557c3546f030b85be97ebac4)

val mnemonic = MnemonicCode.fromEntropy(entropy)
// mnemonic: MnemonicCode = Masked(MnemonicCodeImpl)

//you can print that mnemonic seed with this
println(mnemonic.words)
// Vector(option, certain, cactus, wrong, bundle, cattle, flip, umbrella, crash, art, shift, hammer, pride, topple, rich, question, crystal, daring, around, idle, salon, satisfy, twelve, october)
```

Now that we have a `MnemonicCode` that was securely generated, we need to now create `KeyManagerParams` which tells us how to generate
generate specific kinds of addresses for wallets.

`KeyManagerParams` takes 3 parameters:

1. `seedPath` there is where we store the `MnemonicCode` on your file system
2. [`purpose`](/api/org/bitcoins/core/hd/HDPurpose) which represents what type of utxo this `KeyManager` is associated with. The specification for this is in [BIP43](https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki)
3. [`network`](/api/org/bitcoins/core/config/NetworkParameters) what cryptocurrency network this key manager is associated with


This controls how the root key is defined. The combination of `purpose` and `network` determine how the root `ExtKey` is serialized. For more information on how this works please see [hd-keys](../core/hd-keys.md)

Now we can construct a native segwit key manager for the regtest network!

```scala
//this will create a temp directory with the prefix 'key-manager-example` that will
//have a file in it called "encrypted-bitcoin-s-seed.json"
val seedPath = Files.createTempDirectory("key-manager-example").resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
// seedPath: Path = /var/folders/00/27b127nj25q8gnjhjqfnw9040000gn/T/key-manager-example1363737922298881160/encrypted-bitcoin-s-seed.json

//let's create a native segwit key manager
val purpose = HDPurpose.SegWit
// purpose: HDPurpose = m/84'

//let's choose regtest as our network
val network = RegTest
// network: RegTest.type = RegTest

val kmParams = KeyManagerParams(seedPath, purpose, network)
// kmParams: KeyManagerParams = KeyManagerParams(/var/folders/00/27b127nj25q8gnjhjqfnw9040000gn/T/key-manager-example1363737922298881160/encrypted-bitcoin-s-seed.json,m/84',RegTest)

val aesPasswordOpt = Some(AesPassword.fromString("password"))
// aesPasswordOpt: Some[AesPassword] = Some(Masked(AesPassword))

val km = BIP39KeyManager.initializeWithMnemonic(aesPasswordOpt, mnemonic, None, kmParams)
// km: Either[KeyManagerInitializeError, BIP39KeyManager] = Right(org.bitcoins.keymanager.bip39.BIP39KeyManager@6193b3a0)

val rootXPub = km.right.get.getRootXPub
// rootXPub: ExtPublicKey = vpub5SLqN2bLY4WeYyJciwbDBryLWynBtFkAEAUVwe4cRQmcFE5XTNtsirupkSarg1mfkXyySF3ycA3t78hPeG52Gfi8VZYcqbJ8zXJrAAavdak

println(rootXPub)
// vpub5SLqN2bLY4WeYyJciwbDBryLWynBtFkAEAUVwe4cRQmcFE5XTNtsirupkSarg1mfkXyySF3ycA3t78hPeG52Gfi8VZYcqbJ8zXJrAAavdak
```

Which should print something that looks like this

`vpub5SLqN2bLY4WeXxMqwJHJFBEwxSscGB2uDUnsTS3edVjZEwTrQDFDNqoR2xLqARQPabGaXsHSTenTRcqm2EnB9MpuC4vSk3LqSgNmGGZtuq7`

which is a native segwit `ExtPubKey` for the regtest network!

You can always change the `network` or `purpose` to support different things. You do _not_ need to initialize the key manager
again after initializing it once. You can use the same `mnemonic` for different networks, which you control `KeyManagerParams`.

```scala
//let's create a nested segwit key manager for mainnet
val mainnetKmParams = KeyManagerParams(seedPath, HDPurpose.SegWit, MainNet)
// mainnetKmParams: KeyManagerParams = KeyManagerParams(/var/folders/00/27b127nj25q8gnjhjqfnw9040000gn/T/key-manager-example1363737922298881160/encrypted-bitcoin-s-seed.json,m/84',MainNet)

//we do not need to all `initializeWithMnemonic()` again as we have saved the seed to dis
val mainnetKeyManager = BIP39KeyManager.fromMnemonic(mnemonic, mainnetKmParams, None, Instant.now, false)
// mainnetKeyManager: BIP39KeyManager = org.bitcoins.keymanager.bip39.BIP39KeyManager@6960fc61

val mainnetXpub = mainnetKeyManager.getRootXPub
// mainnetXpub: ExtPublicKey = zpub6jftahH18ngZxA564Nji2DMMCrMyeji9tcZP5De9wSH8TdLSU1Z8D7YNqGRCfePMP6TCS9SDSoU5eH9eX3j5TcSXxvLKBEa65RZRiT3WiBE

println(mainnetXpub)
// zpub6jftahH18ngZxA564Nji2DMMCrMyeji9tcZP5De9wSH8TdLSU1Z8D7YNqGRCfePMP6TCS9SDSoU5eH9eX3j5TcSXxvLKBEa65RZRiT3WiBE
```

Which gives us something that looks like this

`zpub6jftahH18ngZw98KGjRo5XcxeKTQ2eztsvskb1dC9XF5TLimQquTs6Ry7nBBA425D9joXmfgJJCexmJ1u2SELJZJfRi95gcnXadLpZzYb5c`

which is a p2sh wrapped segwit `ExtPubKey` for the bitcoin main network!

#### Creating a key manager from existing mnemonic

To create a `KeyManager` from existing mnemonic you need to specify the `seedPath` and then construct the `KeyManagerParams` that you would like.

Finally you call `KeyManager.fromParams()` that reads the mnemonic from disk and create's the key manager