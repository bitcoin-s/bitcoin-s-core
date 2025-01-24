![Bitcoin-S logo](website/static/img/bitcoin-s-dark-logo.png)
[![Build Status](https://github.com/bitcoin-s/bitcoin-s/workflows/Release/badge.svg)](https://github.com/bitcoin-s/bitcoin-s/actions) [![Coverage Status](https://coveralls.io/repos/github/bitcoin-s/bitcoin-s/badge.svg?branch=master)](https://coveralls.io/github/bitcoin-s/bitcoin-s?branch=master) [![Maven Central](https://img.shields.io/badge/Maven%20Central-1.9.9-brightgreen.svg)](https://mvnrepository.com/artifact/org.bitcoin-s) [![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/bitcoin-s-core)

Feature-rich toolkit for making Bitcoin and Lightning applications on the JVM.

For a complete guide on how to get started with Bitcoin-S, see our website at [Bitcoin-S.org](https://bitcoin-s.org).

### Contents

- [Running bitcoin-s](#running-bitcoin-s)
- [Getting setup (developers)](#getting-setup-developers)
- [Adding bitcoin-s to your library](#adding-bitcoin-s-to-your-library)
- [Docker images](#docker-images)
- [Contributing](#contributing)
- [Good first issues](#good-first-issues)
- [License](#license)

### Running bitcoin-s

#### Docker

In this repo, you can just run

```
APP_PASSWORD=topsecret docker-compose up
```

which will spin up a docker environment that starts syncing the backend and will allow you to visit
the web frontend of the wallet at `localhost:3002`

or you can build and run the electron app natively.

```
git clone https://github.com/bitcoin-s/bitcoin-s-ts.git
cd bitcoin-s-ts && ./build-wallet-electron.sh
```

### Getting setup (developers)

For a complete guide on how to get setup with bitcoin-s, see our [Getting setup](https://bitcoin-s.org/docs/getting-setup).

This link is intended for setting up development of bitcoin-s. If you want to just install bitcoin-s rather than develop, see **Getting started** above.

### Adding bitcoin-s to your library

The latest release of bitcoin-s is `1.9.10`, here is how you can use the dependencies in your projects:

```
libraryDependencies += "org.bitcoin-s" % "bitcoin-s-secp256k1jni" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-core" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-crypto" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-chain" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-oracle" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-app-commons" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-db-commons" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-fee-provider" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-bitcoind-rpc" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-eclair-rpc" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-lnd-rpc" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-key-manager" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-node" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-node" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-wallet" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-dlc-wallet" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit-core" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-testkit" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-zmq" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-tor" % "1.9.10"

libraryDependencies += "org.bitcoin-s" %% "bitcoin-s-cli" % "1.9.10"

```

#### Docker images

We publish docker images to docker hub on every PR merge and tag on github.
You can obtain the images for both the app server and oracle server on these
docker hub repos

[bitcoin-s docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-server/tags?page=1&ordering=last_updated)

[oracle-server docker hub repo](https://hub.docker.com/r/bitcoinscala/bitcoin-s-oracle-server/tags?page=1&ordering=last_updated)

### Contributing

Bitcoin-S is an open source project where anyone is welcome to contribute. All contributions are encouraged and appreciated, whether that is code, testing, documentation or something else entirely.

See [here](https://bitcoin-s.org/docs/contributing) for more information.

### Good first issues

[Here](https://github.com/bitcoin-s/bitcoin-s/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22) is a list of good first issues that contain bugs which have a relatively limited scope. This is a great place to get started, gain experience, and get familiar with the bitcoin-s contribution process.

### License

Bitcoin-s is MIT licensed, as found in the [LICENSE](LICENSE) file.
