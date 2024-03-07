import com.github.sbt.git.SbtGit.GitKeys._
import sbt.Keys.{publish, publishLocal}

import scala.util.Properties

Global / cancelable := true

lazy val Benchmark = config("bench") extend Test

lazy val benchSettings: Seq[Def.SettingsDefinition] = {
  //for scalameter
  //https://scalameter.github.io/home/download/
  //you can add benchmarking to a project by adding these to lines
  //to the projects build definition
  //  .settings(benchSettings: _*)
  //  .configs(Benchmark)
  List(
    Test / unmanagedSourceDirectories += baseDirectory.value / "src" / "bench" / "scala",
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    Benchmark / parallelExecution := false,
    Benchmark / outputStrategy := Some(StdoutOutput),
    Benchmark / fork := true,
    Benchmark / connectInput := true,
    inConfig(Benchmark)(Defaults.testSettings)
  )
}

lazy val commonJsSettings = {
  Seq(
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    }
  ) ++ CommonSettings.settings
}

lazy val crypto = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(
    name := "bitcoin-s-crypto",
    libraryDependencies ++= Deps.crypto.value
  )
  .settings(CommonSettings.settings: _*)
  .jvmSettings(
    libraryDependencies ++= Deps.cryptoJVM
  )
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "bcrypto" -> "5.4.0"
    )
  )
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .in(file("crypto"))

lazy val cryptoJS = crypto.js
  .settings(Compile / scalacOptions += {
    "-Wconf:cat=unused:site=org\\.bitcoins\\.crypto\\.facade\\..*:silent,cat=w-flag-dead-code:site=org\\.bitcoins\\.crypto\\.facade\\..*:silent"
  })
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val cryptoJVM = crypto.jvm
  .dependsOn(secp256k1jni)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(name := "bitcoin-s-core")
  .settings(libraryDependencies ++= Deps.core.value)
  .settings(CommonSettings.prodSettings: _*)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .in(file("core"))
  .dependsOn(crypto)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js
  .settings(libraryDependencies ++= Deps.coreJs.value)
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val asyncUtils = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("async-utils"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-async-utils",
            libraryDependencies ++= Deps.asyncUtils.value)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(core)

lazy val asyncUtilsJVM = asyncUtils.jvm

lazy val asyncUtilsJS = asyncUtils.js

lazy val asyncUtilsTest = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("async-utils-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(name := "bitcoin-s-async-utils-test")
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(asyncUtils, testkitCore)

lazy val asyncUtilsTestJVM = asyncUtilsTest.jvm

lazy val asyncUtilsTestJS = asyncUtilsTest.js

lazy val testkitCore = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("testkit-core"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-testkit-core",
            libraryDependencies ++= Deps.testkitCore.value)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(asyncUtils, core, crypto)

lazy val testkitCoreJVM = testkitCore.jvm

lazy val testkitCoreJS = testkitCore.js

lazy val bitcoindRpc = project
  .in(file("bitcoind-rpc"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(
    asyncUtilsJVM,
    appCommons,
    tor
  )

lazy val eclairRpc = project
  .in(file("eclair-rpc"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(asyncUtilsJVM, bitcoindRpc)

lazy val lndRpc = project
  .in(file("lnd-rpc"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(asyncUtilsJVM, appCommons)

lazy val clightningRpc = project
  .in(file("clightning-rpc"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(asyncUtilsJVM, bitcoindRpc)

lazy val lnurl = project
  .in(file("lnurl"))
  .settings(name := "bitcoin-s-lnurl")
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(appCommons, asyncUtilsJVM, tor)

lazy val lnurlTest = project
  .in(file("lnurl-test"))
  .settings(name := "bitcoin-s-lnurl-test")
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(lnurl, testkit)

lazy val tor = project
  .in(file("tor"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(coreJVM, appCommons, asyncUtilsJVM)

lazy val torTest = project
  .in(file("tor-test"))
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(tor, testkit)

lazy val jsProjects: Vector[ProjectReference] =
  Vector(asyncUtilsJS,
         asyncUtilsTestJS,
         cryptoJS,
         coreJS,
         cryptoTestJS,
         coreTestJS,
         testkitCoreJS)

// quoting the val name this way makes it appear as
// 'bitcoin-s' in sbt/bloop instead of 'bitcoins'
lazy val `bitcoin-s` = project
  .in(file("."))
  .aggregate(
    asyncUtilsJVM,
    secp256k1jni,
    chain,
    chainTest,
    cli,
    cliTest,
    coreJVM,
    coreJS,
    coreTestJVM,
    coreTestJS,
    cryptoJVM,
    cryptoJS,
    cryptoTestJVM,
    cryptoTestJS,
    dbCommons,
    dbCommonsTest,
    feeProvider,
    feeProviderTest,
    esplora,
    esploraTest,
    dlcOracle,
    dlcOracleTest,
    bitcoindRpc,
    bitcoindRpcTest,
    bench,
    eclairRpc,
    eclairRpcTest,
    keyManager,
    keyManagerTest,
    node,
    nodeTest,
    wallet,
    walletTest,
    dlcWallet,
    dlcWalletTest,
    dlcNode,
    dlcNodeTest,
    appServer,
    appServerTest,
    appCommons,
    appCommonsTest,
    testkitCoreJVM,
    testkitCoreJS,
    testkit,
    zmq,
    oracleServer,
    oracleServerTest,
    serverRoutes,
    lndRpc,
    lndRpcTest,
    lnurl,
    lnurlTest,
    tor,
    torTest,
    scripts,
    clightningRpc,
    clightningRpcTest
  )
  .dependsOn(
    secp256k1jni,
    chain,
    chainTest,
    cli,
    cliTest,
    coreJVM,
    coreJS,
    coreTestJVM,
    coreTestJS,
    cryptoJVM,
    cryptoJS,
    cryptoTestJVM,
    cryptoTestJS,
    dbCommons,
    dbCommonsTest,
    feeProvider,
    feeProviderTest,
    esplora,
    esploraTest,
    dlcOracle,
    dlcOracleTest,
    bitcoindRpc,
    bitcoindRpcTest,
    bench,
    eclairRpc,
    eclairRpcTest,
    keyManager,
    keyManagerTest,
    node,
    nodeTest,
    wallet,
    walletTest,
    dlcWallet,
    dlcWalletTest,
    dlcNode,
    dlcNodeTest,
    appServer,
    appServerTest,
    appCommons,
    appCommonsTest,
    testkit,
    zmq,
    oracleServer,
    oracleServerTest,
    serverRoutes,
    lndRpc,
    lndRpcTest,
    lnurl,
    lnurlTest,
    tor,
    torTest,
    scripts,
    clightningRpc,
    clightningRpcTest
  )
  .settings(CommonSettings.settings: _*)
  // unidoc aggregates Scaladocs for all subprojects into one big doc
  .enablePlugins(ScalaUnidocPlugin)
  .settings(
  )
  .settings(
    name := "bitcoin-s",
    gitRemoteRepo := "git@github.com:bitcoin-s/bitcoin-s-core.git",
    publish / skip := true
  )

lazy val secp256k1jni = project
  .in(file("secp256k1jni"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    libraryDependencies ++= Deps.secp256k1jni,
    // we place lib files in this directory
    Compile / unmanagedResourceDirectories += baseDirectory.value / "natives",
    //since this is not a scala module, we have no code coverage
    //this also doesn't place nice with scoverage, see
    //https://github.com/scoverage/sbt-scoverage/issues/275
    coverageEnabled := false,
    Compile / compile / javacOptions ++= {
      //https://github.com/eclipse/jetty.project/issues/3244#issuecomment-495322586
      Seq("--release", "8")
    }
  )

val testAndCompile = "compile->compile;test->test"

lazy val cryptoTest = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("crypto-test"))
  .settings(CommonSettings.testSettings: _*)
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .settings(
    name := "bitcoin-s-crypto-test",
    libraryDependencies ++= Deps.cryptoTest.value
  )
  .dependsOn(crypto)

lazy val cryptoTestJVM = cryptoTest.jvm

lazy val cryptoTestJS = cryptoTest.js
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val coreTest = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-core-test",
    libraryDependencies ++= Deps.coreTest.value
  )
  .jvmSettings(CommonSettings.jvmSettings: _*)
  .jsSettings(commonJsSettings: _*)
  .dependsOn(
    core,
    testkitCore
  )

lazy val coreTestJVM = coreTest.jvm
  .settings(libraryDependencies ++= Deps.coreTestJVM.value)
  .dependsOn(testkit)

lazy val coreTestJS = coreTest.js
  .enablePlugins(ScalaJSBundlerPlugin)

lazy val appCommons = project
  .in(file("app-commons"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(
    coreJVM % testAndCompile
  )

lazy val appCommonsTest = project
  .in(file("app-commons-test"))
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(appCommons, testkit)

lazy val oracleServer = project
  .in(file("app/oracle-server"))
  .settings(CommonSettings.appSettings: _*)
  .settings(CommonSettings.dockerSettings: _*)
  .settings(CommonSettings.dockerBuildxSettings: _*)
  .settings(jlinkModules ++= CommonSettings.jlinkModules)
  .settings(jlinkModules --= CommonSettings.rmJlinkModules)
  .settings(jlinkOptions ++= CommonSettings.jlinkOptions)
  .settings(jlinkIgnoreMissingDependency := CommonSettings.oracleServerJlinkIgnore)
  .settings(bashScriptExtraDefines ++= IO.readLines(baseDirectory.value / "src" / "universal" / "oracle-server-extra-startup-script.sh"))
  .dependsOn(
    dlcOracle,
    serverRoutes
  )
  .enablePlugins(JavaAppPackaging, DockerPlugin, JlinkPlugin, 
    //needed for windows, else we have the 'The input line is too long` on windows OS
    LauncherJarPlugin)

lazy val oracleServerTest = project
  .in(file("app/oracle-server-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(libraryDependencies ++= Deps.walletServerTest)
  .dependsOn(
    oracleServer,
    testkit
  )

lazy val serverRoutes = project
  .in(file("app/server-routes"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-server-routes")
  .settings(libraryDependencies ++= Deps.serverRoutes)
  .dependsOn(appCommons, dbCommons)

lazy val appServer = project
  .in(file("app/server"))
  .settings(CommonSettings.appSettings: _*)
  .settings(CommonSettings.dockerSettings: _*)
  .settings(CommonSettings.dockerBuildxSettings: _*)
  .settings(jlinkModules ++= CommonSettings.jlinkModules)
  .settings(jlinkModules --= CommonSettings.rmJlinkModules)
  .settings(jlinkOptions ++= CommonSettings.jlinkOptions)
  .settings(jlinkIgnoreMissingDependency := CommonSettings.appServerJlinkIgnore)
  .settings(bashScriptExtraDefines ++= IO.readLines(baseDirectory.value / "src" / "universal" / "wallet-server-extra-startup-script.sh"))
  .dependsOn(
    serverRoutes,
    appCommons,
    node,
    chain,
    wallet,
    dlcWallet,
    dlcNode,
    bitcoindRpc,
    feeProvider,
    zmq
  )
  .enablePlugins(JavaAppPackaging, DockerPlugin, JlinkPlugin,
    //needed for windows, else we have the 'The input line is too long` on windows OS
    LauncherJarPlugin)

lazy val appServerTest = project
  .in(file("app/server-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(libraryDependencies ++= Deps.walletServerTest)
  .dependsOn(
    appServer,
    testkit,
    cli
  )

lazy val cli = project
  .in(file("app/cli"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-cli"
  )
  .settings(jlinkOptions ++= CommonSettings.jlinkOptions)
  .settings(jlinkModules --= CommonSettings.rmCliJlinkModules)
  .settings(jlinkIgnoreMissingDependency := CommonSettings.cliJlinkIgnore)
  .settings(bashScriptExtraDefines ++= IO.readLines(baseDirectory.value / "src" / "universal" / "cli-extra-startup-script.sh"))
  .dependsOn(
    appCommons
  ).enablePlugins(JavaAppPackaging, NativeImagePlugin, JlinkPlugin)

lazy val cliTest = project
  .in(file("app/cli-test"))
  .settings(CommonSettings.testSettings: _*)
  .dependsOn(
    cli,
    testkit
  )

lazy val chain = project
  .in(file("chain"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-chain",
    libraryDependencies ++= Deps.chain
  )
  .dependsOn(coreJVM, dbCommons)

lazy val chainTest = project
  .in(file("chain-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-chain-test",
    libraryDependencies ++= Deps.chainTest
  )
  .dependsOn(chain, coreJVM % testAndCompile, testkit, zmq)

lazy val dbCommons = project
  .in(file("db-commons"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-db-commons",
    libraryDependencies ++= Deps.dbCommons.value
  )
  .dependsOn(coreJVM, appCommons, keyManager)

lazy val dbCommonsTest = project
  .in(file("db-commons-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-db-commons-test"
  )
  .dependsOn(testkit)

lazy val esplora = project
  .in(file("esplora"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-esplora",
    libraryDependencies ++= Deps.esplora.value
  )
  .dependsOn(coreJVM, appCommons, tor)

lazy val esploraTest = project
  .in(file("esplora-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-esplora-test",
    libraryDependencies ++= Deps.esploraTest.value
  )
  .dependsOn(coreJVM % testAndCompile, esplora, testkit)

lazy val feeProvider = project
  .in(file("fee-provider"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-fee-provider",
    libraryDependencies ++= Deps.feeProvider.value
  )
  .dependsOn(coreJVM, appCommons, tor)

lazy val feeProviderTest = project
  .in(file("fee-provider-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-fee-provider-test",
    libraryDependencies ++= Deps.feeProviderTest.value
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val zmq = project
  .in(file("zmq"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(name := "bitcoin-s-zmq",
            libraryDependencies ++= Deps.bitcoindZmq.value)
  .dependsOn(
    coreJVM % testAndCompile,
    appCommons
  )

def isCI = {
  Properties
    .envOrNone("CI")
    .isDefined
}

def isTor = {
  Properties
    .envOrNone("TOR")
    .isDefined
}

lazy val bitcoindRpcTest = project
  .in(file("bitcoind-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(name := "bitcoin-s-bitcoind-rpc-test",
            libraryDependencies ++= Deps.bitcoindRpcTest.value,
            parallelExecution := !(isCI && Properties.isMac))
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val bench = project
  .in(file("bench"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    libraryDependencies ++= Deps.bench,
    name := "bitcoin-s-bench",
    publish / skip := true
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val eclairRpcTest = project
  .in(file("eclair-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    libraryDependencies ++= Deps.eclairRpcTest.value,
    name := "bitcoin-s-eclair-rpc-test",
    parallelExecution := !(isCI && Properties.isMac)
  )
  .dependsOn(coreJVM % testAndCompile, testkit)

lazy val clightningRpcTest = project
  .in(file("clightning-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    libraryDependencies ++= Deps.clightningRpcTest.value,
    name := "bitcoin-s-clightning-rpc-test"
  )
  .dependsOn(coreJVM % testAndCompile, clightningRpc, testkit)

lazy val lndRpcTest = project
  .in(file("lnd-rpc-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    libraryDependencies ++= Deps.eclairRpcTest.value,
    name := "bitcoin-s-lnd-rpc-test",
    parallelExecution := !(isCI && Properties.isMac)
  )
  .dependsOn(coreJVM % testAndCompile, testkit, lndRpc)

lazy val node =
  project
    .in(file("node"))
    .settings(CommonSettings.prodSettings: _*)
    .settings(
      name := "bitcoin-s-node",
      libraryDependencies ++= Deps.node
    )
    .dependsOn(
      asyncUtilsJVM,
      coreJVM,
      chain,
      dbCommons,
      bitcoindRpc,
      tor
    )

lazy val nodeTest =
  project
    .in(file("node-test"))
    .settings(CommonSettings.testSettings: _*)
    .settings(
      name := "bitcoin-s-node-test",
      // There's a weird issue with forking
      // in node tests, for example this CI
      // error: https://travis-ci.org/bitcoin-s/bitcoin-s-core/jobs/525018199#L1252
      // It seems to be related to this
      // Scalatest issue:
      // https://github.com/scalatest/scalatest/issues/556
      Test / fork := false,
      libraryDependencies ++= Deps.nodeTest.value,
      parallelExecution := !isTor
    )
    .dependsOn(
      coreJVM % testAndCompile,
      node,
      testkit
    )

lazy val testkit = project
  .in(file("testkit"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-testkit",
    libraryDependencies ++= Deps.testkit.value
  )
  .dependsOn(
    asyncUtilsJVM,
    coreJVM % testAndCompile,
    appServer,
    chain,
    bitcoindRpc,
    eclairRpc,
    lndRpc,
    clightningRpc,
    node,
    wallet,
    dlcWallet,
    zmq,
    dlcOracle,
    testkitCoreJVM
  )

lazy val docs = project
  .in(file("bitcoin-s-docs")) // important: it must not be docs/
  .settings(CommonSettings.testSettings: _*)
  .settings(libraryDependencies ++= Deps.docs.value)
  .settings(
    name := "bitcoin-s-docs",
    moduleName := name.value,
    //removes scalajs projects from unidoc, see
    //https://github.com/bitcoin-s/bitcoin-s/issues/2740
    ScalaUnidoc / unidoc / unidocProjectFilter := {
      inAnyProject -- inProjects(jsProjects: _*)
    },
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite
      .dependsOn(Compile / unidoc)
      .value,
    docusaurusPublishGhpages := docusaurusPublishGhpages
      .dependsOn(Compile / unidoc)
      .value
  )
  .enablePlugins(MdocPlugin,
                 DocusaurusPlugin,
                 ScalaUnidocPlugin,
                 BuildInfoPlugin)
  .dependsOn(
    appCommons,
    asyncUtilsJVM,
    appServer,
    bitcoindRpc,
    chain,
    cli,
    cryptoJVM,
    coreJVM,
    dbCommons,
    feeProvider,
    dlcOracle,
    eclairRpc,
    keyManager,
    node,
    secp256k1jni,
    testkitCoreJVM,
    testkit,
    wallet,
    zmq
  )

lazy val keyManager = project
  .in(file("key-manager"))
  .settings(CommonSettings.prodSettings: _*)
  .dependsOn(coreJVM, appCommons)

lazy val keyManagerTest = project
  .in(file("key-manager-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(name := "bitcoin-s-keymanager-test",
            libraryDependencies ++= Deps.keyManagerTest)
  .dependsOn(keyManager, testkit)

lazy val wallet = project
  .in(file("wallet"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-wallet",
    libraryDependencies ++= Deps.wallet(scalaVersion.value)
  )
  .dependsOn(coreJVM, appCommons, dbCommons, keyManager, asyncUtilsJVM, tor)

lazy val walletTest = project
  .in(file("wallet-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-wallet-test",
    libraryDependencies ++= Deps.walletTest
  )
  .dependsOn(coreJVM % testAndCompile, testkit, wallet)

lazy val dlcWallet = project
  .in(file("dlc-wallet"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-wallet",
    libraryDependencies ++= Deps.dlcWallet
  )
  .dependsOn(wallet)

lazy val dlcWalletTest = project
  .in(file("dlc-wallet-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-wallet-test",
    libraryDependencies ++= Deps.dlcWalletTest
  )
  .dependsOn(coreJVM % testAndCompile,
             dlcWallet,
             testkit,
             testkitCoreJVM)

lazy val dlcNode = project
  .in(file("dlc-node"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-node",
    libraryDependencies ++= Deps.dlcNode
  )
  .dependsOn(coreJVM, tor, dbCommons)

lazy val dlcNodeTest = project
  .in(file("dlc-node-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-node-test",
    libraryDependencies ++= Deps.dlcNodeTest,
    parallelExecution := !isTor
  )
  .dependsOn(coreJVM % testAndCompile, dlcNode, testkit)

lazy val dlcOracle = project
  .in(file("dlc-oracle"))
  .settings(CommonSettings.prodSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-oracle",
    libraryDependencies ++= Deps.dlcOracle
  )
  .dependsOn(coreJVM, keyManager, dbCommons)

lazy val dlcOracleTest = project
  .in(file("dlc-oracle-test"))
  .settings(CommonSettings.testSettings: _*)
  .settings(
    name := "bitcoin-s-dlc-oracle-test",
    libraryDependencies ++= Deps.dlcOracleTest
  )
  .dependsOn(coreJVM % testAndCompile, dlcOracle, testkit)

lazy val scripts = project
  .in(file("app/scripts"))
  .settings(CommonSettings.settings: _*)
  .settings(
    name := "bitcoin-s-scripts",
    publishArtifact := false //do not want to publish our scripts
  )
  .dependsOn(appServer)
  .enablePlugins(JavaAppPackaging)
