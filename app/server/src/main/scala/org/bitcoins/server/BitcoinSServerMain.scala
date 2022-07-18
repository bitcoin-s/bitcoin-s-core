package org.bitcoins.server

import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{
  BroadcastHub,
  Keep,
  Sink,
  Source,
  SourceQueueWithComplete
}
import akka.{Done, NotUsed}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.asyncutil.AsyncUtil.Exponential
import org.bitcoins.chain.ChainCallbacks
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockChainInfoResult
import org.bitcoins.commons.jsonmodels.ws.WsNotification
import org.bitcoins.commons.util.{DatadirParser, ServerArgParser}
import org.bitcoins.core.api.chain.{ChainApi, ChainQueryApi}
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.{
  InternalImplementationNodeType,
  NodeApi,
  NodeType
}
import org.bitcoins.core.api.wallet.{NeutrinoWalletApi, WalletApi}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.dlc.node.config.DLCNodeAppConfig
import org.bitcoins.dlc.wallet._
import org.bitcoins.feeprovider.MempoolSpaceTarget.HourFeeTarget
import org.bitcoins.feeprovider._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.NodeStateDescriptorDAO
import org.bitcoins.rpc.BitcoindException.InWarmUp
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.{BitcoindRpcAppConfig, ZmqConfig}
import org.bitcoins.server.routes.{BitcoinSServerRunner, CommonRoutes, Server}
import org.bitcoins.server.util._
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.wallet.WalletHolder
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.SpendingInfoDAO

import java.sql.SQLException
import java.time.Instant
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

class BitcoinSServerMain(override val serverArgParser: ServerArgParser)(implicit
    override val system: ActorSystem,
    val conf: BitcoinSAppConfig)
    extends BitcoinSServerRunner {

  implicit lazy val nodeConf: NodeAppConfig = conf.nodeConf
  implicit lazy val chainConf: ChainAppConfig = conf.chainConf
  implicit lazy val dlcNodeConf: DLCNodeAppConfig = conf.dlcNodeConf
  implicit lazy val bitcoindRpcConf: BitcoindRpcAppConfig = conf.bitcoindRpcConf
  implicit lazy val torConf: TorAppConfig = conf.torConf
  lazy val network = conf.walletConf.network

  override def start(): Future[Unit] = {
    logger.info("Starting appServer")
    val startTime = TimeUtil.currentEpochMs
    val startedConfigF = conf.start()

    logger.info(s"Start on network $network")

    startedConfigF.failed.foreach { err =>
      logger.error(s"Failed to initialize configuration for BitcoinServerMain",
                   err)
    }

    for {
      startedConfig <- startedConfigF
      chainApi = ChainHandler.fromDatabase()
      //on server startup we assume we are out of sync with the bitcoin network
      //so we set this flag to true.
      _ <- chainApi.setSyncing(true)
      start <- {
        nodeConf.nodeType match {
          case _: InternalImplementationNodeType =>
            startBitcoinSBackend(startedConfig.torStartedF)
          case NodeType.BitcoindBackend =>
            startBitcoindBackend(startedConfig.torStartedF)
        }
      }
    } yield {
      logger.info(
        s"Done start BitcoinSServerMain, it took=${TimeUtil.currentEpochMs - startTime}ms")
      start
    }
  }

  override def stop(): Future[Unit] = {
    logger.error(s"Exiting process")
    for {
      _ <- conf.stop()
      _ <- serverBindingsOpt match {
        case Some(bindings) => bindings.stop()
        case None           => Future.unit
      }
      _ = logger.info(s"Stopped ${nodeConf.nodeType.shortName} node")
    } yield {
      ()
    }
  }

  /** Start the bitcoin-s wallet server with a neutrino backend
    * @param startedTorConfigF a future that is completed when tor is fully started
    * @return
    */
  def startBitcoinSBackend(startedTorConfigF: Future[Unit]): Future[Unit] = {
    logger.info(s"startBitcoinSBackend()")
    val start = System.currentTimeMillis()

    val chainApi = ChainHandler.fromDatabase()
    val creationTime: Instant = conf.walletConf.creationTime

    //get a node that isn't started
    val nodeF = nodeConf.createNode(
      peers = Vector.empty,
      walletCreationTimeOpt = Some(creationTime))(chainConf, system)

    val defaultApi =
      MempoolSpaceProvider(HourFeeTarget, network, torConf.socks5ProxyParams)
    val feeProvider = FeeProviderFactory.getFeeProviderOrElse(
      defaultApi,
      conf.walletConf.feeProviderNameOpt,
      conf.walletConf.feeProviderTargetOpt,
      torConf.socks5ProxyParams,
      network)
    //get our wallet
    val walletHolder = new WalletHolder()
    val configuredWalletF = for {
      node <- nodeF
      _ = logger.info("Initialized chain api")

      lastLoadedWalletNameOpt <- getLastLoadedWalletName()
      walletNameOpt = lastLoadedWalletNameOpt.getOrElse(
        WalletAppConfig.DEFAULT_WALLET_NAME)
      (walletConfig, dlcConfig) <- updateWalletConfigs(walletNameOpt, None)
        .recover { case _: Throwable => (conf.walletConf, conf.dlcConf) }
      dlcWallet <- dlcConfig.createDLCWallet(node, chainApi, feeProvider)(
        walletConfig,
        ec)
      _ <- walletHolder.replaceWallet(dlcWallet)
      nodeCallbacks <- CallbackUtil.createNeutrinoNodeCallbacksForWallet(
        walletHolder)
      _ = nodeConf.addCallbacks(nodeCallbacks)
    } yield {
      logger.info(
        s"Done configuring wallet, it took=${System.currentTimeMillis() - start}ms")
      (walletHolder, walletConfig, dlcConfig)
    }

    //add callbacks to our uninitialized node
    val configuredNodeF = for {
      node <- nodeF
      _ <- configuredWalletF
    } yield {
      logger.info(
        s"Done configuring node, it took=${System.currentTimeMillis() - start}ms")
      node
    }

    val dlcNodeF = for {
      (wallet, _, _) <- configuredWalletF
      node = dlcNodeConf.createDLCNode(wallet)
    } yield node

    val tuple = buildWsSource

    val wsQueue: SourceQueueWithComplete[WsNotification[_]] = tuple._1
    val wsSource: Source[WsNotification[_], NotUsed] = tuple._2

    val callbacksF: Future[Unit] = for {
      (_, walletConfig, dlcConfig) <- configuredWalletF
    } yield buildNeutrinoCallbacks(wsQueue, chainApi, walletConfig, dlcConfig)

    val torCallbacks = WebsocketUtil.buildTorCallbacks(wsQueue)
    torConf.addCallbacks(torCallbacks)

    val isTorStartedF = if (torConf.torProvided) {
      //if tor is provided we need to execute the tor started callback immediately
      torConf.callBacks.executeOnTorStarted()
    } else {
      Future.unit
    }
    val startedNodeF = {
      //can't start connecting to peers until tor is done starting
      for {
        _ <- startedTorConfigF
        _ <- isTorStartedF
        started <- configuredNodeF.flatMap(_.start())
      } yield started
    }

    val startedDLCNodeF = {
      for {
        dlcNode <- dlcNodeF
        _ <- dlcNode.start()
      } yield dlcNode
    }

    val loadWalletFnF =
      configuredWalletF.map(tuple => loadNeutrinoWallet(tuple._1)(_, _))
    //start our http server now that we are synced
    for {
      loadWalletFn <- loadWalletFnF
      _ <- startHttpServer(
        nodeApiF = startedNodeF,
        chainApi = chainApi,
        walletF = configuredWalletF.map(_._1),
        dlcNodeF = startedDLCNodeF,
        torConfStarted = startedTorConfigF,
        serverCmdLineArgs = serverArgParser,
        wsSource = wsSource,
        loadWalletFn
      )
      _ = {
        logger.info(
          s"Starting ${nodeConf.nodeType.shortName} node sync, it took=${System
            .currentTimeMillis() - start}ms")
      }
      //make sure callbacks are registered before we start sync
      _ <- callbacksF
      node <- startedNodeF
      _ <- startedTorConfigF
      (wallet, walletConfig, _) <- configuredWalletF
      _ <- handleDuplicateSpendingInfoDb(wallet, walletConfig)
      _ <- restartRescanIfNeeded(wallet)
      _ <- node.sync()
    } yield {
      logger.info(
        s"Done starting Main! It took ${System.currentTimeMillis() - start}ms")
      ()
    }
  }

  private def buildNeutrinoCallbacks(
      wsQueue: SourceQueueWithComplete[WsNotification[_]],
      chainApi: ChainApi,
      walletConf: WalletAppConfig,
      dlcConf: DLCAppConfig): Unit = {
    val chainCallbacks = WebsocketUtil.buildChainCallbacks(wsQueue, chainApi)
    chainConf.addCallbacks(chainCallbacks)

    val walletCallbacks =
      WebsocketUtil.buildWalletCallbacks(wsQueue, walletConf.walletName)
    walletConf.addCallbacks(walletCallbacks)

    val dlcWalletCallbacks = WebsocketUtil.buildDLCWalletCallbacks(wsQueue)
    dlcConf.addCallbacks(dlcWalletCallbacks)

    val torCallbacks = WebsocketUtil.buildTorCallbacks(wsQueue)
    torConf.addCallbacks(torCallbacks)

    ()
  }

  /** Returns blockchain info, in case of  [[InWarmUp]] exception it retries.
    */
  private def getBlockChainInfo(
      client: BitcoindRpcClient): Future[GetBlockChainInfoResult] = {
    val promise = Promise[GetBlockChainInfoResult]()
    for {
      _ <- AsyncUtil.retryUntilSatisfiedF(
        conditionF = { () =>
          val infoF = client.getBlockChainInfo
          val res = infoF.map(promise.success).map(_ => true)
          res.recover { case _: InWarmUp => false }
        },
        // retry for approximately 2 hours
        mode = Exponential,
        interval = 1.second,
        maxTries = 12
      )
      info <- promise.future
    } yield info
  }

  /** Start the bitcoin-s wallet server with a bitcoind backend
    * @param startedTorConfigF a future that is completed when tor is fully started
    * @return
    */
  def startBitcoindBackend(startedTorConfigF: Future[Unit]): Future[Unit] = {
    logger.info(s"startBitcoindBackend()")
    val bitcoindF = for {
      client <- bitcoindRpcConf.clientF
      _ <- client.start()
    } yield {
      logger.info("Started bitcoind")
      client
    }
    val tuple = buildWsSource
    val wsQueue: SourceQueueWithComplete[WsNotification[_]] = tuple._1
    val wsSource: Source[WsNotification[_], NotUsed] = tuple._2
    val torCallbacks = WebsocketUtil.buildTorCallbacks(wsQueue)
    val _ = torConf.addCallbacks(torCallbacks)
    val isTorStartedF = if (torConf.torProvided) {
      //if tor is provided we need to emit a tor started event immediately
      torConf.callBacks.executeOnTorStarted()
    } else {
      Future.unit
    }
    val walletNameF = for {
      lastLoadedWallet <- getLastLoadedWalletName()
      walletName = lastLoadedWallet.getOrElse(
        WalletAppConfig.DEFAULT_WALLET_NAME)
    } yield walletName

    val walletHolder = new WalletHolder()
    val chainCallbacksF = for {
      bitcoind <- bitcoindF
    } yield {
      WebsocketUtil.buildChainCallbacks(wsQueue, bitcoind)
    }
    val nodeApiF = for {
      bitcoind <- bitcoindF
      chainCallbacks <- chainCallbacksF
    } yield BitcoindRpcBackendUtil.buildBitcoindNodeApi(bitcoind,
                                                        walletHolder,
                                                        Some(chainCallbacks))

    val feeProviderF = bitcoindF.map { bitcoind =>
      FeeProviderFactory.getFeeProviderOrElse(
        bitcoind,
        feeProviderNameStrOpt = conf.walletConf.feeProviderNameOpt,
        feeProviderTargetOpt = conf.walletConf.feeProviderTargetOpt,
        proxyParamsOpt = torConf.socks5ProxyParams,
        network = network
      )
    }

    val loadWalletFnF = {
      for {
        bitcoind <- bitcoindF
        nodeApi <- nodeApiF
        feeProvider <- feeProviderF
        fn = loadWalletBitcoindBackend(walletHolder,
                                       nodeApi,
                                       bitcoind,
                                       feeProvider)(_, _)
      } yield fn
    }

    val walletF: Future[(WalletHolder, WalletAppConfig, DLCAppConfig)] = {
      for {
        _ <- isTorStartedF
        loadWalletFn <- loadWalletFnF
        walletName <- walletNameF
        result <- loadWalletFn(Some(walletName), conf.walletConf.aesPasswordOpt)
      } yield result
    }

    val dlcNodeF = {
      for {
        (wallet, _, _) <- walletF
        dlcNode = dlcNodeConf.createDLCNode(wallet)
        _ <- dlcNode.start()
      } yield dlcNode
    }

    for {
      bitcoind <- bitcoindF
      bitcoindNetwork <- getBlockChainInfo(bitcoind).map(_.chain)
      _ = require(
        bitcoindNetwork == network,
        s"bitcoind ($bitcoindNetwork) on different network than wallet ($network)")
      loadWalletFn <- loadWalletFnF
      _ <- startHttpServer(
        nodeApiF = Future.successful(bitcoind),
        chainApi = bitcoind,
        walletF = walletF.map(_._1),
        dlcNodeF = dlcNodeF,
        torConfStarted = startedTorConfigF,
        serverCmdLineArgs = serverArgParser,
        wsSource = wsSource,
        loadWalletFn
      )
      walletName <- walletNameF
      walletCallbacks = WebsocketUtil.buildWalletCallbacks(wsQueue, walletName)
      chainCallbacks <- chainCallbacksF
      (wallet, walletConfig, dlcConfig) <- walletF
      _ = walletConfig.addCallbacks(walletCallbacks)

      //intentionally doesn't map on this otherwise we
      //wait until we are done syncing the entire wallet
      //which could take 1 hour
      _ = syncWalletWithBitcoindAndStartPolling(bitcoind,
                                                wallet,
                                                walletConfig,
                                                Some(chainCallbacks))
      dlcWalletCallbacks = WebsocketUtil.buildDLCWalletCallbacks(wsQueue)
      _ = dlcConfig.addCallbacks(dlcWalletCallbacks)
      _ <- startedTorConfigF
      _ <- handleDuplicateSpendingInfoDb(wallet, walletConfig)
      _ <- restartRescanIfNeeded(wallet)
    } yield {
      logger.info(s"Done starting Main!")
      ()
    }
  }

  private var serverBindingsOpt: Option[ServerBindings] = None

  private def startHttpServer(
      nodeApiF: Future[NodeApi],
      chainApi: ChainApi,
      walletF: Future[WalletHolder],
      dlcNodeF: Future[DLCNode],
      torConfStarted: Future[Unit],
      serverCmdLineArgs: ServerArgParser,
      wsSource: Source[WsNotification[_], NotUsed],
      loadWalletFn: (Option[String], Option[AesPassword]) => Future[
        (WalletHolder, WalletAppConfig, DLCAppConfig)])(implicit
      system: ActorSystem,
      conf: BitcoinSAppConfig): Future[Server] = {
    implicit val nodeConf: NodeAppConfig = conf.nodeConf
    implicit val walletConf: WalletAppConfig = conf.walletConf

    val walletRoutesF = {
      walletF.map { w =>
        WalletRoutes(w)(loadWalletFn)
      }
    }
    val nodeRoutesF = nodeApiF.map(NodeRoutes(_))
    val chainRoutes =
      ChainRoutes(chainApi, nodeConf.network, torConfStarted)
    val coreRoutes = CoreRoutes()
    val dlcRoutesF = dlcNodeF.map(DLCRoutes(_))
    val commonRoutes = CommonRoutes(conf.baseDatadir)

    val handlers =
      Seq(walletRoutesF,
          nodeRoutesF,
          Future.successful(chainRoutes),
          Future.successful(coreRoutes),
          dlcRoutesF,
          Future.successful(commonRoutes))

    val rpcBindConfOpt = serverCmdLineArgs.rpcBindOpt match {
      case Some(rpcbind) => Some(rpcbind)
      case None          => conf.rpcBindOpt
    }

    val wsBindConfOpt = serverCmdLineArgs.wsBindOpt match {
      case Some(wsbind) => Some(wsbind)
      case None         => conf.wsBindOpt
    }

    val wsPort = serverCmdLineArgs.wsPortOpt match {
      case Some(wsPort) => wsPort
      case None         => conf.wsPort
    }

    val wsServerConfig =
      WsServerConfig(wsBindConfOpt.getOrElse("localhost"), wsPort = wsPort)

    val server = {
      serverCmdLineArgs.rpcPortOpt match {
        case Some(rpcport) =>
          Server(conf = nodeConf,
                 handlersF = handlers,
                 rpcbindOpt = rpcBindConfOpt,
                 rpcport = rpcport,
                 rpcPassword = conf.rpcPassword,
                 wsConfigOpt = Some(wsServerConfig),
                 wsSource)
        case None =>
          Server(
            conf = nodeConf,
            handlersF = handlers,
            rpcbindOpt = rpcBindConfOpt,
            rpcport = conf.rpcPort,
            rpcPassword = conf.rpcPassword,
            wsConfigOpt = Some(wsServerConfig),
            wsSource
          )
      }
    }
    val bindingF = server.start()

    bindingF.map { bindings =>
      serverBindingsOpt = Some(bindings)
      server
    }
  }

  /** Syncs the bitcoin-s wallet against bitcoind and then
    * starts rpc polling if zmq isn't enabled, otherwise it starts zmq polling.
    *
    * The key thing this helper method does is it logs errors based on the
    * future returned by this method. This is needed because we don't want
    * to block the rest of the application from starting if we have to
    * do a ton of syncing. However, we don't want to swallow
    * exceptions thrown by this method.
    */
  private def syncWalletWithBitcoindAndStartPolling(
      bitcoind: BitcoindRpcClient,
      wallet: WalletApi with NeutrinoWalletApi,
      walletConfig: WalletAppConfig,
      chainCallbacksOpt: Option[ChainCallbacks]): Future[Unit] = {
    val f = for {
      _ <- AsyncUtil.retryUntilSatisfiedF(
        conditionF = { () =>
          for {
            bitcoindHeight <- bitcoind.getBlockCount
            walletStateOpt <- wallet.getSyncDescriptorOpt()
          } yield walletStateOpt.forall(bitcoindHeight >= _.height)
        },
        // retry for approximately 2 hours
        mode = Exponential,
        interval = 1.second,
        maxTries = 12
      )
      _ <- BitcoindRpcBackendUtil.syncWalletToBitcoind(
        bitcoind,
        wallet,
        chainCallbacksOpt)(system, walletConfig)
      _ <- wallet.updateUtxoPendingStates()
      _ <-
        if (bitcoindRpcConf.zmqConfig == ZmqConfig.empty) {
          BitcoindRpcBackendUtil
            .startBitcoindBlockPolling(wallet, bitcoind, chainCallbacksOpt)
            .map { _ =>
              BitcoindRpcBackendUtil
                .startBitcoindMempoolPolling(wallet, bitcoind) { tx =>
                  nodeConf.callBacks
                    .executeOnTxReceivedCallbacks(logger, tx)
                }
              ()
            }
        } else {
          Future {
            BitcoindRpcBackendUtil.startZMQWalletCallbacks(
              wallet,
              bitcoindRpcConf.zmqConfig)
          }
        }
    } yield ()

    f.failed.foreach(err =>
      logger.error(s"Error syncing bitcoin-s wallet with bitcoind", err))
    f
  }

  /** Builds a websocket queue that you can feed elements to.
    * The Source can be wired up with Directives.handleWebSocketMessages
    * to create a flow that emits websocket messages
    */
  private def buildWsSource: (
      SourceQueueWithComplete[WsNotification[_]],
      Source[WsNotification[_], NotUsed]) = {
    val maxBufferSize: Int = 25

    /** This will queue [[maxBufferSize]] elements in the queue. Once the buffer size is reached,
      * we will drop the first element in the buffer
      */
    val tuple = {
      //from: https://github.com/akka/akka-http/issues/3039#issuecomment-610263181
      //the BroadcastHub.sink is needed to avoid these errors
      // 'Websocket handler failed with Processor actor'
      Source
        .queue[WsNotification[_]](maxBufferSize, OverflowStrategy.dropHead)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
    }

    //need to drain the websocket queue if no one is connected
    val _: Future[Done] = tuple._2.runWith(Sink.ignore)

    tuple
  }

  private def handleDuplicateSpendingInfoDb(
      wallet: AnyDLCHDWalletApi,
      walletConfig: WalletAppConfig): Future[Unit] = {
    val spendingInfoDAO = SpendingInfoDAO()(ec, walletConfig)
    for {
      rescanNeeded <- spendingInfoDAO.hasDuplicates()
      _ <-
        if (rescanNeeded) {
          logger.warn("Found duplicate UTXOs. Rescanning...")
          wallet
            .rescanNeutrinoWallet(startOpt = None,
                                  endOpt = None,
                                  addressBatchSize =
                                    wallet.discoveryBatchSize(),
                                  useCreationTime = true,
                                  force = true)
            .recover { case scala.util.control.NonFatal(exn) =>
              logger.error(s"Failed to handleDuplicateSpendingInfoDb rescan",
                           exn)
              RescanState.RescanDone
            }
        } else {
          Future.successful(RescanState.RescanDone)
        }
      _ <- spendingInfoDAO.createOutPointsIndexIfNeeded()
    } yield ()
  }

  private def restartRescanIfNeeded(
      wallet: AnyDLCHDWalletApi): Future[RescanState] = {
    for {
      isRescanning <- wallet.isRescanning()
      res <-
        if (isRescanning)
          wallet.rescanNeutrinoWallet(startOpt = None,
                                      endOpt = None,
                                      addressBatchSize =
                                        wallet.discoveryBatchSize(),
                                      useCreationTime = true,
                                      force = true)
        else Future.successful(RescanState.RescanDone)
    } yield res
  }

  private lazy val nodeStateDAO: NodeStateDescriptorDAO =
    NodeStateDescriptorDAO()(ec, nodeConf)

  private def getLastLoadedWalletName(): Future[Option[String]] = {
    nodeStateDAO
      .getWalletName()
      .recover { case _: SQLException => None }
      .map(_.map(_.walletName))
  }

  private def updateWalletName(walletNameOpt: Option[String]): Future[Unit] = {
    nodeStateDAO.updateWalletName(walletNameOpt)
  }

  private def updateWalletConfigs(
      walletName: String,
      aesPasswordOpt: Option[Option[AesPassword]]): Future[
    (WalletAppConfig, DLCAppConfig)] = {
    val kmConfigF = Future.successful(
      conf.walletConf.kmConf.copy(walletNameOverride = Some(walletName),
                                  aesPasswordOverride = aesPasswordOpt))

    (for {
      kmConfig <- kmConfigF
      _ = if (!kmConfig.seedExists())
        throw new RuntimeException(s"Wallet `${walletName}` does not exist")

      // First thing start the key manager to be able to fail fast if the password is invalid
      _ <- kmConfig.start()

      walletConfig = conf.walletConf.copy(kmConfOpt = Some(kmConfig))
      dlcConfig = conf.dlcConf.copy(walletConfigOpt = Some(walletConfig))
    } yield (walletConfig, dlcConfig))
  }

  /** Loads a wallet, if no wallet name is given the default wallet is loaded */
  private def loadNeutrinoWallet(walletHolder: WalletHolder)(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword])(implicit
      ec: ExecutionContext): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)] = {
    val nodeCallbacksF =
      CallbackUtil.createNeutrinoNodeCallbacksForWallet(walletHolder)
    val replacedNodeCallbacks = for {
      nodeCallbacks <- nodeCallbacksF
      _ = nodeConf.replaceCallbacks(nodeCallbacks)
    } yield ()

    val nodeApi = walletHolder.nodeApi
    val chainQueryApi = walletHolder.chainQueryApi
    val feeRateApi = walletHolder.feeRateApi
    for {
      _ <- replacedNodeCallbacks
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        walletHolder = walletHolder,
        chainQueryApi = chainQueryApi,
        nodeApi = nodeApi,
        feeProviderApi = feeRateApi,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt
      )
      _ <- walletHolder.replaceWallet(dlcWallet)
      _ <- updateWalletName(walletNameOpt)
    } yield (walletHolder, walletConfig, dlcConfig)
  }

  private def loadWalletBitcoindBackend(
      walletHolder: WalletHolder,
      nodeApi: NodeApi,
      bitcoind: BitcoindRpcClient,
      feeProvider: FeeRateApi)(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword])(implicit
      ec: ExecutionContext): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)] = {

    for {
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        walletHolder = walletHolder,
        chainQueryApi = bitcoind,
        nodeApi = nodeApi,
        feeProviderApi = feeProvider,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt)

      nodeCallbacks <- CallbackUtil.createBitcoindNodeCallbacksForWallet(
        walletHolder)
      _ = nodeConf.addCallbacks(nodeCallbacks)
      _ <- walletHolder.replaceWallet(dlcWallet)
    } yield (walletHolder, walletConfig, dlcConfig)
  }

  private def loadWallet(
      walletHolder: WalletHolder,
      chainQueryApi: ChainQueryApi,
      nodeApi: NodeApi,
      feeProviderApi: FeeRateApi,
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (AnyDLCHDWalletApi, WalletAppConfig, DLCAppConfig)] = {
    logger.info(
      s"Loading wallet with bitcoind backend, walletName=${walletNameOpt.getOrElse("DEFAULT")}")
    val walletName =
      walletNameOpt.getOrElse(WalletAppConfig.DEFAULT_WALLET_NAME)

    for {
      (walletConfig, dlcConfig) <- updateWalletConfigs(walletName,
                                                       Some(aesPasswordOpt))
        .recover { case _: Throwable => (conf.walletConf, conf.dlcConf) }
      _ <- {
        if (walletHolder.isInitialized) {
          walletHolder
            .stop()
            .map(_ => ())
        } else {
          Future.unit
        }
      }
      _ <- walletConfig.start()
      _ <- dlcConfig.start()
      dlcWallet <- dlcConfig.createDLCWallet(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi,
        feeRateApi = feeProviderApi
      )(walletConfig, ec)
    } yield (dlcWallet, walletConfig, dlcConfig)
  }
}

object BitcoinSServerMain extends BitcoinSAppScalaDaemon {

  override val actorSystemName =
    s"bitcoin-s-server-${System.currentTimeMillis()}"

  /** Directory specific for current network or custom dir */
  override val customFinalDirOpt: Option[String] = None

  val serverCmdLineArgs = ServerArgParser(args.toVector)

  val datadirParser =
    DatadirParser(serverCmdLineArgs, customFinalDirOpt)

  System.setProperty("bitcoins.log.location", datadirParser.networkDir.toString)

  implicit lazy val conf: BitcoinSAppConfig =
    BitcoinSAppConfig(
      datadirParser.datadir,
      Vector(datadirParser.baseConfig, serverCmdLineArgs.toConfig))(system)

  val m = new BitcoinSServerMain(serverCmdLineArgs)

  m.run()

  sys.addShutdownHook {
    logger.info(
      s"@@@@@@@@@@@@@@@@@@@@@ Shutting down ${getClass.getSimpleName} @@@@@@@@@@@@@@@@@@@@@")
    Await.result(m.stop(), 10.seconds)
  }
}
