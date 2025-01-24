package org.bitcoins.server.routes

import de.heikoseeberger.akkahttpupickle.UpickleSupport._
import org.apache.pekko.{Done, NotUsed}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.event.Logging
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ws.{Message, TextMessage}
import org.apache.pekko.http.scaladsl.model.{
  ContentTypes,
  HttpEntity,
  HttpResponse,
  StatusCode,
  StatusCodes
}
import org.apache.pekko.http.scaladsl.server.Directives.{
  authenticateBasic,
  complete,
  handleExceptions,
  handleRejections
}
import org.apache.pekko.http.scaladsl.server.directives.{
  Credentials,
  DebuggingDirectives,
  MarshallingDirectives,
  MethodDirectives,
  PathDirectives
}
import org.apache.pekko.http.scaladsl.server.directives.Credentials.Missing
import org.apache.pekko.http.scaladsl.server.{
  Directive1,
  Directives,
  ExceptionHandler,
  RejectionHandler,
  Route,
  StandardRoute
}
import org.apache.pekko.http.scaladsl.unmarshalling.FromRequestUnmarshaller
import org.apache.pekko.stream.scaladsl.{Flow, Keep, Sink, Source}
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.commons.jsonmodels.ws.WsNotification
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.server.util.{ServerBindings, WsServerConfig}
import upickle.{default => up}

import scala.concurrent.Future

case class Server(
    conf: AppConfig,
    handlersF: Seq[Future[ServerRoute]],
    rpcbindOpt: Option[String],
    rpcport: Int,
    rpcPassword: String,
    wsConfigOpt: Option[WsServerConfig],
    wsSource: Source[WsNotification[?], NotUsed]
)(implicit system: ActorSystem)
    extends HttpLogger {

  import system.dispatcher

  if (rpcPassword.isEmpty) {
    if (rpchost == "localhost" || rpchost == "127.0.0.1") {
      logger.warn(s"RPC password is not set (rpchost=$rpchost)")
    } else {
      require(
        rpcPassword.nonEmpty,
        s"RPC password must be set (rpchost=$rpchost)"
      )
    }
  }

  /** Handles all server commands by throwing a MethodNotFound */
  private val catchAllHandler: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand(name, _) => throw HttpError.MethodNotFound(name)
  }

  /** HTTP directive that handles both exceptions and rejections */
  private def withErrorHandling(route: Route): Route = {

    val rejectionHandler =
      RejectionHandler
        .newBuilder()
        .handleNotFound {
          complete {
            Server.httpError(
              """Resource not found. Hint: all RPC calls are made against root ('/')""",
              StatusCodes.BadRequest
            )
          }
        }
        .result()

    val exceptionHandler = ExceptionHandler {
      case HttpError.MethodNotFound(method) =>
        complete(
          Server.httpError(
            s"'$method' is not a valid method",
            StatusCodes.BadRequest
          )
        )
      case err: Throwable =>
        logger.error(s"Unhandled error in server:", err)
        complete(Server.httpError(err.getMessage))
    }

    handleRejections(rejectionHandler) {
      handleExceptions(exceptionHandler) {
        route
      }
    }
  }

  def rpchost: String = rpcbindOpt.getOrElse("localhost")

  private def authenticator(credentials: Credentials): Option[Done] = {
    credentials match {
      case p @ Credentials.Provided(_) =>
        if (p.verify(rpcPassword)) Some(Done) else None
      case Missing => None
    }
  }

  private val serverCmdDirective: FromRequestUnmarshaller[ServerCommand] =
    MarshallingDirectives.as[ServerCommand]

  private val initF =
    Future.successful(PartialFunction.empty[ServerCommand, Route])

  private def handlerF: Future[PartialFunction[ServerCommand, Route]] = {
    handlersF.foldLeft(initF) { case (accumF, currF) =>
      for {
        accum <- accumF
        newAccum <-
          if (currF.isCompleted) {
            currF.map(curr => accum.orElse(curr.handleCommand))
          } else {
            Future.successful(accum)
          }
      } yield {
        newAccum
      }
    }
  }

  val route: Route = {

    val commonRoute: Route = {
      withErrorHandling {
        PathDirectives.pathSingleSlash {
          MethodDirectives.post { ctx =>
            val route: Future[Route] = {
              for {
                handler <- handlerF
              } yield {
                MarshallingDirectives.entity(serverCmdDirective) { cmd =>
                  val i = handler.orElse(catchAllHandler).apply(cmd)
                  i
                }
              }
            }
            route.flatMap(_.apply(ctx))
          }
        }
      }
    }

    val authDirectiveOpt: Option[Directive1[Done]] = {
      if (rpcPassword.isEmpty) {
        None
      } else {
        Some(authenticateBasic("auth", authenticator))
      }
    }
    val authenticatedRoute: Route = authDirectiveOpt match {
      case Some(authDirective) =>
        authDirective { case _ =>
          commonRoute
        }
      case None => commonRoute
    }

    DebuggingDirectives.logRequestResult(
      ("http-rpc-server", Logging.DebugLevel)
    ) {
      authenticatedRoute
    }
  }

  def start(): Future[ServerBindings] = {
    val httpFut = for {
      http <- Http()
        .newServerAt(rpchost, rpcport)
        .bindFlow(route)
    } yield http

    httpFut.foreach { http =>
      logger.info(s"Started Bitcoin-S HTTP server at ${http.localAddress}")
    }
    val wsFut = startWsServer()

    for {
      http <- httpFut
      ws <- wsFut
    } yield ServerBindings(http, ws)
  }

  private def startWsServer(): Future[Option[Http.ServerBinding]] = {
    wsConfigOpt match {
      case Some(wsConfig) =>
        val httpFut =
          Http()
            .newServerAt(wsConfig.wsBind, wsConfig.wsPort)
            .bindFlow(wsRoutes)
        httpFut.foreach { http =>
          logger.info(s"Started Bitcoin-S websocket at ${http.localAddress}")
        }
        httpFut.map(Some(_))
      case None =>
        Future.successful(None)
    }
  }

  private val eventsRoute = "events"

  private def wsRoutes: Route = {
    val commonRoute = Directives.path(eventsRoute) {
      Directives.handleWebSocketMessages(wsHandler)
    }

    if (rpcPassword.isEmpty) {
      commonRoute
    } else {
      authenticateBasic("auth", authenticator) { _ =>
        commonRoute
      }
    }
  }

  private val notificationToMsgFn: WsNotification[?] => Message = {
    notification =>
      val msg = TextMessage.Strict(notification.json.toString())
      msg
  }
  private val notificationToMsgFlow = Flow.fromFunction(notificationToMsgFn)

  private val msgSource: Source[Message, NotUsed] = {
    wsSource.viaMat(notificationToMsgFlow)(Keep.right)
  }

  private def wsHandler: Flow[Message, Message, Any] = {
    Flow.fromSinkAndSource(Sink.ignore, msgSource)
  }

}

object Server extends BitcoinSLogger {

  // TODO id parameter
  case class Response(
      result: Option[ujson.Value] = None,
      error: Option[String] = None
  ) {

    def toJsonMap: Map[String, ujson.Value] = {
      Map(
        "result" -> (result match {
          case None      => ujson.Null
          case Some(res) => res
        }),
        "error" -> (error match {
          case None      => ujson.Null
          case Some(err) => err
        })
      )
    }
  }

  /** Creates a HTTP response with the given body as a JSON response */
  def httpSuccess[T](
      body: T
  )(implicit writer: up.Writer[T]): HttpEntity.Strict = {
    val response = Response(result = Some(up.writeJs(body)))
    HttpEntity(
      ContentTypes.`application/json`,
      up.write(response.toJsonMap)
    )
  }

  def httpSuccessOption[T](
      bodyOpt: Option[T]
  )(implicit writer: up.Writer[T]): HttpEntity.Strict = {
    val response = Response(result = bodyOpt.map(body => up.writeJs(body)))
    HttpEntity(
      ContentTypes.`application/json`,
      up.write(response.toJsonMap)
    )
  }

  def httpError(msg: String): HttpEntity.Strict = {
    val entity = {
      val response = Response(error = Some(msg))
      HttpEntity(
        ContentTypes.`application/json`,
        up.write(response.toJsonMap)
      )
    }
    entity
  }

  def httpBadRequest(ex: Throwable): HttpResponse = {
    logger.info(s"Http bad request", ex)
    val msg = ex.getMessage
    val entity = httpError(msg)
    HttpResponse(status = StatusCodes.BadRequest, entity = entity)
  }

  def httpError(msg: String, status: StatusCode): HttpResponse = {
    logger.error(s"Http error msg=$msg statusCode=$status")
    val entity = {
      val response = Response(error = Some(msg))
      HttpEntity(
        ContentTypes.`application/json`,
        up.write(response.toJsonMap)
      )
    }

    HttpResponse(status = status, entity = entity)
  }
}
