package debate

import scala.annotation.unused
import scala.util.Try
import scala.scalajs.js.annotation.JSExportTopLevel

import cats.implicits._
import cats.~>

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import jjm.ui.Mounting

import debate.util._
import scala.concurrent.Future
import jjm.io.HttpUtil

/** The main webapp. */
object App {
  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  val DebateWebSocket = WebSocketConnection2.forJsonString[DebateState, DebateState]
  val SyncedDebate = SyncedState.forJsonString[DebateStateUpdateRequest, DebateState, DebateState](
    getRequestFromState = DebateStateUpdateRequest.State(_),
    getStateUpdateFromResponse = responseState => _ => responseState
  )

  val MainWebSocket = WebSocketConnection2.forJsonString[MainChannelRequest, Option[Lobby]]

  val mainWebsocketUri: String =
    s"${Helpers.wsProtocol}//${dom.document.location.hostname}:8080/main-ws"

  val httpProtocol = dom.document.location.protocol
  val qualityApiUrl: String =
    s"$httpProtocol//${dom.document.location.hostname}:8080/$qualityServiceApiEndpoint"
  type DelayedFuture[A] = () => Future[A]
  val toAsyncCallback = λ[DelayedFuture ~> AsyncCallback](f => AsyncCallback.fromFuture(f()))
  val qualityService = quality.QuALITYService(
    HttpUtil
      .makeHttpPostClient[quality.QuALITYService.Request](qualityApiUrl)
      .andThenK(toAsyncCallback)
  )

  import jjm.ui.LocalState

  // Shortcuts for styles and view elements

  val S = Styles
  val V = new jjm.ui.View(S)

  // instantiate the HOCs we need

  val LocalDouble            = new LocalState[Double]
  val LocalConnectionSpecOpt = new LocalState2[Option[ConnectionSpec]]
  val LocalLobby             = new LocalState[Lobby]

  val StringOptField = V
    .LiveTextField[Option[String]](x => Some(Option(x).filter(_.nonEmpty)), _.getOrElse(""))
  val IntOptField = V.LiveTextField[Option[Int]](
    x =>
      if (x.isEmpty)
        Option(None)
      else
        Try(x.toInt).toOption.map(Option(_)),
    _.foldMap(_.toString)
  )

  class Backend(
    @unused
    scope: BackendScope[Unit, Unit]
  ) {

    /** Main render method. */
    def render(
      @unused
      props: Unit,
      @unused
      state: Unit
    ) =
      <.div(S.app)(
        LocalLobby.make(Lobby.init) { lobby =>
          MainWebSocket.make(
            mainWebsocketUri,
            onOpen = _ => Callback.empty,
            onMessage = (_, msg: Option[Lobby]) => msg.foldMap(lobby.setState(_))
          ) {
            case MainWebSocket.Disconnected(reconnect, reason) =>
              Mounting.make(AsyncCallback.unit.delayMs(5000).completeWith(_ => reconnect))(
                <.div(S.loading)(
                  """You've been disconnected. Will attempt to reconnect every 5 seconds.
                     If you don't reconnect after a few seconds,
                     Please refresh the page. """ + reason
                )
              )
            case MainWebSocket.Connecting =>
              <.div(S.loading)("Connecting to metadata server...")
            case MainWebSocket.Connected(sendToMainChannel) =>
              LocalConnectionSpecOpt
                .syncedWithLocalStorage(key = "connection-details", defaultValue = None) {
                  connectionSpecOpt =>
                    connectionSpecOpt.value match {
                      case None =>
                        LobbyPage.make(
                          qualityService = qualityService,
                          lobby = lobby,
                          sendToMainChannel = sendToMainChannel,
                          connect = (cs: ConnectionSpec) => connectionSpecOpt.setState(Some(cs))
                        )
                      case Some(cs: ConnectionSpec) =>
                        DebatePage.make(
                          qualityService = qualityService,
                          profiles = lobby.value.trackedDebaters,
                          connectionSpec = cs,
                          disconnect = connectionSpecOpt.setState(None)
                        )
                    }
                }
          }
        }
      )
  }

  val Component =
    ScalaComponent.builder[Unit]("Full UI").initialState(()).renderBackend[Backend].build

  def setupUI(): Unit = {
    Styles.addToDocument()
    Component().renderIntoDOM(org.scalajs.dom.document.getElementById("app"))
  }

  @JSExportTopLevel("main")
  final def main(): Unit = jQuery { () =>
    dom.experimental.Notification.requestPermission(_ => ())
    setupUI()
  }
}
