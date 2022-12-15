package debate

import annotation.unused
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSImport}
import org.scalajs.jquery.JQueryStatic
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import scala.util.Try
import cats.~>
import cats.implicits._
import debate.util._
import jjm.OrWrapped

@js.native
@JSImport("jquery", JSImport.Namespace)
object jQuery extends JQueryStatic

/** The main webapp. */
object App {
  val DebateWebSocket =
    WebSocketConnection2.forJsonString[DebateState, DebateState]
  val SyncedDebate = SyncedState
    .forJsonString[DebateStateUpdateRequest, DebateState, DebateState](
      getRequestFromState = DebateStateUpdateRequest.State(_),
      getStateUpdateFromResponse = responseState => _ => responseState
    )

  val MainWebSocket =
    WebSocketConnection2.forJsonString[MainChannelRequest, Option[Lobby]]

  val mainWebsocketUri: String = {
    s"${Helpers.wsProtocol()}//${dom.document.location.hostname}:8080/main-ws"
  }

  val httpProtocol = dom.document.location.protocol

  def wrap[F[_]] = λ[F ~> OrWrapped[F, *]] { f =>
    OrWrapped.wrapped(f)
  }

  import jjm.ui.LocalState

  val defaultRoomName: String =
    jQuery("#defaultRoomName").attr("value").toOption.getOrElse("")

  // Shortcuts for styles and view elements

  val S = Styles
  val V = new jjm.ui.View(S)

  // instantiate the HOCs we need

  val LocalDouble = new LocalState[Double]
  val LocalConnectionSpecOpt = new LocalState2[Option[ConnectionSpec]]
  val LocalLobby = new LocalState[Lobby]

  val StringOptField = V.LiveTextField[Option[String]](
    x => Some(Option(x).filter(_.nonEmpty)),
    _.getOrElse("")
  )
  val IntOptField = V.LiveTextField[Option[Int]](
    x => if (x.isEmpty) Option(None) else Try(x.toInt).toOption.map(Option(_)),
    _.foldMap(_.toString)
  )

  val facilitatorPanel = new FacilitatorPanel(S, V)

  class Backend(@unused scope: BackendScope[Unit, Unit]) {

    /** Main render method. */
    def render(@unused props: Unit, @unused state: Unit) = {
      <.div(S.app)(
        LocalLobby.make(Lobby.init) { lobby =>
          MainWebSocket.make(
            mainWebsocketUri,
            onOpen = _ => Callback(println("Main socket opened.")),
            onMessage = (_, msg: Option[Lobby]) => msg.foldMap(lobby.setState(_))
          ) {
            case MainWebSocket.Disconnected(_, reason) =>
              <.div(S.loading)(
                """You've been disconnected. This is probably either because of a bug or
                    because the server is restarting. Please refresh the page.
                    Sorry about that.
                """ + reason
              )
            case MainWebSocket.Connecting =>
              <.div(S.loading)("Connecting to metadata server...")
            case MainWebSocket.Connected(sendToMainChannel) =>
              LocalConnectionSpecOpt.syncedWithLocalStorage(
                key = "connection-details",
                defaultValue = None
              ) { connectionSpecOpt =>
                connectionSpecOpt.value match {
                  case None =>
                    LobbyPage.make(
                      lobby = lobby,
                      sendToMainChannel = sendToMainChannel,
                      connect = (cs: ConnectionSpec) => connectionSpecOpt.setState(Some(cs))
                    )
                  case Some(cs: ConnectionSpec) =>
                    DebatePage.make(
                      profiles = lobby.value.trackedDebaters,
                      connectionSpec = cs,
                      disconnect = connectionSpecOpt.setState(None),
                    )
                }
              }
          }
        }
      )
    }
  }

  val Component = ScalaComponent
    .builder[Unit]("Full UI")
    .initialState(())
    .renderBackend[Backend]
    .build

  def setupUI(): Unit = {
    Styles.addToDocument()
    Component().renderIntoDOM(
      org.scalajs.dom.document.getElementById("app")
    )
  }

  @JSExportTopLevel("main")
  final def main(): Unit = jQuery { () =>
    dom.experimental.Notification.requestPermission(result =>
      dom.console.log(result)
    )
    setupUI()
  }
}
