package debate

import scala.concurrent.Future

import cats.implicits._
import cats.~>

import io.circe.generic.JsonCodec
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import jjm.io.HttpUtil
import jjm.ui.Mounting

import org.scalajs.jquery.jQuery
// import debate.facades.jQuery
import debate.util._
import japgolly.scalajs.react.extra.StateSnapshot

import Utils.ClassSetInterpolator
import jjm.ui.CacheCallContent
import jjm.OrWrapped

@JsonCodec
case class ConnectionSpec(isOfficial: Boolean, roomName: String, participantName: String)

/** The main webapp. */
object App {

  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  val MainWebSocket = WebSocketConnection2.forJsonString[MainChannelRequest, Option[Lobby]]

  def mainWebsocketUri(userName: String): String =
    s"${Utils.wsProtocol}//${dom.document.location.host}/main-ws?name=$userName"

  val httpProtocol = dom.document.location.protocol
  val qualityApiUrl: String =
    s"$httpProtocol//${dom.document.location.host}/$qualityServiceApiEndpoint"
  type DelayedFuture[A] = () => Future[A]
  val toAsyncCallback = λ[DelayedFuture ~> AsyncCallback](f => AsyncCallback.fromFuture(f()))
  val qualityService = quality.QuALITYService(
    HttpUtil
      .makeHttpPostClient[quality.QuALITYService.Request](qualityApiUrl)
      .andThenK(toAsyncCallback)
  )

  val DebatersFetch = new CacheCallContent[Unit, Set[String]]

  // Shortcuts for styles and view elements

  val S = Styles
  val V = new jjm.ui.View(S)

  // instantiate the HOCs we need

  val StringOptField = V
    .LiveTextField[Option[String]](x => Some(Option(x).filter(_.nonEmpty)), _.getOrElse(""))
  val IntOptField = V.LiveTextField[Option[Int]](
    x =>
      if (x.isEmpty)
        Option(None)
      else
        scala.util.Try(x.toInt).toOption.map(Option(_)),
    _.foldMap(_.toString)
  )

  val noProfileString = "(select profile)"

  def setChoice(userName: StateSnapshot[Option[String]])(name: String) = {
    val adjustedName =
      if (name == noProfileString)
        None
      else
        Some(name)
    userName.setState(adjustedName)
  }

  def curChoice(profiles: Set[String], profile: Option[String]) = profile
    .filter(profiles.contains)
    .getOrElse(noProfileString)

  def profileSelector(
    profiles: Set[String],
    isAdmin: StateSnapshot[Boolean],
    profile: StateSnapshot[Option[String]]
  ) =
    <.div(c"form-group row")(
      <.label(c"col-sm-2 col-form-label")("Profile:", ^.onClick --> isAdmin.modState(!_)),
      V.Select
        .String
        .modFull(TagMod(c"col-sm-10", S.customSelect))(
          choices = noProfileString +: profiles.toList.sorted,
          curChoice = curChoice(profiles, profile.value),
          setChoice = setChoice(profile)
        )
    )

  val Component =
    ScalaComponent
      .builder[Unit]("Full UI")
      .render { _ =>
        <.div(S.app)(
          Local[Boolean].syncedWithSessionStorage(key = "is-admin", defaultValue = false) { isAdmin =>
            Local[Option[String]].syncedWithLocalStorage("profile", None) { profile =>
              profile.value match {
                case None =>
                  DebatersFetch.make(
                    request = (),
                    sendRequest = _ => OrWrapped.pure(Set("Julian Michael", "David Rein"))
                  ) { profilesFetch =>
                    <.div(c"container")(
                      profilesFetch match {
                        case DebatersFetch.Loading =>
                          <.div("Loading profiles...")
                        case DebatersFetch.Loaded(profiles) =>
                          profileSelector(profiles, isAdmin = isAdmin, profile = profile)
                      }
                    )
                  }
                case Some(userName) =>
                  Local[Lobby].make(Lobby.empty) { lobby =>
                    MainWebSocket.make(
                      mainWebsocketUri(userName),
                      onOpen = _ => Callback.empty,
                      onMessage = (_, msg: Option[Lobby]) => msg.foldMap(lobby.setState(_))
                    ) {
                      case MainWebSocket.Disconnected(reconnect, reason) =>
                        Mounting.make(
                          AsyncCallback.unit.delayMs(5000).completeWith(_ => reconnect)
                        )(
                          <.div(S.loading)(
                            """You've been disconnected. Will attempt to reconnect every 5 seconds.
                     If you don't reconnect after a few seconds,
                     Please refresh the page. """ + reason
                          )
                        )
                      case MainWebSocket.Connecting =>
                        <.div(S.loading)("Connecting to metadata server...")
                      case MainWebSocket.Connected(sendToMainChannel) =>
                        Local[Option[ConnectionSpec]].syncedWithSessionStorage(
                          key = "connection-details",
                          defaultValue = None
                        ) { connectionSpecOpt =>
                          connectionSpecOpt.value match {
                            case None =>
                              view
                                .lobby
                                .LobbyPage(
                                  qualityService = qualityService,
                                  lobby = lobby.value,
                                  sendToMainChannel = sendToMainChannel,
                                  connect =
                                    (cs: ConnectionSpec) => connectionSpecOpt.setState(Some(cs)),
                                  isAdmin = isAdmin,
                                  logout = profile.setState(None),
                                  userName = userName
                                )
                            case Some(cs: ConnectionSpec) =>
                              view
                                .debate
                                .DebatePage(
                                  profiles = lobby.value.trackedDebaters,
                                  connectionSpec = cs,
                                  disconnect = connectionSpecOpt.setState(None)
                                )
                          }
                        }
                    }
                  }
              }
            }
          }
        )
      }
      .build

  def setupUI(): Unit = {
    Styles.addToDocument()
    Component().renderIntoDOM(org.scalajs.dom.document.getElementById(appDivId))
  }

  // @JSExportTopLevel("main")
  final def main(args: Array[String]): Unit = jQuery { () =>
    dom.experimental.Notification.requestPermission(_ => ())
    setupUI()
  }
}
