package debate

import scala.concurrent.Future

import cats.implicits._
import cats.~>

import io.circe.generic.JsonCodec
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.jquery.jQuery
import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import jjm.OrWrapped
import jjm.io.HttpUtil
import jjm.ui.CacheCallContent
import jjm.ui.Mounting

import debate.Utils.ClassSetInterpolator
import debate.service._
import debate.util._

@JsonCodec
case class ConnectionSpec(isOfficial: Boolean, roomName: String, participantName: String)

import scalajs.js
@js.native
@js.annotation.JSGlobal("window")
object Globals extends js.Object {
  var powerOverwhelming: js.Function0[Unit] = js.native
}

/** The main webapp. */
object App {

  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  val MainWebSocket = WebSocketConnection2.forJsonString[MainChannelRequest, Option[Lobby]]

  def mainWebsocketUri(userName: String): String =
    s"${Utils.wsProtocol}//${dom.document.location.host}/main-ws?name=$userName"

  def httpProtocol = dom.document.location.protocol
  type DelayedFuture[A] = () => Future[A]
  val toAsyncCallback = λ[DelayedFuture ~> AsyncCallback](f => AsyncCallback.fromFuture(f()))

  val qualityApiUrl: String =
    s"$httpProtocol//${dom.document.location.host}/$qualityServiceApiEndpoint"
  val qualityService = quality.QuALITYService(
    HttpUtil
      .makeHttpPostClient[quality.QuALITYService.Request](qualityApiUrl)
      .andThenK(toAsyncCallback)
  )

  val ajaxApiUrl: String = s"$httpProtocol//${dom.document.location.host}/$ajaxServiceApiEndpoint"
  val ajaxService = AjaxService(
    HttpUtil.makeHttpPostClient[AjaxService.Request](ajaxApiUrl).andThenK(toAsyncCallback)
  )
  val DebatersFetch = new CacheCallContent[Unit, Map[String, Profile]]

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
    // isAdmin: StateSnapshot[Boolean],
    profile: StateSnapshot[Option[String]]
  ) =
    <.div(c"form-group row")(
      <.label(c"col-sm-2 col-form-label")("Profile:"),
      V.Select
        .String
        .modFull(TagMod(c"col-sm-10", S.customSelect))(
          choices = noProfileString +: profiles.toList.sorted,
          curChoice = curChoice(profiles, profile.value),
          setChoice = setChoice(profile)
        )
    )

  def setAdminCallback(profile: StateSnapshot[Option[String]]) = Callback(
    Globals.powerOverwhelming = () => profile.setState(Some(adminUsername)).runNow()
  )

  val Component =
    ScalaComponent
      .builder[Unit]("Full UI")
      .render { _ =>
        <.div(S.app)(
          Local[Option[String]].syncedWithLocalStorage("profile", None) { profile =>
            val isAdmin = profile.value == Some(adminUsername)
            Mounting.make(setAdminCallback(profile)) {
              profile.value match {
                case None =>
                  DebatersFetch.make(
                    request = (),
                    sendRequest = _ => OrWrapped.wrapped(ajaxService.getDebaters)
                  ) { profilesFetch =>
                    <.div(c"container")(
                      profilesFetch match {
                        case DebatersFetch.Loading =>
                          <.div("Loading profiles...")
                        case DebatersFetch.Loaded(profiles) =>
                          profileSelector(profiles.keySet, profile = profile)
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
                                  profiles = lobby.value.profiles,
                                  connectionSpec = cs,
                                  disconnect = connectionSpecOpt.setState(None),
                                  sendToMainChannel = sendToMainChannel
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
