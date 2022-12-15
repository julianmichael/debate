package debate
package util

import org.scalajs.dom.WebSocket
import org.scalajs.dom.raw.CloseEvent
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.raw.Event

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

import monocle.macros.Lenses
import monocle.macros.GenPrism

import cats.implicits._
import io.circe.Encoder
import io.circe.Decoder

/** HOC for websocket connections that manage a state variable synced between
  * multiple clients.
  *
  * @param sendRequest
  * @param readResponse
  * @param getRequestFromState
  */
class SyncedState[Request, Response, State](
    sendRequest: (WebSocket, Request) => Callback,
    readResponse: MessageEvent => Option[Response],
    getRequestFromState: State => Request,
    getStateUpdateFromResponse: Response => Option[State] => State,
) {

  sealed trait FullState
    @Lenses case class ConnectedState(socket: WebSocket, stateOpt: Option[State]) extends FullState

    sealed trait Context

    case class Disconnected(reconnect: Callback, reason: String)
        extends FullState with Context
    case object Connecting extends FullState with Context

    // case class Waiting(sendRequest: Request => Callback) extends Context
    case class Connected(sendRequest: Request => Callback, stateOpt: Option[StateSnapshot[State]]) extends Context

    object FullState {
      val connectedState = GenPrism[FullState, ConnectedState]
    }

    case class Props(
        websocketURI: String,
        didUpdate: (Option[State], State) => Callback,
        onOpen: (Request => Callback) => Callback,
        onMessage: ((Request => Callback), Response) => Callback,
        render: Context => VdomElement
    )

  class Backend(scope: BackendScope[Props, FullState]) {

    def connect(props: Props): Callback = scope.state >>= {
      case ConnectedState(_, _) =>
        Callback(System.err.println("Already connected."))
      case Disconnected(_, _) | Connecting =>
        scope.setState(Connecting) >> Callback {
          val socket = new WebSocket(props.websocketURI)
          val send = (r: Request) => sendRequest(socket, r)
          socket.onopen = { (_: Event) =>
            (scope.setState(ConnectedState(socket, None)) >> props.onOpen(send))
              .runNow()
          }
          socket.onerror = { (event: Event) =>
            val msg = s"WebSocket connection failure. Error: ${event}"
            System.err.println(msg)
            scope.setState(Disconnected(connect(props), msg)).runNow()
          }
          socket.onmessage = { (event: MessageEvent) =>
            // do nothing if the message was None, which means it's a keepalive
            val cb = readResponse(event).foldMap { response => 
              scope.modState(
                FullState.connectedState.composeLens(ConnectedState.stateOpt)
                  .modify(curState => Some(getStateUpdateFromResponse(response)(curState)))
              ) >> props.onMessage(send, response)
            }
            cb.runNow()
          }
          socket.onclose = { (event: CloseEvent) =>
            val cleanly = if (event.wasClean) "cleanly" else "uncleanly"
            val msg =
              s"WebSocket connection closed $cleanly with code ${event.code}. reason: ${event.reason}"
            if (!event.wasClean) {
              System.err.println(msg)
            }
            // will trigger a warning if closure was done with unmount i think
            scope.setState(Disconnected(connect(props), msg)).runNow()
          }
        }
    }

    def close(s: FullState): Callback = s match {
      case Disconnected(_, _)        => Callback.empty
      case Connecting                => Callback.empty
      case ConnectedState(socket, _) => Callback(socket.close(1000))
    }

    def render(props: Props, s: FullState) =
      props.render(
        s match {
          case x @ Disconnected(_, _) => (x: Context)
          case Connecting                          => Connecting
          case ConnectedState(socket, None)        => Connected(sendRequest(socket, _), None)
          case ConnectedState(socket, Some(state)) =>
            Connected(
              sendRequest(socket, _),
              Some(
                StateSnapshot(state)(
                  (stateOpt: Option[State], cb: Callback) => stateOpt.foldMap(
                    state => sendRequest(socket, getRequestFromState(state))
                  ) >> cb
                )
              )
            ): Context
        }
      )
  }

  val Component = ScalaComponent
    .builder[Props]("WebSocket")
    .initialState(Connecting: FullState)
    .renderBackend[Backend]
    .componentDidMount($ => $.backend.connect($.props))
    .componentWillUnmount($ => $.backend.close($.state))
    .componentDidUpdate($ =>
      ($.prevState, $.currentState) match {
        case (ConnectedState(_, prevStateOpt), ConnectedState(_, Some(curState))) =>
          $.currentProps.didUpdate(prevStateOpt, curState)
      case _ => Callback.empty
    }
  )
  .build


  def make(
      websocketURI: String,
      didUpdate: (Option[State], State) => Callback = (_, _) => Callback.empty,
      onOpen: (Request => Callback) => Callback = _ => Callback.empty,
      onMessage: (((Request => Callback), Response) => Callback) = ((_, _) => Callback.empty))(
      render: Context => VdomElement
  ) = Component(Props(websocketURI, didUpdate, onOpen, onMessage, render))

}
object SyncedState {

  def forString[Request, Response, State](
    sendRequest: (WebSocket, Request) => Callback,
    readResponse: String => Option[Response],
    getRequestFromState: State => Request,
    getStateUpdateFromResponse: Response => Option[State] => State
  ) = new SyncedState[
    Request, Response, State
  ](
    sendRequest = sendRequest,
    readResponse = (event: MessageEvent) => readResponse(event.data.toString),
    getRequestFromState = getRequestFromState,
    getStateUpdateFromResponse = getStateUpdateFromResponse
  )

  // TODO: handle parser errors
  import io.circe.syntax._
  def forJsonString[Request: Encoder, Response: Decoder, State](
    getRequestFromState: State => Request,
    getStateUpdateFromResponse: Response => Option[State] => State
  ) = new SyncedState[
    Request, Response, State
  ](
    sendRequest = (socket: WebSocket, r: Request) => Callback(socket.send(r.asJson.noSpaces)),
    readResponse = (event: MessageEvent) => io.circe.parser.decode[Option[Response]](event.data.toString).toOption.get,
    getRequestFromState = getRequestFromState,
    getStateUpdateFromResponse = getStateUpdateFromResponse
  )
}
