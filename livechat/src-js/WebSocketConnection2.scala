package livechat

import scalajs.js.typedarray.ArrayBuffer

import org.scalajs.dom.WebSocket
import org.scalajs.dom.raw.Blob
import org.scalajs.dom.raw.CloseEvent
import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.FileReader

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

class WebSocketConnection2[Request, Response](
  sendRequest: (WebSocket, Request) => Callback,
  readResponse: ArrayBuffer => Response) {

  sealed trait State
  case class ConnectedState(socket: WebSocket) extends State

  sealed trait Context
  case class Connected(send: Request => Callback, setCallback: (Response => Callback) => Callback) extends Context

  case class Disconnected(reconnect: Callback, reason: String) extends State with Context
  case object Connecting extends State with Context

  case class Props(
    websocketURI: String,
    onOpen: (Request => Callback) => Callback,
    onMessage: (((Request => Callback), Response) => Callback),
    render: Context => VdomElement
  )

  class Backend(scope: BackendScope[Props, State]) {

    var messageCallbackOpt: Option[Response => Callback] = None

    def connect(props: Props): Callback = scope.state >>= {
      case ConnectedState(_) => Callback(System.err.println("Already connected."))
      case Disconnected(_, _) | Connecting => scope.setState(Connecting) >> Callback {
        val socket = new WebSocket(props.websocketURI)
        val send = (r: Request) => sendRequest(socket, r)
        socket.onopen = { (event: Event) =>
          (scope.setState(ConnectedState(socket)) >> props.onOpen(send)).runNow()
        }
        socket.onerror = { (event: Event) =>
          val msg = s"WebSocket connection failure. Error: ${event}"
          System.err.println(msg)
          scope.setState(Disconnected(connect(props), msg)).runNow()
        }
        // socket.onmessage = { (event: MessageEvent) =>
        //   props.onMessage(send, responseFromString(event.data.toString)).runNow()
        // }
        socket.onmessage = { (event: MessageEvent) =>
          var reader = new FileReader();
          reader.addEventListener(
            "loadend", (e: Event) => {
              // reader.result contains the contents of blob as an ArrayBuffer
              // println(reader.result) // XXX
              val message = readResponse(reader.result.asInstanceOf[ArrayBuffer])
              // println(message) // XXX
              // messageCallbackOpt.foreach(messageCallback =>
              //   messageCallback(message).runNow()
              // )
              props.onMessage(send, message).runNow()
            });
          // println(event.data) // XXX
          reader.readAsArrayBuffer(event.data.asInstanceOf[Blob]);
        }
        socket.onclose = { (event: CloseEvent) =>
          val cleanly = if (event.wasClean) "cleanly" else "uncleanly"
          val msg = s"WebSocket connection closed $cleanly with code ${event.code}. reason: ${event.reason}"
          if(!event.wasClean) {
            System.err.println(msg)
          }
          // will trigger a warning if closure was done with unmount i think
          scope.setState(Disconnected(connect(props), msg)).runNow()
        }
      }
    }

    def close(s: State): Callback = s match {
      case Disconnected(_, _) => Callback.empty
      case Connecting => Callback.empty
      case ConnectedState(socket) => Callback(socket.close(1000))
    }

    def render(props: Props, s: State) =
      props.render(
        s match {
          case x @ Disconnected(_, _) => (x: Context)
          case x @ Connecting => Connecting
          case x @ ConnectedState(socket) => Connected(
            (req: Request) => sendRequest(socket, req),
            (cb: (Response => Callback)) => Callback { messageCallbackOpt = Some(cb) }
          ): Context
        }
      )
  }

  val Component = ScalaComponent
    .builder[Props]("WebSocket")
    .initialState(Connecting: State)
    .renderBackend[Backend]
    .componentDidMount(context => context.backend.connect(context.props))
    .componentWillUnmount(context => context.backend.close(context.state))
    .build

  def make(
    websocketURI: String,
    onOpen: (Request => Callback) => Callback = _ => Callback.empty,
    onMessage: (((Request => Callback), Response) => Callback) = ((_, _) => Callback.empty))(
    render: Context => VdomElement
  ) = Component(Props(websocketURI, onOpen, onMessage, render))
}
