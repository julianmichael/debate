// package debate.util

// import scalajs.js.typedarray.ArrayBuffer

// import org.scalajs.dom.WebSocket
// import org.scalajs.dom.raw.Blob
// import org.scalajs.dom.raw.CloseEvent
// import org.scalajs.dom.raw.MessageEvent
// import org.scalajs.dom.raw.Event
// import org.scalajs.dom.raw.FileReader

// import japgolly.scalajs.react.vdom.html_<^._
// import japgolly.scalajs.react._

// import scala.concurrent.Promise
// import scala.concurrent.ExecutionContext

// import cats.Id
// import io.circe.Encoder
// import io.circe.Decoder

// /** HOC for websocket connections.
//   *
//   * @param sendRequest
//   * @param readResponse
//   */
// class WebSocketConnection2[F[_], Request, Response](
//     sendRequest: (WebSocket, Request) => Callback,
//     readResponse: MessageEvent => F[Response]
// ) {

//   sealed trait State
//   case class ConnectedState(socket: WebSocket) extends State

//   sealed trait Context
//   case class Connected(
//       send: Request => Callback,
//   ) extends Context

//   case class Disconnected(reconnect: Callback, reason: String)
//       extends State
//       with Context
//   case object Connecting extends State with Context

//   case class Props(
//       websocketURI: String,
//       onOpen: (Request => Callback) => Callback,
//       onMessage: (((Request => Callback), F[Response]) => Callback),
//       render: Context => VdomElement
//   )

//   class Backend(scope: BackendScope[Props, State]) {

//     def connect(props: Props): Callback = scope.state >>= {
//       case ConnectedState(_) =>
//         Callback(System.err.println("Already connected."))
//       case Disconnected(_, _) | Connecting =>
//         scope.setState(Connecting) >> Callback {
//           val socket = new WebSocket(props.websocketURI)
//           val send = (r: Request) => sendRequest(socket, r)
//           socket.onopen = { (_: Event) =>
//             (scope.setState(ConnectedState(socket)) >> props.onOpen(send))
//               .runNow()
//           }
//           socket.onerror = { (event: Event) =>
//             val msg = s"WebSocket connection failure. Error: ${event}"
//             System.err.println(msg)
//             scope.setState(Disconnected(connect(props), msg)).runNow()
//           }
//           socket.onmessage = { (event: MessageEvent) =>
//             props.onMessage(send, readResponse(event)).runNow()
//           }
//           socket.onclose = { (event: CloseEvent) =>
//             val cleanly = if (event.wasClean) "cleanly" else "uncleanly"
//             val msg =
//               s"WebSocket connection closed $cleanly with code ${event.code}. reason: ${event.reason}"
//             if (!event.wasClean) {
//               System.err.println(msg)
//             }
//             // will trigger a warning if closure was done with unmount i think
//             scope.setState(Disconnected(connect(props), msg)).runNow()
//           }
//         }
//     }

//     def close(s: State): Callback = s match {
//       case Disconnected(_, _)     => Callback.empty
//       case Connecting             => Callback.empty
//       case ConnectedState(socket) => Callback(socket.close(1000))
//     }

//     def render(props: Props, s: State) =
//       props.render(
//         s match {
//           case x @ Disconnected(_, _) => (x: Context)
//           case Connecting         => Connecting
//           case ConnectedState(socket) =>
//             Connected(
//               (req: Request) => sendRequest(socket, req)
//             ): Context
//         }
//       )
//   }

//   val Component = ScalaComponent
//     .builder[Props]("WebSocket")
//     .initialState(Connecting: State)
//     .renderBackend[Backend]
//     .componentDidMount(context => context.backend.connect(context.props))
//     .componentWillUnmount(context => context.backend.close(context.state))
//     .build

//   def make(
//       websocketURI: String,
//       onOpen: (Request => Callback) => Callback = _ => Callback.empty,
//       onMessage: (((Request => Callback), F[Response]) => Callback) =
//         ((_, _) => Callback.empty)
//   )(
//       render: Context => VdomElement
//   ) = Component(Props(websocketURI, onOpen, onMessage, render))
// }
// object WebSocketConnection2 {
//   def forArrayBuffer[Request, Response](
//     sendRequest: (WebSocket, Request) => Callback,
//     readResponse: ArrayBuffer => Response)(
//     implicit ec: ExecutionContext
//   ) = new WebSocketConnection2[
//     AsyncCallback, Request, Response
//   ](
//     sendRequest = sendRequest,
//     readResponse = (event: MessageEvent) => {
//       val promise = Promise[Response]()
//       val reader = new FileReader();
//       reader.addEventListener(
//         "loadend",
//         (_: Event) => {
//           // reader.result contains the contents of blob as an ArrayBuffer
//           val message =
//             readResponse(reader.result.asInstanceOf[ArrayBuffer])
//           promise.success(message)
//           // props.onMessage(send, message).runNow()
//         }
//       );
//       reader.readAsArrayBuffer(event.data.asInstanceOf[Blob]);
//       AsyncCallback.fromFuture(promise.future)
//     }
//   )

//   def forString[Request, Response](
//     sendRequest: (WebSocket, Request) => Callback,
//     readResponse: String => Response
//   ) = new WebSocketConnection2[
//     Id, Request, Response
//   ](
//     sendRequest = sendRequest,
//     readResponse = (event: MessageEvent) => readResponse(event.data.toString)
//   )

//   // TODO: handle parser errors
//   import io.circe.syntax._
//   def forJsonString[Request: Encoder, Response: Decoder] = new WebSocketConnection2[
//     Id, Request, Response
//   ](
//     sendRequest = (socket: WebSocket, r: Request) => Callback(socket.send(r.asJson.noSpaces)),
//     readResponse = (event: MessageEvent) => io.circe.parser.decode[Response](event.data.toString).toOption.get
//   )
// }
