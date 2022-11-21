package debate

import org.http4s.websocket.WebSocketFrame

import fs2.Pipe

import cats.effect.IO


trait PackagePlatformExtensions {

  /** URL prefix for serving static files. */
  val staticFilePrefix = "file"

  // websocket convenience functions
  import boopickle.Default._

  def pickleToWSFrame[A: Pickler](message: A): WebSocketFrame = {
    WebSocketFrame.Binary(scodec.bits.ByteVector.view(Pickle.intoBytes(message)))
  }
  def unpickleFromWSFrame[A: Pickler](frame: WebSocketFrame): A = {
    Unpickle[A].fromBytes(frame.data.toByteBuffer)
  }

  val filterCloseFrames: Pipe[IO, WebSocketFrame, WebSocketFrame] =
    _.filter {
      case WebSocketFrame.Close(_) => false
      case _        => true
    }

  // NOTE: for json encode/decode if we end up wanting it
  // import _root_.io.circe.parser.{decode => circeDecode}
  // import _root_.io.circe.Encoder
  // import _root_.io.circe.Decoder

  // NOTE: maybe change to json pickling later if reading network message is a pain.
  // haven't bothered with this yet because not sure if it will break something.

  // def jsonPickleToWSFrame[A: Encoder](message: A): WebSocketFrame = {
  //     Text(message.asJson.noSpaces)
  //   }
  // def jsonUnpickleFromWSFrame[A: Decoder](frame: WebSocketFrame): IO[A] = {
  //   for {
  //     str <- IO.fromEither(frame.data.decodeUtf8)
  //     res <- IO.fromEither(circeDecode[A](str))
  //   } yield res
  // }
}
