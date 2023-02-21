package debate

import cats.effect.IO

import fs2.Pipe
import org.http4s.websocket.WebSocketFrame

trait PackagePlatformExtensions {

  /** URL prefix for serving static files. */
  val staticFilePrefix = "file"

  // websocket convenience functions
  val filterCloseFrames: Pipe[IO, WebSocketFrame, WebSocketFrame] =
    _.filter {
      case WebSocketFrame.Close(_) =>
        false
      case _ =>
        true
    }

  import io.circe.syntax._
  import _root_.io.circe.parser.{decode => circeDecode}
  import _root_.io.circe.Encoder
  import _root_.io.circe.Decoder

  def pickleToWSFrame[A: Encoder](message: A): WebSocketFrame = WebSocketFrame
    .Text(message.asJson.noSpaces)
  // TODO: handle parsing errors
  def unpickleFromWSFrame[A: Decoder](frame: WebSocketFrame): A = {
    val res =
      for {
        str <- IO.fromEither(frame.data.decodeUtf8)
        res <- IO.fromEither(circeDecode[A](str))
      } yield res
    res.unsafeRunSync()
  }
}
