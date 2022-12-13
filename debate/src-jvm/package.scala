package debate

import org.http4s.websocket.WebSocketFrame

import fs2.Pipe

import cats.effect.IO

trait PackagePlatformExtensions {

  /** URL prefix for serving static files. */
  val staticFilePrefix = "file"

  // websocket convenience functions
  val filterCloseFrames: Pipe[IO, WebSocketFrame, WebSocketFrame] =
    _.filter {
      case WebSocketFrame.Close(_) => false
      case _                       => true
    }

  import io.circe.syntax._
  import _root_.io.circe.parser.{decode => circeDecode}
  import _root_.io.circe.Encoder
  import _root_.io.circe.Decoder

  def pickleToWSFrame[A: Encoder](message: A): WebSocketFrame = {
    WebSocketFrame.Text(message.asJson.noSpaces)
  }
  // TODO: handle parsing errors
  def unpickleFromWSFrame[A: Decoder](frame: WebSocketFrame): A = {
    val res = for {
      str <- frame.data.decodeUtf8
      res2 <- circeDecode[A](str)
    } yield res2
    res.get
  }
}
