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
    WebSocketFrame.Binary(
      scodec.bits.ByteVector.view(Pickle.intoBytes(message))
    )
  }
  def unpickleFromWSFrame[A: Pickler](frame: WebSocketFrame): A = {
    Unpickle[A].fromBytes(frame.data.toByteBuffer)
  }

  val filterCloseFrames: Pipe[IO, WebSocketFrame, WebSocketFrame] =
    _.filter {
      case WebSocketFrame.Close(_) => false
      case _                       => true
    }

}
