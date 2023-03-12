package debate.quality

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.JsonCodec

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotKleisli

trait QuALITYService[F[_]] extends DotKleisli[F, QuALITYService.Request] {
  def getIndex: F[Map[String, String]]
  def getStory(articleId: String): F[QuALITYStory]

  import QuALITYService.Request
  def apply(req: Request): F[req.Out] = {
    val res =
      req match {
        case Request.GetIndex =>
          getIndex
        case Request.GetStory(articleId) =>
          getStory(articleId)
      }
    // not sure why it isn't inferring the type...
    res.asInstanceOf[F[req.Out]]
  }
}
object QuALITYService {
  def apply[F[_]](f: DotKleisli[F, Request]) =
    new QuALITYService[F] {
      def getIndex: F[Map[String, String]]             = f(Request.GetIndex)
      def getStory(articleId: String): F[QuALITYStory] = f(Request.GetStory(articleId))
    }

  @JsonCodec
  sealed trait Request {
    type Out
  }
  object Request {
    case object GetIndex extends Request {
      type Out = Map[String, String] // article IDs to titles
    }
    @JsonCodec
    case class GetStory(articleId: String) extends Request {
      type Out = QuALITYStory
    }

    implicit val qualityServiceRequestDotEncoder =
      new DotEncoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case GetIndex =>
                implicitly[Encoder[Map[String, String]]]
              case GetStory(_) =>
                implicitly[Encoder[QuALITYStory]]
            }
          res.asInstanceOf[Encoder[req.Out]]
        }
      }
    implicit val qualityServiceRequestDotDecoder =
      new DotDecoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case GetIndex =>
                implicitly[Decoder[Map[String, String]]]
              case GetStory(_) =>
                implicitly[Decoder[QuALITYStory]]
            }
          res.asInstanceOf[Decoder[req.Out]]
        }
      }
  }
}
