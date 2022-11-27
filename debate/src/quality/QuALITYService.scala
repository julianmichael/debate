package debate.quality

import jjm.DotKleisli
import jjm.{DotEncoder, DotDecoder}

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

trait QuALITYService[F[_]] {
  def getIndex: F[Map[String, String]]
  def getStory(articleId: String): F[QuALITYStory]
}
object QuALITYService {
  def apply[F[_]](f: DotKleisli[F, Request]) = new QuALITYService[F] {
    def getIndex: F[Map[String, String]] = f(Request.GetIndex)
    def getStory(articleId: String): F[QuALITYStory] = f(Request.GetStory(articleId))
  }

  @JsonCodec sealed trait Request { type Out }
  object Request {
    case object GetIndex extends Request {
      type Out = Map[String, String] // article IDs to titles
    }
    @JsonCodec case class GetStory(articleId: String) extends Request {
      type Out = QuALITYStory
    }

    implicit val qualityServiceRequestDotEncoder = new DotEncoder[Request] {
      def apply(req: Request) = {
        val res = req match {
          case GetIndex => implicitly[Encoder[Map[String, String]]]
          case GetStory(_) => implicitly[Encoder[QuALITYStory]]
        }
        res.asInstanceOf[Encoder[req.Out]]
      }
    }
    implicit val qualityServiceRequestDotDecoder = new DotDecoder[Request] {
      def apply(req: Request) = {
        val res = req match {
          case GetIndex => implicitly[Decoder[Map[String, String]]]
          case GetStory(_) => implicitly[Decoder[QuALITYStory]]
        }
        res.asInstanceOf[Decoder[req.Out]]
      }
    }
  }
}
