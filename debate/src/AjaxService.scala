package debate

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.JsonCodec

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotKleisli

trait AjaxService[F[_]] {
  def getDebaters: F[Set[String]]
}
object AjaxService {
  def apply[F[_]](f: DotKleisli[F, Request]) =
    new AjaxService[F] {
      def getDebaters: F[Set[String]] = f(Request.GetDebaters)
    }

  @JsonCodec
  sealed trait Request {
    type Out
  }
  object Request {
    case object GetDebaters extends Request {
      type Out = Set[String]
    }

    implicit val ajaxServiceRequestDotEncoder =
      new DotEncoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case GetDebaters =>
                implicitly[Encoder[Set[String]]]
            }
          res.asInstanceOf[Encoder[req.Out]]
        }
      }
    implicit val ajaxServiceRequestDotDecoder =
      new DotDecoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case GetDebaters =>
                implicitly[Decoder[Set[String]]]
            }
          res.asInstanceOf[Decoder[req.Out]]
        }
      }
  }
}
