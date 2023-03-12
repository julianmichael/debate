package debate

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.JsonCodec

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotKleisli

trait AjaxService[F[_]] extends DotKleisli[F, AjaxService.Request] {
  def getDebaters: F[Map[String, Profile]]

  import AjaxService.Request
  def apply(req: Request): F[req.Out] = {
    val res =
      req match {
        case Request.GetDebaters =>
          getDebaters
      }
    res.asInstanceOf[F[req.Out]]
  }
}
object AjaxService {
  def apply[F[_]](f: DotKleisli[F, Request]) =
    new AjaxService[F] {
      def getDebaters: F[Map[String, Profile]] = f(Request.GetDebaters)
    }

  @JsonCodec
  sealed trait Request {
    type Out
  }
  object Request {
    case object GetDebaters extends Request {
      type Out = Map[String, Profile]
    }

    implicit val ajaxServiceRequestDotEncoder =
      new DotEncoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case GetDebaters =>
                implicitly[Encoder[Map[String, Profile]]]
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
                implicitly[Decoder[Map[String, Profile]]]
            }
          res.asInstanceOf[Decoder[req.Out]]
        }
      }
  }
}
