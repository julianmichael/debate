package debate

import scala.util.Try

import cats.ApplicativeError
import cats.effect.Sync
import cats.implicits._

import io.circe.Json
import io.circe.generic.JsonCodec
import org.http4s._
import org.http4s.client.Client

import jjm.DotKleisli

object Slack {
  @JsonCodec
  sealed trait Request {
    type Out
  }
  object Request {
    case class LookupByEmail(email: String) extends Request {
      type Out = String
    }
    case class PostMessage(channel: String, text: String) extends Request {
      type Out = Unit
    }
    // implicit val slackRequestDotEncoder =
    //   new DotEncoder[Request] {
    //     def apply(req: Request) = {
    //       val res =
    //         req match {
    //           case LookupByEmail(_) =>
    //             implicitly[Encoder[String]]
    //           case PostMessage(_, _) =>
    //             implicitly[Encoder[Unit]]
    //         }
    //       res.asInstanceOf[Encoder[req.Out]]
    //     }
    //   }

    // implicit val slackRequestDotDecoder =
    //   new DotDecoder[Request] {
    //     def apply(req: Request) = {
    //       val res =
    //         req match {
    //           case LookupByEmail(_) =>
    //             implicitly[Decoder[String]]
    //           case PostMessage(_, _) =>
    //             implicitly[Decoder[Unit]]
    //         }
    //       res.asInstanceOf[Decoder[req.Out]]
    //     }
    //   }
  }

  sealed trait Service[F[_]] extends DotKleisli[F, Request] {
    def apply(req: Request): F[req.Out] = {
      val res =
        req match {
          case Request.LookupByEmail(email) =>
            lookupByEmail(email)
          case Request.PostMessage(channel, text) =>
            postMessage(channel, text)
        }
      res.asInstanceOf[F[req.Out]]
    }
    def lookupByEmail(email: String): F[String]
    def postMessage(channel: String, text: String): F[Unit]

    def sendMessage(profiles: Map[String, Profile], debater: String, msg: String)(
      implicit F: Sync[F]
    ): F[Unit] = {

      val notify = profiles
        .get(debater)
        .flatMap(_.slackEmail)
        .traverse(lookupByEmail)
        .flatMap(_.traverse_(id => postMessage(id, msg)))

      notify.recoverWith { case e: Throwable =>
        Sync[F].delay {
          println(s"Slack notification failed for $debater")
          // println(s"Email: $email")
          println(s"Message: ${e.getMessage()}")
          println(s"Stack trace: ")
          e.printStackTrace()
        }
      }

    }
  }
  object Service {
    def fromDotKleisli[F[_]](dk: DotKleisli[F, Request]): Service[F] =
      new Service[F] {
        override def apply(req: Request): F[req.Out] = dk(req)
        def lookupByEmail(email: String): F[String]  = dk(Request.LookupByEmail(email))
        def postMessage(channel: String, text: String): F[Unit] = dk(
          Request.PostMessage(channel, text)
        )
      }

    import org.http4s.{Request => HttpRequest}
    import org.http4s.circe._
    import org.http4s.implicits._

    def httpRequestFactory[F[_]](token: String) = {
      val headers = Headers.of(Header("Authorization", s"Bearer $token"))
      new Service[Constant[HttpRequest[F], *]] {
        def lookupByEmail(email: String) = HttpRequest[F](
          method = Method.GET,
          uri = uri"https://slack.com/api/users.lookupByEmail".withQueryParam("email", email),
          headers = headers
        )
        def postMessage(channel: String, text: String) = HttpRequest[F](
          method = Method.POST,
          uri = uri"https://slack.com/api/chat.postMessage"
            .withQueryParam("channel", channel)
            .withQueryParam("text", text),
          headers = headers
        )
      }
    }

    // since we're not directly using DotEncoder/DotDecoder
    def extractFromResponseJson[F[_]: ApplicativeError[*[_], Throwable]] =
      new Service[Lambda[A => Json => F[A]]] {
        def lookupByEmail(email: String) =
          responseJson =>
            ApplicativeError[F, Throwable].fromTry(
              Try(
                responseJson.asObject.get("user").get.asObject.get("id").get.as[String].toOption.get
              )
            )
        def postMessage(channel: String, text: String) =
          _ => ApplicativeError[F, Throwable].pure(())
      }

    def fullHttpClient[F[_]: Sync](client: Client[F], token: String) = {
      def log(msg: String) = Sync[F].delay(println(s"[[[SLACK]]] $msg"))
      val getHttpReq       = httpRequestFactory[F](token)
      Slack
        .Service
        .fromDotKleisli(
          new DotKleisli[F, Request] {
            def apply(req: Request): F[req.Out] = {
              val httpRequest = getHttpReq(req)
              log(s"Sending request: $httpRequest") >>
                client
                  .expect[Json](httpRequest)
                  .flatMap { responseJson =>
                    log(s"Response: ${responseJson.noSpaces}")
                      .flatMap(_ => extractFromResponseJson[F].apply(req)(responseJson))
                  }
            }

          }
        )
    }
  }
}
