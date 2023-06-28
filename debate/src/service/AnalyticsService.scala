package debate
package service

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.JsonCodec

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotKleisli

trait AnalyticsService[F[_]] extends DotKleisli[F, AnalyticsService.Request] {
  def refresh: F[Unit]
  def getAnalyticsGraphNames: F[Vector[String]]
  def getAnalyticsGraph(name: String): F[Json]
  def getPersonalizedAnalyticsGraphNames: F[Vector[String]]
  def getPersonalizedAnalyticsGraph(user: String, name: String): F[Json]

  import AnalyticsService.Request
  def apply(req: Request): F[req.Out] = {
    val res =
      req match {
        case Request.Refresh =>
          refresh
        case Request.GetAnalyticsGraphNames =>
          getAnalyticsGraphNames
        case Request.GetAnalyticsGraph(name) =>
          getAnalyticsGraph(name)
        case Request.GetPersonalizedAnalyticsGraphNames =>
          getPersonalizedAnalyticsGraphNames
        case Request.GetPersonalizedAnalyticsGraph(user, name) =>
          getPersonalizedAnalyticsGraph(user, name)
      }
    res.asInstanceOf[F[req.Out]]
  }
}
object AnalyticsService {
  def apply[F[_]](f: DotKleisli[F, Request]) =
    new AnalyticsService[F] {
      def refresh: F[Unit]                          = f(Request.Refresh)
      def getAnalyticsGraphNames: F[Vector[String]] = f(Request.GetAnalyticsGraphNames)
      def getAnalyticsGraph(name: String): F[Json]  = f(Request.GetAnalyticsGraph(name))
      def getPersonalizedAnalyticsGraphNames: F[Vector[String]] = f(
        Request.GetPersonalizedAnalyticsGraphNames
      )
      def getPersonalizedAnalyticsGraph(user: String, name: String): F[Json] = f(
        Request.GetPersonalizedAnalyticsGraph(user, name)
      )
    }

  @JsonCodec
  sealed trait Request {
    type Out
  }
  object Request {
    case object Refresh extends Request {
      type Out = Unit
    }
    case object GetAnalyticsGraphNames extends Request {
      type Out = Vector[String]
    }
    case class GetAnalyticsGraph(name: String) extends Request {
      type Out = Json
    }

    case object GetPersonalizedAnalyticsGraphNames extends Request {
      type Out = Vector[String]
    }

    case class GetPersonalizedAnalyticsGraph(user: String, name: String) extends Request {
      type Out = Json
    }

    implicit val analyticsServiceRequestDotEncoder =
      new DotEncoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case Refresh =>
                implicitly[Encoder[Unit]]
              case GetAnalyticsGraphNames =>
                implicitly[Encoder[Vector[String]]]
              case GetAnalyticsGraph(_) =>
                implicitly[Encoder[Json]]
              case GetPersonalizedAnalyticsGraphNames =>
                implicitly[Encoder[Vector[String]]]
              case GetPersonalizedAnalyticsGraph(_, _) =>
                implicitly[Encoder[Json]]
            }
          res.asInstanceOf[Encoder[req.Out]]
        }
      }
    implicit val analyticsServiceRequestDotDecoder =
      new DotDecoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case Refresh =>
                implicitly[Decoder[Unit]]
              case GetAnalyticsGraphNames =>
                implicitly[Decoder[Vector[String]]]
              case GetAnalyticsGraph(_) =>
                implicitly[Decoder[Json]]
              case GetPersonalizedAnalyticsGraphNames =>
                implicitly[Decoder[Vector[String]]]
              case GetPersonalizedAnalyticsGraph(_, _) =>
                implicitly[Decoder[Json]]
            }
          res.asInstanceOf[Decoder[req.Out]]
        }
      }
  }
}
