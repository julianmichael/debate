package debate
package service

import cats.kernel.Order

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.JsonCodec

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotKleisli

import debate.quality.QuALITYStory
import debate.util.SparseDistribution

@JsonCodec
case class QuALITYStoryMetadata(
  articleId: String,
  title: String,
  splits: Set[String],
  source: String,
  numSingleTurnDebateMatches: Int,
  hasBeenDebated: Boolean
)
object QuALITYStoryMetadata {
  implicit def qualityStoryMetadataOrder = Order
    .by[QuALITYStoryMetadata, (String, String)](m => m.title -> m.articleId)
}

trait AjaxService[F[_]] extends DotKleisli[F, AjaxService.Request] {
  def getDebaters: F[Map[String, Profile]]
  def getSourceMaterialIndex: F[Map[String, QuALITYStoryMetadata]]
  def getStoryAndMatches(articleId: String): F[(QuALITYStory, Set[String])]
  def sampleSchedule(
    workloadDist: SparseDistribution[String],
    ruleDist: SparseDistribution[RuleConfig],
    articleId: String,
    questionIds: Set[String],
    numDebatesPerQuestion: Int
  ): F[Option[Vector[DebateSetup]]]

  import AjaxService.Request
  def apply(req: Request): F[req.Out] = {
    val res =
      req match {
        case Request.GetDebaters =>
          getDebaters
        case Request.GetSourceMaterialIndex =>
          getSourceMaterialIndex
        case Request.GetStoryAndMatches(articleId) =>
          getStoryAndMatches(articleId)
        case Request
              .SampleSchedule(workload, ruleDist, articleId, questionIds, numDebatesPerQuestion) =>
          sampleSchedule(workload, ruleDist, articleId, questionIds, numDebatesPerQuestion)
      }
    res.asInstanceOf[F[req.Out]]
  }
}
object AjaxService {
  def apply[F[_]](f: DotKleisli[F, Request]) =
    new AjaxService[F] {
      def getDebaters: F[Map[String, Profile]] = f(Request.GetDebaters)
      def getSourceMaterialIndex: F[Map[String, QuALITYStoryMetadata]] = f(
        Request.GetSourceMaterialIndex
      )
      def getStoryAndMatches(articleId: String): F[(QuALITYStory, Set[String])] = f(
        Request.GetStoryAndMatches(articleId)
      )
      def sampleSchedule(
        workloadDist: SparseDistribution[String],
        ruleDist: SparseDistribution[RuleConfig],
        articleId: String,
        questionIds: Set[String],
        numDebatesPerQuestion: Int
      ): F[Option[Vector[DebateSetup]]] = f(
        Request
          .SampleSchedule(workloadDist, ruleDist, articleId, questionIds, numDebatesPerQuestion)
      )
    }

  @JsonCodec
  sealed trait Request {
    type Out
  }
  object Request {
    case object GetDebaters extends Request {
      type Out = Map[String, Profile]
    }
    case object GetSourceMaterialIndex extends Request {
      type Out = Map[String, QuALITYStoryMetadata]
    }
    case class GetStoryAndMatches(articleId: String) extends Request {
      type Out = (QuALITYStory, Set[String])
    }
    case class SampleSchedule(
      workloadDist: SparseDistribution[String],
      ruleDist: SparseDistribution[RuleConfig],
      articleId: String,
      questionIds: Set[String],
      numDebatesPerQuestion: Int
    ) extends Request {
      type Out = Option[Vector[DebateSetup]]
    }

    implicit val ajaxServiceRequestDotEncoder =
      new DotEncoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case GetDebaters =>
                implicitly[Encoder[Map[String, Profile]]]
              case GetSourceMaterialIndex =>
                implicitly[Encoder[Map[String, QuALITYStoryMetadata]]]
              case GetStoryAndMatches(_) =>
                implicitly[Encoder[(QuALITYStory, Set[String])]]
              case SampleSchedule(_, _, _, _, _) =>
                implicitly[Encoder[Option[Vector[DebateSetup]]]]
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
              case GetSourceMaterialIndex =>
                implicitly[Decoder[Map[String, QuALITYStoryMetadata]]]
              case GetStoryAndMatches(_) =>
                implicitly[Decoder[(QuALITYStory, Set[String])]]
              case SampleSchedule(_, _, _, _, _) =>
                implicitly[Decoder[Option[Vector[DebateSetup]]]]
            }
          res.asInstanceOf[Decoder[req.Out]]
        }
      }
  }
}