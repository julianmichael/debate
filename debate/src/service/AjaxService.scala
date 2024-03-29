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
import debate.scheduler.DebateScheduler
import debate.scheduler.RoundRobinStorySchedule
import debate.util.SparseDistribution
import jjm.Duad

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
    canJudge: Set[String],
    canDebate: Set[String],
    workloadDist: SparseDistribution[String],
    ruleDist: SparseDistribution[RuleConfig],
    articleId: String,
    questionIds: Set[String],
    numDebatesPerQuestion: Int,
    dontAssignNewReading: Boolean,
    enforceJudgingConstraints: Boolean,
    numUniqueDebatersConstraint: Option[Int]
  ): F[Either[String, Vector[DebateSetup]]]

  def sampleOfflineJudging(
    excludes: Set[String],
    maxNumJudgesForOnline: Int,
    maxNumJudgesForOffline: Int
  ): F[Either[String, DebateScheduler.OfflineJudgeSchedulingResult]]

  def scheduleRoundRobin(
    sourceFilters: StoryAndQuestionFilters,
    debaterPairsToSchedule: Set[Duad[Profile.Human]],
    judges: Set[Profile.Human],
    aiDebater: Profile.AI
  ): F[Either[String, Vector[RoundRobinStorySchedule]]]

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
        case Request.SampleSchedule(
              canJudge,
              canDebate,
              workload,
              ruleDist,
              articleId,
              questionIds,
              numDebatesPerQuestion,
              dontAssignNewReading,
              enforceJudgingConstraints,
              numUniqueDebatersConstraint
            ) =>
          sampleSchedule(
            canJudge,
            canDebate,
            workload,
            ruleDist,
            articleId,
            questionIds,
            numDebatesPerQuestion,
            dontAssignNewReading,
            enforceJudgingConstraints,
            numUniqueDebatersConstraint
          )
        case Request
              .SampleOfflineJudging(excludes, maxNumJudgesForOnline, maxNumJudgesForOffline) =>
          sampleOfflineJudging(excludes, maxNumJudgesForOnline, maxNumJudgesForOffline)
        case Request.ScheduleRoundRobin(sourceFilters, debaterPairsToSchedule, judges, aiDebater) =>
          scheduleRoundRobin(sourceFilters, debaterPairsToSchedule, judges, aiDebater)
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
        canJudge: Set[String],
        canDebate: Set[String],
        workloadDist: SparseDistribution[String],
        ruleDist: SparseDistribution[RuleConfig],
        articleId: String,
        questionIds: Set[String],
        numDebatesPerQuestion: Int,
        dontAssignNewReading: Boolean,
        enforceJudgingConstraints: Boolean,
        numUniqueDebatersConstraint: Option[Int]
      ): F[Either[String, Vector[DebateSetup]]] = f(
        Request.SampleSchedule(
          canJudge,
          canDebate,
          workloadDist,
          ruleDist,
          articleId,
          questionIds,
          numDebatesPerQuestion,
          dontAssignNewReading,
          enforceJudgingConstraints,
          numUniqueDebatersConstraint
        )
      )

      def sampleOfflineJudging(
        excludes: Set[String],
        maxNumJudgesForOnline: Int,
        maxNumJudgesForOffline: Int
      ): F[Either[String, DebateScheduler.OfflineJudgeSchedulingResult]] = f(
        Request.SampleOfflineJudging(excludes, maxNumJudgesForOnline, maxNumJudgesForOffline)
      )

      def scheduleRoundRobin(
        sourceFilters: StoryAndQuestionFilters,
        debaterPairsToSchedule: Set[Duad[Profile.Human]],
        judges: Set[Profile.Human],
        aiDebater: Profile.AI
      ): F[Either[String, Vector[RoundRobinStorySchedule]]] = f(
        Request.ScheduleRoundRobin(sourceFilters, debaterPairsToSchedule, judges, aiDebater)
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
      canJudge: Set[String],
      canDebate: Set[String],
      workloadDist: SparseDistribution[String],
      ruleDist: SparseDistribution[RuleConfig],
      articleId: String,
      questionIds: Set[String],
      numDebatesPerQuestion: Int,
      dontAssignNewReading: Boolean,
      enforceJudgingConstraints: Boolean,
      numUniqueDebatersConstraint: Option[Int]
    ) extends Request {
      type Out = Either[String, Vector[DebateSetup]]
    }
    case class SampleOfflineJudging(
      excludes: Set[String],
      maxNumJudgesForOnline: Int,
      maxNumJudgesForOffline: Int
    ) extends Request {
      type Out = Either[String, DebateScheduler.OfflineJudgeSchedulingResult]
    }

    case class ScheduleRoundRobin(
      sourceFilters: StoryAndQuestionFilters,
      debaterPairsToSchedule: Set[Duad[Profile.Human]],
      judges: Set[Profile.Human],
      aiDebater: Profile.AI
    ) extends Request {
      type Out = Either[String, Vector[RoundRobinStorySchedule]]
    }

    import io.circe.disjunctionCodecs._

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
              case SampleSchedule(_, _, _, _, _, _, _, _, _, _) =>
                implicitly[Encoder[Either[String, Vector[DebateSetup]]]]
              case SampleOfflineJudging(_, _, _) =>
                implicitly[Encoder[Either[String, DebateScheduler.OfflineJudgeSchedulingResult]]]
              case ScheduleRoundRobin(_, _, _, _) =>
                implicitly[Encoder[Either[String, Vector[RoundRobinStorySchedule]]]]
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
              case SampleSchedule(_, _, _, _, _, _, _, _, _, _) =>
                implicitly[Decoder[Either[String, Vector[DebateSetup]]]]
              case SampleOfflineJudging(_, _, _) =>
                implicitly[Decoder[Either[String, DebateScheduler.OfflineJudgeSchedulingResult]]]
              case ScheduleRoundRobin(_, _, _, _) =>
                implicitly[Decoder[Either[String, Vector[RoundRobinStorySchedule]]]]
            }
          res.asInstanceOf[Decoder[req.Out]]
        }
      }
  }
}
