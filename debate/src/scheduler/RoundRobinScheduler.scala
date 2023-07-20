package debate
package scheduler

import cats.implicits._
import cats.data.NonEmptySet

import io.circe.generic.JsonCodec

import jjm.Duad
import jjm.implicits._

import debate.quality.QuALITYStory
import scala.util.Random
import cats.kernel.Monoid

@JsonCodec
case class RoundRobinStorySchedule(
  qa: RoundRobinStorySchedule.QASpec,
  debater1: Profile.Human,
  debater2: Profile.Human,
  ai: Profile.AI,
  judges: Vector[Profile.Human] // 10 of these
) {
  import RoundRobinStorySchedule._
  def debaters = debater1 <-> debater2
  def getDebateSetups = {
    def makeDebate(
      honest: Option[Profile],
      dishonest: Option[Profile],
      judge: Profile.Human,
      honestFirst: Boolean
    ) = DebateSetupSpec(
      rules =
        if (List(honest, dishonest).flatten.size > 1)
          twoDebaterRules
        else
          singleDebaterRules,
      sourceMaterial = QuALITYSourceMaterialSpec(qa.articleId),
      question = qa.question,
      answers = {
        val as = Vector(qa.correctAnswer, qa.incorrectAnswer)
        if (honestFirst)
          as
        else
          as.reverse
      },
      correctAnswerIndex =
        if (honestFirst)
          0
        else
          1,
      roles =
        (
          (Judge -> judge.name) ::
            (
              if (honestFirst)
                List(honest, dishonest)
              else
                List(dishonest, honest)
            ).zipWithIndex
              .flatMap { case (debater, i) =>
                debater.map(p => Debater(i) -> p.name)
              }
        ).toMap,
      offlineJudges = Map.empty
    )

    Vector(
      makeDebate(Some(debater1), Some(debater2), judges(0), honestFirst = true),
      makeDebate(Some(debater2), Some(debater1), judges(1), honestFirst = false),
      makeDebate(Some(debater1), None, judges(2), honestFirst = false),
      makeDebate(None, Some(debater2), judges(3), honestFirst = true),
      makeDebate(Some(ai), Some(ai), judges(4), honestFirst = true),
      makeDebate(Some(ai), Some(ai), judges(5), honestFirst = false),
      makeDebate(Some(ai), None, judges(6), honestFirst = true),
      makeDebate(None, Some(ai), judges(7), honestFirst = false)
    )
  }
  import RoundRobinStorySchedule.JudgeLoad
  import JudgeLoad._
  def getJudgeLoads: Map[Profile.Human, JudgeLoad] = Map(
    judges(0) -> JudgeLoad(Map(TwoHumans -> 1), Map(debater1 -> 1, debater2 -> 1)),
    judges(1) -> JudgeLoad(Map(TwoHumans -> 1), Map(debater1 -> 1, debater2 -> 1)),
    judges(2) -> JudgeLoad(Map(OneHuman -> 1), Map(debater1 -> 1)),
    judges(3) -> JudgeLoad(Map(OneHuman -> 1), Map(debater2 -> 1)),
    judges(4) -> JudgeLoad(Map(TwoModels -> 1)),
    judges(5) -> JudgeLoad(Map(TwoModels -> 1)),
    judges(6) -> JudgeLoad(Map(OneModel -> 1)),
    judges(7) -> JudgeLoad(Map(OneModel -> 1))
  )
}
object RoundRobinStorySchedule {

  case class JudgeLoad(
    settingsJudged: Map[JudgeLoad.DebateSetting, Int],
    debatersJudged: Map[Profile.Human, Int] = Map()
  ) {
    def setting(setting: JudgeLoad.DebateSetting) = settingsJudged.getOrElse(setting, 0)
    def debaters(ds: Profile.Human*)              = ds.foldMap(d => debatersJudged.getOrElse(d, 0))
  }
  object JudgeLoad {
    sealed trait DebateSetting
    case object TwoHumans extends DebateSetting
    case object OneHuman  extends DebateSetting
    case object TwoModels extends DebateSetting
    case object OneModel  extends DebateSetting

    implicit val judgeLoadMonoid: Monoid[JudgeLoad] = cats.derived.semiauto.monoid[JudgeLoad]
  }

  @JsonCodec
  case class QASpec(
    articleId: String,
    questionId: String,
    question: String,
    correctAnswer: String,
    incorrectAnswer: String
  )

  val twoDebaterRules = DebateRules(
    fixedOpening = Vector(
      DebateRoundType.JudgeFeedbackRound(true, 750),
      DebateRoundType.SimultaneousSpeechesRound(750, Some(250), false),
      DebateRoundType.JudgeFeedbackRound(true, 750)
    ),
    repeatingStructure = Vector(
      DebateRoundType.SequentialSpeechesRound(750, Some(250), false),
      DebateRoundType.JudgeFeedbackRound(true, 750)
    ),
    fixedClosing = None,
    globalQuoteRestriction = None,
    scoringFunction = ScoringFunction.LogScoreWithLinearPenalty.default
  )
  val singleDebaterRules = DebateRules(
    fixedOpening = Vector(DebateRoundType.JudgeFeedbackRound(true, 750)),
    repeatingStructure = Vector(
      DebateRoundType.SequentialSpeechesRound(1500, Some(500), true),
      DebateRoundType.JudgeFeedbackRound(true, 750)
    ),
    fixedClosing = None,
    globalQuoteRestriction = None,
    scoringFunction = ScoringFunction.LogScoreWithLinearPenalty.default
  )
}

object RoundRobinScheduler {
  import RoundRobinStorySchedule.QASpec

  def getQuestionForStory(
    story: QuALITYStory,
    maxSpeedAccuracy: Double,
    minAverageContextScore: Double
  ): Option[QASpec] = story
    .questions
    .values
    .view
    .flatMap { question =>
      for {
        annotations <- question.annotations
        if annotations.goldLabel == annotations.writerLabel
        if annotations.untimedAccuracyAgainstGold == 1.0
        if annotations.speedAccuracyAgainstGold <= maxSpeedAccuracy
        if annotations.averageContextScore >= minAverageContextScore
        bestDistractors = annotations.bestDistractors(correctAnswerIndex = annotations.goldLabel)
      } yield {
        if (bestDistractors.size > 1) {
          // import io.circe.syntax._
          // System.err.println(s"Multiple best distractors: ${question.asJson.spaces2}")
        }
        QASpec(
          articleId = story.articleId,
          questionId = question.questionUniqueId,
          question = question.question,
          correctAnswer = question.options(annotations.goldLabel - 1),
          incorrectAnswer = question.options(bestDistractors.head - 1)
        ) -> annotations.averageContextScore
      }
    }
    .toVector
    .sortBy(-_._2)
    .headOption
    .map(_._1)

  def renderAssignmentText(setup: DebateSetup): String =
    s"""|Title:     ${setup.sourceMaterial.title}
        |Question:  ${setup.question}
        |Debaters:
        |  A: ${setup.roles.get(Debater(0)).getOrElse("<none>")}${if (setup.correctAnswerIndex == 0)
         " (H)"
       else
         ""}
        |  B: ${setup.roles.get(Debater(1)).getOrElse("<none>")}${if (setup.correctAnswerIndex == 1)
         " (H)"
       else
         ""}
        |Judges:${setup.roles.get(Judge).foldMap(j => s"\n  $j (Live)")}
        |  ${setup.offlineJudges.keySet.mkString("\n|  ")}
    """.stripMargin.trim

  // this fully specifies a set of debates for a question
  // in the round-robin scheduling setting

  def scheduleRoundRobin(
    eligibleStories: Vector[QuALITYStory],
    qualityMatches: Map[String, NonEmptySet[String]],
    sourceFilters: StoryAndQuestionFilters,
    existingSchedules: Vector[RoundRobinStorySchedule],
    debaterPairsToSchedule: Set[Duad[Profile.Human]],
    judges: Set[Profile.Human],
    aiDebater: Profile.AI,
    rand: Random
  ): Either[String, Vector[RoundRobinStorySchedule]] = {
    debaterPairsToSchedule
      .toVector
      .foldLeftM(Vector.empty[RoundRobinStorySchedule]) { case (curSchedules, debaters) =>
        val allCurSchedules = existingSchedules ++ curSchedules
        val debatersPair    = (debaters.min, debaters.max)
        val (debater1, debater2) =
          if (rand.nextBoolean())
            debatersPair
          else
            debatersPair.swap
        val chosenJudges = {
          import RoundRobinStorySchedule.JudgeLoad
          val baseVector = rand.shuffle(
            (
              allCurSchedules.foldMap(sched => sched.getJudgeLoads) |+|
                judges
                  .map(_ -> Monoid[JudgeLoad].empty)
                  .toMap // shuffle before sorting to randomize order of judges and break ties randomly
            ).toVector
          )
          case class Acc(
            currentAssignment: Vector[Profile.Human],
            remainingChoices: Vector[(Profile.Human, JudgeLoad)]
          )
          def loop(acc: Acc): Acc =
            acc match {
              case Acc(currentAssignment, remainingChoices) =>
                if (currentAssignment.size == 8)
                  return acc
                else {
                  import JudgeLoad._
                  val nextJudge =
                    currentAssignment.size match {
                      case 0 | 1 =>
                        remainingChoices.minBy { case (_, load) =>
                          load.setting(TwoHumans) + load.debaters(debater1, debater2)
                        }
                      case 2 | 3 =>
                        remainingChoices.minBy { case (_, load) =>
                          load.setting(OneHuman) + load.debaters(debater1, debater2)
                        }
                      case 4 | 5 =>
                        remainingChoices.minBy { case (_, load) =>
                          load.setting(TwoModels)
                        }
                      case 6 | 7 =>
                        remainingChoices.minBy { case (_, load) =>
                          load.setting(OneModel)
                        }
                    }
                  Acc(
                    currentAssignment :+ nextJudge._1,
                    remainingChoices.filterNot(_._1 == nextJudge._1)
                  )
                }
            }
          // independently randomly assign honest/dishonest
          loop(Acc(Vector.empty, baseVector))
            .currentAssignment
            .grouped(2)
            .map(rand.shuffle(_))
            .toVector
            .flatten
        }

        for {
          qa <- eligibleStories
            .view
            .filterNot(s => allCurSchedules.exists(_.qa.articleId == s.articleId))
            .flatMap { story =>
              val matches = qualityMatches.get(story.articleId).foldMap(_.toSortedSet)
              story
                .questions
                .values
                .toVector
                .filter(q =>
                  sourceFilters
                    .admitsQuestion(q, matches, Set()) // TODO: fill in questions debated so far
                )
                .flatMap(q => q.annotations.map(q -> _))
                .map { case (question, annotations) =>
                  val bestDistractors = annotations
                    .bestDistractors(correctAnswerIndex = annotations.goldLabel)
                  QASpec(
                    articleId = story.articleId,
                    questionId = question.questionUniqueId,
                    question = question.question,
                    correctAnswer = question.options(annotations.goldLabel - 1),
                    incorrectAnswer = question.options(bestDistractors.head - 1)
                  ) -> annotations.averageContextScore
                }
                .sortBy(-_._2)
                .map(_._1)
                .headOption
            }
            .headOption
            .toRight("No eligible questions left.")
        } yield curSchedules :+
          RoundRobinStorySchedule(
            qa = qa,
            debater1 = debater1,
            debater2 = debater2,
            ai = aiDebater,
            judges = chosenJudges
          )
      }
  }
}
