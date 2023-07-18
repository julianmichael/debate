package debate
package scheduler

import cats.implicits._

import io.circe.generic.JsonCodec

import jjm.Duad
import jjm.implicits._

import debate.quality.QuALITYStory
import scala.util.Random

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
}
object RoundRobinStorySchedule {

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

  def getQuestionForStory(story: QuALITYStory): Option[QASpec] = story
    .questions
    .values
    .view
    .flatMap { question =>
      for {
        annotations <- question.annotations
        if annotations.goldLabel == annotations.writerLabel
        if annotations.untimedAccuracyAgainstGold == 1.0
        if annotations.speedAccuracyAgainstGold <= 0.5
        if annotations.averageContextScore >= 3.0
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
    eligibleStories: Set[QuALITYStory],
    existingSchedules: Vector[RoundRobinStorySchedule],
    debaterPairsToSchedule: Set[Duad[Profile.Human]],
    judges: Set[Profile.Human],
    aiDebater: Profile.AI,
    rand: Random
  ): Either[String, Vector[RoundRobinStorySchedule]] =
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
        val chosenJudges = rand
          .shuffle(
            (
              allCurSchedules.foldMap(sched =>
                sched
                  .judges
                  .map(_ -> Map(None -> 1, Some(sched.debater1) -> 1, Some(sched.debater2) -> 1))
                  .toMap
              ) |+| judges.map(_ -> Map.empty[Option[Profile.Human], Int]).toMap
            ).toVector // shuffle before sorting to randomize order of judges and break ties randomly
          )
          .sortBy(p =>
            p._2.getOrElse(None, 0) -> // prefer judges who have judged less
              (p._2.getOrElse(Some(debater1), 0) + p._2.getOrElse(Some(debater2), 0))
          // and among those, prefer those who have judged these debaters less
          )
          .map(_._1)
          .take(8)
        for {
          qa <- eligibleStories
            .view
            .filterNot(s => allCurSchedules.exists(_.qa.articleId == s.articleId))
            .flatMap(getQuestionForStory)
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
