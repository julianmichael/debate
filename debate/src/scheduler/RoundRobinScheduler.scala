package debate
package scheduler

import scala.util.Random

import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.data.StateT
import cats.implicits._
import cats.kernel.Monoid

import io.circe.generic.JsonCodec

import jjm.implicits._

import debate.quality.QuALITYStory
import debate.util.DenseDistribution
import debate.util.SparseDistribution
import debate.quality.QuALITYQuestion

object RoundRobinScheduler {

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
        bestDistractors = annotations.bestDistractors(correctAnswerIndex = annotations.goldLabel)
      } yield {
        if (bestDistractors.size > 1) {
          // import io.circe.syntax._
          // System.err.println(s"Multiple best distractors: ${question.asJson.spaces2}")
        }
        QASpec(
          questionId = question.questionUniqueId,
          question = question.question,
          correctAnswer = question.options(annotations.goldLabel - 1),
          incorrectAnswer = question.options(bestDistractors.head - 1)
        ) -> annotations.context.map(_.toDouble).meanOpt.getOrElse(0.0)
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

  case class QASpec(
    questionId: String,
    question: String,
    correctAnswer: String,
    incorrectAnswer: String
  )

  // this fully specifies a set of debates for a question
  // in the round-robin scheduling setting
  case class RoundRobinStorySchedule(
    story: QuALITYStory,
    question: QASpec,
    debater1: Profile.Human,
    debater2: Profile.Human,
    ai: Profile.AI,
    judges: Vector[Profile.Human] // 10 of these
  ) {
    // val sourceMaterial = SourceMaterial
    def getDebateSetups = {
      def humanHuman(
        honest: Profile.Human,
        dishonest: Profile.Human,
        judge: Profile.Human,
        honestFirst: Boolean
      ) = DebateSetup(
        rules = RoundRobinStorySchedule.twoDebaterRules,
        sourceMaterial = sourceMaterial,
        question = qa.question,
        answers = {
          val as = Vector(qa.correctAnswer, qa.incorrectAnswer)
          if (correctAnswerIndex == 0)
            as
          else
            as.reverse
        },
        correctAnswerIndex = correctAnswerIndex,
        roles =
          List[Option[(LiveDebateRole, String)]](
            honestDebaterOpt.map(Debater(correctAnswerIndex) -> _),
            dishonestDebaterOpt.map(Debater(1 - correctAnswerIndex) -> _)
          ).flatten.toMap ++ judgeOpt.map(Judge -> _),
        offlineJudges = offlineJudges.map(_ -> None).toMap,
        creationTime = creationTime
      )
    }
  }
  object RoundRobinStorySchedule {
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

  def scheduleRoundRobin(
    debaterPairs: Set[(Profile.Human, Profile.Human)],
    judges: Set[Profile.Human],
    aiDebater: Profile.AI,
    stories: Vector[QuALITYStory]
  ): Either[String, Vector[DebateSetup]] = {}
  def efficientlySampleSchedules(
    canJudge: Set[String],
    canDebate: Set[String],
    desiredWorkload: SparseDistribution[String],
    rules: SparseDistribution[RuleConfig],
    complete: Vector[DebateSetup],
    incomplete: Vector[DebateSetup],
    sourceMaterial: SourceMaterial,
    qas: Vector[QASpec],
    numDebatesPerQuestion: Int,
    dontAssignNewReading: Boolean,
    numUniqueDebatersConstraint: Option[Int],
    // debaters: Map[String, DebaterLoadConstraint],
    creationTime: Long,
    rand: Random
  ): Either[String, NonEmptyVector[Schedule]] = {
    // TODO validate setups

    val numDebaters: Int = numUniqueDebatersConstraint.getOrElse {
      val minNumDebaters = math.max(2, numDebatesPerQuestion)
      // val maxNumDebaters = math.ceil(math.sqrt(2 * numDebatesPerQuestion * qas.size) + 0.5)
      val preferredNumDebaters = math.floor(math.sqrt(2 * numDebatesPerQuestion * qas.size)).toInt
      math.max(minNumDebaters, preferredNumDebaters)
    }

    // val numDebates       = qas.size * numDebatesPerQuestion

    val startingSchedule = Schedule(
      desiredWorkload = desiredWorkload,
      desiredRules = rules,
      complete = complete,
      incomplete = incomplete,
      novel = Vector()
    )
    val workload = startingSchedule.workload

    val potentialDebaters: Set[String] =
      if (dontAssignNewReading) {
        val sourceMaterialId = SourceMaterialId.fromSourceMaterial(sourceMaterial)
        val existingReaders = (complete ++ incomplete)
          .filter(s => SourceMaterialId.fromSourceMaterial(s.sourceMaterial) == sourceMaterialId)
          .foldMap(
            _.roles
              .collect { case (Debater(_), name) =>
                name
              }
              .toSet
          )
        existingReaders.intersect(canDebate)
      } else
        canDebate

    val debaterChoiceDist = DenseDistribution
      .fromSoftmax[String](
        NonEmptyVector.fromVector(potentialDebaters.toVector).get,
        d => Params.workloadMultiplier * (desiredWorkload.prob(d) - workload.prob(d))
      )
      .withTemperature(Params.samplingTemperature)

    // sample debaters according to how far off they are from their desired workload
    val attemptedSampledSchedules = (1 to 15) // number of different debater sets we try
      .toVector
      .map { _ =>
        val chosenDebaters = debaterChoiceDist.sampleWithoutReplacement(numDebaters, rand).toSet

        qas
          .traverse(qa =>
            (1 to numDebatesPerQuestion)
              .toVector
              .traverse(_ =>
                for {
                  schedule <- StateT.get[Either[String, *], Schedule]
                  setup <- StateT.liftF(
                    sampleSetupForQuestion(
                      schedule,
                      sourceMaterial,
                      qa,
                      chosenDebaters,
                      canJudge -- chosenDebaters,
                      creationTime,
                      rand
                    )
                  )
                  _ <- StateT.modify[Either[String, *], Schedule](Schedule.novel.modify(_ :+ setup))
                } yield setup
              )
          )
          .runS(startingSchedule)
          // .flatMap(schedule =>
          //   if (Constraints.doesScheduleObeyLoadConstraints(schedule, people)) {
          //     Right(schedule)
          //   } else {
          //     Left("Schedule does not obey load constraints")
          //   }
          // )
          .flatMap(schedule =>
            if (Constraints.doesScheduleMeetJudgingConstraints(schedule)) {
              Right(schedule)
            } else {
              Left("Schedule does not meet judging constraints")
            }
          )
      }

    val errorsStr = attemptedSampledSchedules
      .flatMap(_.left.toOption)
      .counts
      .toVector
      .sortBy(-_._2)
      .toNev
      .map(
        _.map { case (err, count) =>
            s"($count) $err"
          }
          .toVector
          .mkString("; ")
      )
      .foldMap(x => s"Errors: $x")

    attemptedSampledSchedules
      .flatMap(_.toOption)
      .toNev
      .toRight(s"No valid schedules produced. $errorsStr")
  }

}
