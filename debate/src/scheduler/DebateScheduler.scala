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

object DebateScheduler {

  object Params {
    val samplingTemperature = 1 / 10.0
    val workloadMultiplier  = 5
  }

  def getQAsForStory(story: QuALITYStory) =
    story
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
          DebateScheduler.QASpec(
            questionId = question.questionUniqueId,
            question = question.question,
            correctAnswer = question.options(annotations.goldLabel - 1),
            incorrectAnswer = question.options(bestDistractors.head - 1)
          )
        }
      }
      .toVector

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

  val ruleDistMultiplier = 3.0

  def sampleRules(schedule: Schedule, rand: Random) = {
    // silly?
    val allRuleConfigs = NonEmptyVector
      .fromVector(schedule.desiredRules.probs.toSortedMap.filter(_._2 > 0.0).toVector)
      .get
      .map(_._1)
    DenseDistribution
      .fromSoftmax[RuleConfig](
        allRuleConfigs,
        config =>
          schedule
            .rulesDistOpt
            .foldMap(rulesDist =>
              ruleDistMultiplier * (schedule.desiredRules.prob(config) - rulesDist.prob(config))
            )
      )
      .sample(rand)
  }

  def sampleDebater(
    profiles: Map[String, Profile],
    schedule: Schedule,
    storyId: SourceMaterialId,
    debaters: Set[String],
    opponentOpt: Option[String],
    question: String,
    isHonest: Boolean,
    rand: Random
  ): Option[String] = NonEmptyVector
    .fromVector(
      (
        debaters --
          (
            schedule
              .forStory(storyId)
              .flatMap { setup =>
                setup.offlineJudges.keySet ++ setup.roles.get(Judge) ++
                  setup
                    .roles
                    .get(
                      Debater(
                        if (isHonest)
                          setup.correctAnswerIndex
                        else
                          1 - setup.correctAnswerIndex
                      )
                    )
                    .filter(_ => setup.question == question)
              }
              .toSet ++ opponentOpt
          ).filter(name => profiles.get(name).exists(_.isHuman))
      ).toVector
    )
    .map(candidates =>
      DenseDistribution
        .fromSoftmax[String](
          candidates,
          d =>
            (Params.workloadMultiplier *
              (schedule.desiredWorkload.prob(d) - schedule.workload.prob(d))) +
              +schedule
                .roleDistributionPerDebater
                .get(d)
                .map(debaterRoleDist =>
                  schedule.roleDistributionGlobal.prob("debater") - debaterRoleDist.prob("debater")
                )
                .getOrElse(0.0) +
              (
                0.5 - {
                  val honestyRate = schedule
                    .honestyLoad
                    .get(d)
                    .flatMap(SparseDistribution.fromMap[String])
                    .map(_.prob("honest"))
                    .getOrElse(0.5)

                  if (isHonest)
                    honestyRate
                  else
                    1.0 - honestyRate
                }
              ) +
              opponentOpt.foldMap(opponent =>
                (1.0 / schedule.allPeople.size) -
                  schedule
                    .opponentLoad
                    .get(d)
                    .flatMap(SparseDistribution.fromMap[String])
                    .map(_.prob(opponent))
                    .getOrElse(0.0)
              )
        )
        .withTemperature(Params.samplingTemperature)
        .sample(rand)
    )

  def sampleJudge(
    schedule: Schedule,
    debaters: Set[String],
    storyId: SourceMaterialId,
    judges: Set[String],
    question: String,
    isLive: Boolean,
    enforceJudgingConstraints: Boolean,
    rand: Random
  ): Option[String] = {
    val roleStringForLoadCalc =
      if (isLive)
        "judge (live)"
      else
        "judge (offline)"

    NonEmptyVector
      .fromVector(
        (
          judges -- debaters --
            schedule
              .forStory(storyId)
              .foldMap(setup =>
                setup
                  .roles
                  .map {
                    case (Debater(_), name) =>
                      name -> numJudgingsAllowedPerStory // debaters DQ'd
                    case (Judge, name) if setup.question == question =>
                      name -> numJudgingsAllowedPerStory // DQ if you've judged the question before
                    case (Judge, name) =>
                      name -> 1
                  } |+| setup.offlineJudges.mapVals(_ => 1)
              )
              .collect {
                case (name, i)
                    if i >= numJudgingsAllowedPerStory => // DQ'd if you've judged too many times
                  name
              }
              .toSet
              .filter(_ =>
                enforceJudgingConstraints
              ) // only remove these if we're using judging constraints
        ).toVector
      )
      .map { candidates =>
        DenseDistribution
          .fromSoftmax[String](
            candidates,
            d =>
              schedule.desiredWorkload.prob(d) - schedule.workload.prob(d) +
                +schedule
                  .roleDistributionPerDebater
                  .get(d)
                  .map(debaterRoleDist =>
                    schedule.roleDistributionGlobal.prob(roleStringForLoadCalc) -
                      debaterRoleDist.prob(roleStringForLoadCalc)
                  )
                  .getOrElse(0.0) +
                debaters.unorderedFoldMap(debater =>
                  (1.0 / schedule.allPeople.size) -
                    schedule
                      .judgeDebaterLoad
                      .get(d)
                      .flatMap(SparseDistribution.fromMap[String])
                      .map(_.prob(debater))
                      .getOrElse(0.0)
                ) +
                debaters.unorderedFoldMap(debater =>
                  (1.0 / schedule.allPeople.size) -
                    schedule
                      .debaterJudgeLoad
                      .get(debater)
                      .flatMap(SparseDistribution.fromMap[String])
                      .map(_.prob(d))
                      .getOrElse(0.0)
                )
          )
          .withTemperature(Params.samplingTemperature)
          .sample(rand)
      }
  }

  def sampleCorrectAnswerIndex(
    schedule: Schedule,
    honestDebaterOpt: Option[String],
    dishonestDebaterOpt: Option[String],
    rand: Random
  ): Int = DenseDistribution
    .fromSoftmax[Int](
      NonEmptyVector.of(0, 1),
      i =>
        0.5 -
          honestDebaterOpt
            .flatMap(honestDebater =>
              schedule
                .orderLoad
                .get(honestDebater)
                .flatMap(SparseDistribution.fromMap[Int])
                .map(_.prob(i))
            )
            .getOrElse(0.5) + 0.5 -
          dishonestDebaterOpt
            .flatMap(dishonestDebater =>
              schedule
                .orderLoad
                .get(dishonestDebater)
                .flatMap(SparseDistribution.fromMap[Int])
                .map(_.prob(1 - i))
            )
            .getOrElse(0.5)
    )
    .withTemperature(Params.samplingTemperature)
    .sample(rand)

  def sampleSetupForQuestion(
    profiles: Map[String, Profile],
    schedule: Schedule,
    sourceMaterial: SourceMaterial,
    qa: QASpec,
    debaters: Set[String],
    judges: Set[String],
    enforceJudgingConstraints: Boolean,
    creationTime: Long,
    rand: Random
  ): Either[String, DebateSetup] = {
    val storyId    = SourceMaterialId.fromSourceMaterial(sourceMaterial)
    val ruleConfig = sampleRules(schedule, rand)
    val shouldSampleHonest =
      ruleConfig.numAssignedDebaters match {
        case x if x > 1 =>
          true
        case 1 =>
          rand.nextDouble() < 0.5
        case 0 =>
          false
      }
    val shouldSampleDishonest =
      ruleConfig.numAssignedDebaters match {
        case x if x > 1 =>
          true
        case 1 =>
          !shouldSampleHonest
        case 0 =>
          false
      }
    for {
      honestDebaterOpt <-
        if (shouldSampleHonest) {
          sampleDebater(profiles, schedule, storyId, debaters, None, qa.question, true, rand)
            .toRight("Couldn't sample honest debater.")
            .map(_.some)
        } else
          Right(None)
      dishonestDebaterOpt <-
        if (shouldSampleDishonest) {
          sampleDebater(
            profiles,
            schedule,
            storyId,
            debaters,
            honestDebaterOpt,
            qa.question,
            false,
            rand
          ).toRight("Couldn't sample dishonest debater.").map(_.some)
        } else
          Right(None)
      judgeOpt <-
        if (ruleConfig.rules.hasJudge) {
          sampleJudge(
            schedule,
            Set(honestDebaterOpt, dishonestDebaterOpt).flatten,
            storyId,
            judges,
            qa.question,
            true,
            enforceJudgingConstraints,
            rand
          ).toRight("Couldn't sample judge.").map(_.some)
        } else
          Right(None)
      offlineJudges <- (1 to ruleConfig.numOfflineJudgesPerDebate)
        .toVector
        .traverse(_ =>
          for {
            curJudges <- StateT.get[Option, Set[String]]
            newJudge <- StateT.liftF(
              sampleJudge(
                schedule,
                debaters,
                storyId,
                judges -- curJudges,
                qa.question,
                false,
                enforceJudgingConstraints,
                rand
              )
            )
            _ <- StateT.modify[Option, Set[String]](_ + newJudge)
          } yield newJudge
        )
        .run(judgeOpt.toSet)
        .map(_._2.toSet)
        .toRight("Couldn't sample offline judges.")
      correctAnswerIndex = sampleCorrectAnswerIndex(
        schedule,
        honestDebaterOpt,
        dishonestDebaterOpt,
        rand
      )
    } yield DebateSetup(
      rules = ruleConfig.rules,
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

  def efficientlySampleSchedules(
    profiles: Map[String, Profile],
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
    enforceJudgingConstraints: Boolean,
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
                      profiles,
                      schedule,
                      sourceMaterial,
                      qa,
                      chosenDebaters,
                      canJudge -- chosenDebaters,
                      enforceJudgingConstraints,
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
            if (
              enforceJudgingConstraints --> Constraints.doesScheduleMeetJudgingConstraints(schedule)
            ) {
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

  @JsonCodec
  case class OfflineJudgeSchedulingResult(
    debatesWithoutNewAssignments: Set[String],
    newAssignments: Vector[(String, String)]
  )
  object OfflineJudgeSchedulingResult {
    implicit val offlineJudgeSchedulingResultMonoid = cats
      .derived
      .semiauto
      .monoid[OfflineJudgeSchedulingResult]
  }

  def sampleOfflineJudges(
    debates: Map[String, Debate],
    people: NonEmptySet[String],
    judges: Set[String],
    maxNumJudgesForOnline: Int,
    maxNumJudgesForOffline: Int,
    rand: Random
  ): OfflineJudgeSchedulingResult = {
    def sampleJudgeCumulative(
      assignments: OfflineJudgeSchedulingResult,
      remainingDebates: List[(String, Debate)],
      finishedDebates: List[Debate]
    ): (List[Debate], OfflineJudgeSchedulingResult) =
      remainingDebates match {
        case Nil =>
          finishedDebates -> assignments
        case (roomName, debate) :: rest =>
          val judgeOpt = debate.setup.roles.get(Judge)
          val maxNumJudges =
            if (judgeOpt.nonEmpty)
              maxNumJudgesForOnline
            else
              maxNumJudgesForOffline
          val offlineJudges =
            debate.setup.offlineJudges.keySet ++ debate.offlineJudgingResults.keySet
          val allJudges = offlineJudges ++ judgeOpt
          val newJudgesOpt = (0 until (maxNumJudges - allJudges.size.toInt))
            .toVector
            .traverse { _ =>
              for {
                curJudges <- StateT.get[Option, Set[String]]
                newJudge <- StateT.liftF(
                  sampleJudge(
                    Schedule(
                      SparseDistribution.uniform(people),
                      SparseDistribution.uniform(NonEmptySet.of(RuleConfig.default)),
                      complete =
                        finishedDebates.toVector.map(_.setup) ++ remainingDebates.map(_._2.setup),
                      incomplete = Vector(),
                      novel = Vector()
                    ),
                    Set(),
                    SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial),
                    judges -- curJudges,
                    debate.setup.question,
                    isLive = false,
                    enforceJudgingConstraints = true,
                    rand = rand
                  )
                )
                _ <- StateT.modify[Option, Set[String]](_ + newJudge)
              } yield newJudge
            }
            .run(allJudges)
            .map(_._2.toSet)

          (newJudgesOpt: Option[Set[String]]) match {
            case None =>
              val newResult = OfflineJudgeSchedulingResult(Set(roomName), Vector())
              sampleJudgeCumulative(newResult |+| assignments, rest, debate :: finishedDebates)
            case Some(newJudges) =>
              val newResult = OfflineJudgeSchedulingResult(
                Set(),
                newJudges.toVector.map(roomName -> _)
              )
              val newDebate =
                Debate
                  .setup
                  .composeLens(DebateSetup.offlineJudges)
                  .modify(offlineJudges =>
                    offlineJudges ++ newJudges.map(_ -> Some(OfflineJudgingMode.Stepped)).toMap
                  )(debate)
              sampleJudgeCumulative(newResult |+| assignments, rest, newDebate :: finishedDebates)
          }
      }

    sampleJudgeCumulative(Monoid[OfflineJudgeSchedulingResult].empty, debates.toList, Nil)._2
  }
}
