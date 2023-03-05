package debate
package scheduler

import debate.util.SparseDistribution
import jjm.implicits._
import cats.implicits._
import debate.util.DenseDistribution
import cats.data.NonEmptyVector

object DebateScheduler {

  case class QASpec(question: String, answers: Vector[String], correctAnswerIndex: Int)

  def allAssignmentsForQuestion(
    rules: DebateRules,
    sourceMaterial: SourceMaterial,
    qa: QASpec,
    debaters: Set[String],
    numOfflineJudgesPerQuestion: Int,
    creationTime: Long
  ): Vector[DebateSetup] =
    for {
      honestDebater    <- debaters.toVector
      dishonestDebater <- debaters - honestDebater
      judge            <- debaters - honestDebater - dishonestDebater
      offlineJudges <- (debaters - honestDebater - dishonestDebater - judge)
        .toSeq
        .combinations(numOfflineJudgesPerQuestion)
        .map(_.toSet)
      honestFirst <- List(true, false)
      setup = DebateSetup(
        rules = rules,
        sourceMaterial = sourceMaterial,
        question = qa.question,
        answers = qa.answers,
        correctAnswerIndex = qa.correctAnswerIndex,
        roles = Map(
          Debater(qa.correctAnswerIndex)     -> honestDebater,
          Debater(1 - qa.correctAnswerIndex) -> dishonestDebater,
          Judge                              -> judge
        ),
        offlineJudges = offlineJudges.map(_ -> None).toMap,
        creationTime = creationTime
      )
      if Constraints.isSetupValid(setup)
    } yield setup

  case class RuleSpec(name: String, rules: DebateRules, numOfflineJudgesPerDebate: Int)

  def getDebateScheduleDistribution(
    debates: Vector[Debate],
    rules: DebateRules,
    sourceMaterial: SourceMaterial,
    qas: Vector[QASpec],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    // rules: SparseDistribution[RuleSpec],
    debaters: Map[String, DebaterLoadConstraint],
    creationTime: Long
  ): Option[DenseDistribution[Schedule]] = {
    // TODO validate setups
    val complete   = debates.filter(_.isOver).map(_.setup)
    val incomplete = debates.filterNot(_.isOver).map(_.setup)

    val workload = SparseDistribution(debaters.mapVals(_ => 1.0))
    // each vector in here is of length numQuestions
    /** combinations are fine because we already know we don't want to exactly repeat a debate.
      */
    val allSchedules = qas
      .traverse(qa =>
        allAssignmentsForQuestion(
          rules,
          sourceMaterial,
          qa,
          debaters.keySet,
          numOfflineJudgesPerDebate,
          creationTime
        ).toVector.combinations(numDebatesPerQuestion).toVector
      )
      .map(_.flatten)
      .map { newAssignments =>
        Schedule(
          desiredWorkload = workload,
          complete = complete,
          incomplete = incomplete,
          novel = newAssignments
        )
      }
      .filter(Constraints.doesScheduleObeyLoadConstraints(_, debaters))
      .filter(Constraints.doesScheduleMeetJudgingConstraints)

    NonEmptyVector
      .fromVector(allSchedules)
      .map(schedules => DenseDistribution.fromSoftmax[Schedule](schedules, _.cost))

    // val schedule = scheduleDist.sample(rng)
    // schedule.novel
  }
}
