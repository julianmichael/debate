package debate
package scheduler

import debate.util.SparseDistribution
import jjm.implicits._
import cats.implicits._
import debate.util.DenseDistribution
import cats.data.NonEmptyVector

object DebateScheduler {

  def allAssignmentsForQuestion(
    storyId: SourceMaterialId,
    question: String,
    debaters: Set[String],
    numOfflineJudgesPerQuestion: Int
  ): Vector[Assignment] =
    for {
      honestDebater    <- debaters.toVector
      dishonestDebater <- debaters - honestDebater
      judge            <- debaters - honestDebater - dishonestDebater
      offlineJudges <- (debaters - honestDebater - dishonestDebater - judge)
        .toSeq
        .combinations(numOfflineJudgesPerQuestion)
        .map(_.toSet)
      honestFirst <- List(true, false)
      assignment = Assignment(
        storyId = storyId,
        question = question,
        honestDebater = honestDebater,
        judge = judge,
        dishonestDebater = dishonestDebater,
        offlineJudges = offlineJudges,
        honestFirst = honestFirst
      )
      if Constraints.isAssignmentValid(assignment)
    } yield assignment

  def allAssignmentsForStory(
    storyId: SourceMaterialId,
    questions: Vector[String],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    debaters: Set[String]
  ): Vector[Vector[Assignment]] =
    /** combinations are fine because we already know we don't want to exactly repeat a debate.
      */
    questions
      .traverse(question =>
        allAssignmentsForQuestion(storyId, question, debaters, numOfflineJudgesPerDebate)
          .toVector
          .combinations(numDebatesPerQuestion)
          .toVector
      )
      .map(_.flatten)

  // complete = debates.filter(_.isOver).flatMap(Assignment.fromDebate),
  // incomplete = debates.filterNot(_.isOver).flatMap(Assignment.fromDebate),
  def generateAllSchedules(
    complete: Vector[Assignment],
    incomplete: Vector[Assignment],
    storyId: SourceMaterialId,
    questions: Vector[String],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    debaters: Map[String, DebaterLoadConstraint] // TODO: change to or add soft constraints
  ): Vector[Schedule] = {
    val workload = SparseDistribution(debaters.mapVals(_ => 1.0))
    // each vector in here is of length numQuestions
    allAssignmentsForStory(
      storyId = storyId,
      questions = questions,
      numDebatesPerQuestion = numDebatesPerQuestion,
      numOfflineJudgesPerDebate = numOfflineJudgesPerDebate,
      debaters = debaters.keySet
    ).filter(Constraints.doesAssignmentObeyLoadConstraints(_, debaters))
      .map { newAssignments =>
        Schedule(
          desiredWorkload = workload,
          complete = complete,
          incomplete = incomplete,
          novel = newAssignments
        )
      }
      .filter(Constraints.doesScheduleMeetJudgingConstraints)
  }

  def getScheduleDistribution(
    complete: Vector[Assignment],
    incomplete: Vector[Assignment],
    storyId: SourceMaterialId,
    questions: Vector[String],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    debaters: Map[String, DebaterLoadConstraint] // TODO: change to or add soft constraints
  ) = DenseDistribution.fromSoftmax[Schedule](
    NonEmptyVector
      .fromVector(
        generateAllSchedules(
          complete,
          incomplete,
          storyId,
          questions,
          numDebatesPerQuestion,
          numOfflineJudgesPerDebate,
          debaters
        )
      )
      .get,
    _.cost
  )
}
