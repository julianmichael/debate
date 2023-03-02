package debate
package scheduler

case class Assignment private (
  val honestDebater: String,
  val dishonestDebaters: Set[String],
  val judge: String,
  val offlineJudges: Set[String]
) {
  def allParticipants = dishonestDebaters ++ offlineJudges + honestDebater + judge

  def isAssigned(debater: String): Boolean = allParticipants.contains(debater)

  def toPrettyString: String =
    s"""Honest debater: $honestDebater
       |Dishonest debaters: ${dishonestDebaters.mkString(", ")}
       |Judge: $judge
       |Offline judges: ${offlineJudges.mkString(", ")}""".stripMargin
}

object Assignment {
  def create(
    honestDebater: String,
    dishonestDebaters: Set[String],
    judge: String,
    offlineJudges: Set[String]
  ): Either[Exception, Assignment] = {
    val assignment = new Assignment(honestDebater, dishonestDebaters, judge, offlineJudges)
    if (assignment.allParticipants.size != dishonestDebaters.size + offlineJudges.size + 2) {
      return Left(new IllegalArgumentException("Debaters and judges must all be different people"))
    } else
      Right(assignment)
  }

  /**
      * Returns the assignment of roles to users for a given debate.
      * Returns None if the debate has not been assigned roles.
      */
  def fromDebate(debate: Debate): Option[Assignment] =
    for {
      judge         <- debate.setup.roles.get(Judge)
      honestDebater <- debate.setup.roles.get(Debater(debate.setup.correctAnswerIndex))
      dishonestDebaters =
        debate
          .setup
          .roles
          .collect {
            case (Debater(index), name) if index != debate.setup.correctAnswerIndex =>
              name
          }
          .toSet
      offlineJudges = debate.setup.offlineJudges.keySet
    } yield create(honestDebater, dishonestDebaters, judge, offlineJudges) match {
      case Right(assignment) =>
        assignment
      case Left(error) =>
        // this should never happen if the error comes from dishonest debater assignment, etc. , but technically it's possible in case there are new error cases
        throw error
    }
}
