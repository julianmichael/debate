package debate
package scheduler

case class Assignment(
  val storyId: SourceMaterialId,
  val question: String,
  val honestDebater: String,
  val dishonestDebater: String,
  val judge: String,
  val offlineJudges: Set[String],
  val honestFirst: Boolean
) {
  def debaters        = Set(dishonestDebater, honestDebater)
  def judges          = offlineJudges + judge
  def allParticipants = debaters ++ judges

  def isAssigned(debater: String): Boolean = allParticipants.contains(debater)

  def toPrettyString: String =
    s"""Story ID: $storyId
       |Question: $question
       |Honest debater: $honestDebater
       |Dishonest debater: $dishonestDebater
       |Judge: $judge
       |Offline judges: ${offlineJudges.mkString(", ")}""".stripMargin
}

object Assignment {

  /**
      * Returns the assignment of roles to users for a given debate.
      * Returns None if the debate has not been assigned all of the roles,
      * or if the debate has a number of answers that differs from 2.
      */
  def fromDebateSetup(setup: DebateSetup): Option[Assignment] =
    for {
      judge         <- setup.roles.get(Judge)
      honestDebater <- setup.roles.get(Debater(setup.correctAnswerIndex))
      dishonestDebaters =
        setup
          .roles
          .collect {
            case (Debater(index), name) if index != setup.correctAnswerIndex =>
              name
          }
          .toSet
      dishonestDebater <- dishonestDebaters.headOption.filter(_ => setup.answers.size == 2)
      offlineJudges = setup.offlineJudges.keySet
      honestFirst   = setup.correctAnswerIndex == 0
    } yield Assignment(
      SourceMaterialId.fromSourceMaterial(setup.sourceMaterial),
      setup.question,
      honestDebater,
      dishonestDebater,
      judge,
      offlineJudges,
      honestFirst
    )
}
