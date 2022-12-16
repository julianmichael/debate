package debate

import io.circe.generic.JsonCodec

// Enumerated over judge, honest, and dishonest debaters
@JsonCodec
case class LeaderboardForRoleType(
    perProfile: Map[String, List[
      Double
    ]] // final probability of winning, TODO maybe enforce 0-1?
) // string keys are debater names, TODO should they be ints?
{
  // TODO handle deprecation?
  def nDebatesPerProfile() = perProfile.mapValues(_.size)

  def accuracy() = {
    val nDebates = nDebatesPerProfile()
    val nCorrect = perProfile.mapValues(_.count(_ > 0.5))
    nCorrect.map({ case (debater, n) =>
      (debater, n.toDouble / nDebates(debater))
    })
  }

  def averageReward() = {
    val logProbs =
      perProfile.mapValues(_.map(math.log))
    val nDebates = nDebatesPerProfile()
    logProbs.map({ case (debater, (logProbs: List[Double])) =>
      val sum = logProbs.fold(0.0)(_ + _)
      (debater, sum / nDebates(debater))
    })
  }

}

object LeaderboardCategories {
  sealed trait LeaderboardCategory
  case object Judge extends LeaderboardCategory
  case object HonestDebater extends LeaderboardCategory
  case object DishonestDebater extends LeaderboardCategory
}

object LeaderboardForRoleType {
  type States = Iterable[DebateState]
  type Probabilities = Iterable[Double]
  type Assigned = Map[DebateRole, String]
  type Scores = Map[String, List[Double]]
  // TODO rename
  case class Intermediate(p: Probabilities, a: Assigned)

  def finishedDebatesWithAssignedParticipants(
      states: States
  ) = {
    states.foldLeft(
      List[Intermediate]()
    )({ case (list, state) =>
      state.debate match {
        case None => list
        case Some(debate: debate.Debate) =>
          debate.rounds.lastOption match {
            case Some(JudgeFeedback(distribution, _, true))
                if !debate.setup.roles.isEmpty =>
              println("inside happy case")
              list :+ Intermediate(distribution, debate.setup.roles)
            case _ => list
          }

      }
    })

  }

  type InnerMap = Map[String, List[Double]]
  type OuterMap =
    Map[
      LeaderboardCategories.LeaderboardCategory,
      InnerMap
    ] // TODO proper case class?

  def foldInAssignedRole(
      assignedRoles: Map[DebateRole, String],
      idx: Int,
      prob: Double,
      map: OuterMap
  ) = {
    assignedRoles.get(Debater(idx)) match {
      case None => map // no assigned role
      case Some(profileName) =>
        val leaderboardCategory =
          LeaderboardCategories.DishonestDebater // TODO implement
        map.get(leaderboardCategory) match {
          // TODO i think this calculates judge scores incorrectly but will do it
          case None =>
            map + (leaderboardCategory -> Map(profileName -> List(prob)))
          case Some(innerMap) =>
            innerMap.get(profileName) match {
              case None =>
                map + (leaderboardCategory -> (innerMap + (profileName -> List(
                  prob
                ))))
              case Some(probs) =>
                map + (leaderboardCategory -> (innerMap + (profileName -> (prob :: probs))))
            }
        }
    }
  }

  def profileNameToRoleToProbabilities(
      x: List[Intermediate]
  ): OuterMap = {
    val base: OuterMap = Map()
    x.foldLeft(base)({ case (map, Intermediate(distribution, assignedRoles)) =>
      distribution.zipWithIndex.foldLeft(map)({ case (map, (prob, idx)) =>
        foldInAssignedRole(assignedRoles, idx, prob, map)
      })
    })
  }

  def ofDebateState( // TODO rename
      debateStates: Iterable[DebateState]
  ): OuterMap = {
    val x = finishedDebatesWithAssignedParticipants(debateStates)
    val y = profileNameToRoleToProbabilities(x)
    y
  }
}

@JsonCodec
case class Leaderboard(
    judge: LeaderboardForRoleType, // profile name -> list of final probabilities (for correct side)
    honest: LeaderboardForRoleType,
    dishonest: LeaderboardForRoleType
)

object Leaderboard {
  def ofDebateState(debateStates: Iterable[DebateState]) = {
    val all = LeaderboardForRoleType.ofDebateState(debateStates)
    Leaderboard(
      judge = LeaderboardForRoleType(
        all.getOrElse(LeaderboardCategories.Judge, Map())
      ),
      honest = LeaderboardForRoleType(
        all.getOrElse(LeaderboardCategories.HonestDebater, Map())
      ),
      dishonest = LeaderboardForRoleType(
        all.getOrElse(LeaderboardCategories.DishonestDebater, Map())
      )
    )
  }
}
