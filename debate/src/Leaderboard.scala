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
  case class Intermediate(
      p: Probabilities,
      d: Assigned, // TODO maybe generalize to pack in the setup
      correctAnswerIndex: Int
  )

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
              list :+ Intermediate(
                distribution,
                debate.setup.roles,
                debate.setup.correctAnswerIndex
              )
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

  def foldInAssignedDebaterRole(
      assignedRoles: Map[DebateRole, String],
      idx: Int,
      prob: Double,
      map: OuterMap,
      correctAnswerIndex: Int
  ) = {
    assignedRoles.get(Debater(idx)) match {
      case None => map // no assigned role
      case Some(profileName) =>
        val isHonestDebater = correctAnswerIndex == idx
        val leaderboardCategory =
          if (isHonestDebater) LeaderboardCategories.HonestDebater
          else
            LeaderboardCategories.DishonestDebater
        map.get(leaderboardCategory) match {
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

  def foldInJudge(
      profileName: String,
      map: OuterMap,
      correctAnswerIndex: Int,
      distribution: Seq[Double]
  ) = {
    val judgeProb = distribution(correctAnswerIndex)
    val judge = LeaderboardCategories.Judge
    map.get(judge) match {
      case None =>
        map + (judge -> Map(
          profileName -> List(
            judgeProb
          )
        ))
      case Some(innerMap) =>
        innerMap.get(profileName) match {
          case None =>
            map + (judge -> (innerMap + (profileName -> List(
              judgeProb
            ))))
          case Some(probs) =>
            map + (judge -> (innerMap + (profileName -> (judgeProb :: probs))))
        }
    }
  }

  def profileNameToRoleToProbabilities(
      x: List[Intermediate]
  ): OuterMap = {
    val base: OuterMap = Map()
    x.foldLeft(base)({
      case (
            map,
            Intermediate(distribution, assignedRoles, correctAnswerIndex)
          ) =>
        val mapAfterDebaters =
          distribution.zipWithIndex.foldLeft(map)({ case (map, (prob, idx)) =>
            foldInAssignedDebaterRole(
              assignedRoles,
              idx,
              prob,
              map,
              correctAnswerIndex = correctAnswerIndex
            )
          })
        assignedRoles.get(Judge) match {
          case None => mapAfterDebaters // no assigned judge
          case Some(profileName) =>
            foldInJudge(
              profileName = profileName,
              map = mapAfterDebaters,
              correctAnswerIndex = correctAnswerIndex,
              distribution = distribution.toSeq // TODO ugh! yuck!
            )
        }
    })
  }

  def ofDebateStates(
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
  def ofDebateStates(debateStates: Iterable[DebateState]) = {
    val all = LeaderboardForRoleType.ofDebateStates(debateStates)
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
