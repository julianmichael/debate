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

object LeaderboardForRoleType {
  def ofDebateState( // TODO rename
      debateStates: Iterable[DebateState]
  ): LeaderboardForRoleType = {
    val x: Iterable[Tuple2[Map[DebateRole, String], Iterable[Double]]] =
      debateStates.foldLeft(
        List[Tuple2[Map[DebateRole, String], Iterable[Double]]]()
      )({ case (list, debateState) =>
        debateState.debate match {
          case None => list
          case Some(debate: debate.Debate) =>
            debate.rounds.lastOption match {
              case Some(JudgeFeedback(distribution, _, true)) =>
                println("inside happy case")
                list :+ (debate.setup.roles, distribution)
              case _ => list
            }

        }
      })

    println("x", x)
    val y: Map[String, List[Double]] =
      x.foldLeft(Map[String, List[Double]]())({
        case (map, (assignedRoles, distribution)) =>
          println("inside happy case 2", assignedRoles)
          // TODO judge accuracy is calculated differently
          val kvPairsToInsert =
            distribution.zipWithIndex.map({ case (prob, idx) =>
              assignedRoles.get(Debater(idx)) match {
                case None              => None // no assigned role
                case Some(profileName) => Some(profileName, prob)
              }
            })
          kvPairsToInsert.foldLeft(map)({ case (map, elt) =>
            elt match {
              case None => map
              case Some((profileName, prob)) =>
                map.get(profileName) match {
                  case None => map + (profileName -> List(prob))
                  case Some(probs) =>
                    map + (profileName -> (prob :: probs))
                }
            }
          })
      })
    LeaderboardForRoleType(y)
  }
}

@JsonCodec
case class Leaderboard(
    judge: LeaderboardForRoleType,
    honest: LeaderboardForRoleType,
    dishonest: LeaderboardForRoleType
)
