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
  def ofDebateState(
      debateStates: Iterable[DebateState]
  ): LeaderboardForRoleType = {
    val x = debateStates.map({ (debateState: DebateState) =>
      debateState.debate match {
        case None => None
        case Some(debate: debate.Debate) =>
          debate.rounds.lastOption match {
            case Some(JudgeFeedback(distribution, _, true)) =>
              println("inside happy case")
              Some(debateState.participants, distribution)
            case _ => None
          }

      }
    })
    println("x", x)
    val y: Map[String, List[Double]] = x.foldLeft(Map[String, List[Double]]())({
      case (map, elt) =>
        elt match {
          case None => map
          case Some((participants, distribution)) =>
            println("inside happy case 2", participants)
            participants.foldLeft(map)({ case (map, participant) =>
              participant.role match {
                case Judge => map
                // TODO segment this by role
                // TODO judge accuracy is calculated differently
                case Debater(idx) =>
                  println("inside happy case 3")
                  val prob = distribution(idx)
                  map.get(participant.name) match {
                    case None => map + (participant.name -> List(prob))
                    case Some(probs) =>
                      map + (participant.name -> (prob :: probs))
                  }
                case _ => map
              }
            })
        }
    })
    LeaderboardForRoleType(y)
  }
}

// TODO encode practice / official debates

@JsonCodec
case class Leaderboard(
    judge: LeaderboardForRoleType,
    honest: LeaderboardForRoleType,
    dishonest: LeaderboardForRoleType
)
