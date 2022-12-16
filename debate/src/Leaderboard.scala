package debate

import io.circe.generic.JsonCodec

/** I think this could be made simpler/easier by computing a single stats object
  * holding all of the relevant info for each debate, which can then be combined
  * using a monoid to get all the leaderboard info in a single object. Using the
  * Chosen, Accuracy, and Numbers classes in jjm.metrics, the full object would
  * look something like Chosen[LeaderboardCategory, Chosen[String, DebateStats]]
  * where the String param there is the profile name, and the value is case
  * class DebateStats(wins: Accuracy.Stats, rewards: Numbers[Double]) with a
  * monoid instance that just uses the monoids of its members (you can
  * auto-generate this instance using eg cats.derived.semiauto.monoid).
  *
  * All you would need to do to compute the full set of leaderboards is define a
  * function Debate => Chosen[LeaderboardCategory, Chosen[String, DebateStats]]
  * which computes the stats for all the assigned roles in a debate, and then
  * foldMap this function over all of the debates. I haven't fully followed how
  * you did it in the code here but I think my suggestion might be a simpler way
  * of doing it.
  *
  * (sorry, I mixed up the types the first time i wrote this comment, should be
  * fixed now)
  */

object Experimental {
  import jjm.metrics._
  import cats._
  import cats.implicits._

  case class DebateStats(wins: Accuracy.Stats, rewards: Numbers[Double])

  implicit val debateStatsMonoid: Monoid[DebateStats] =
    cats.derived.semiauto.monoid[DebateStats]

  def h(
      x: (DebateRole, String)
  ): Chosen[LeaderboardCategories.LeaderboardCategory, Chosen[
    String,
    DebateStats
  ]] = {
    // TODO make this accurate
    x match {
      case (debate.Debater(_), name) =>
        Chosen(
          Map(
            LeaderboardCategories.HonestDebater ->
              Chosen(
                Map(name -> DebateStats(Accuracy.Stats(1, 0), Numbers(0)))
              )
          )
        )
      /*       case (DebateRole.DishonestDebater, name) =>
        Chosen(
          LeaderboardCategories.DishonestDebater,
          Chosen(name, DebateStats(Accuracy.Stats(0, 1), Numbers(0.0, 0)))
        ) */
      case (Judge, name) =>
        Chosen(
          Map(
            LeaderboardCategories.Judge ->
              Chosen(Map(name -> DebateStats(Accuracy.Stats(0, 0), Numbers(0))))
          )
        )
    }

  }

  def f(d: Debate): Chosen[LeaderboardCategories.LeaderboardCategory, Chosen[
    String,
    DebateStats
  ]] = {
    d.setup.roles.toList.foldMap(h)
  }

  def g(finishedDebates: List[Debate]): Chosen[
    LeaderboardCategories.LeaderboardCategory,
    Chosen[String, DebateStats]
  ] = {
    finishedDebates.foldMap(f)
  }
}

// a sketch of a new way to implement this

/* trait LeaderboardType {
  def customColumnName
      : String // e.g. "Average Log Probability" or "Average Score"
  def customColumnValue: Double

  /** guarantee: nDebates = wins + losses */
  def wins: Int

  /** guarantee: nDebates = wins + losses */
  def losses: Int
  def leaderboardType: LeaderboardCategories.LeaderboardCategory
}

abstract class Debater(probabilities: Vector[Double]) extends LeaderboardType {
  def customColumnName = "Average Log Prob"
  def customColumnValue =
    (probabilities.map(math.log).sum / probabilities.length)
  def wins = (probabilities.filter(_ > 0.5)).length
  def losses = probabilities.length - wins
}

class HonestDebater(probabilities: Vector[Double])
    extends Debater(probabilities) {
  def leaderboardType = LeaderboardCategories.HonestDebater
}

class DishonestDebater(probabilities: Vector[Double])
    extends Debater(probabilities) {
  def leaderboardType = LeaderboardCategories.DishonestDebater
}

/** precondition that the lists are the same length- TODO maybe enforce this in
 * code
 */
class Judge(
    numTurns: Vector[Int],
    finalDistribution: Vector[Vector[Double]],
    function: ScoringFunction,
    correctAnswerIndices: Vector[Int]
) extends LeaderboardType {
  def customColumnName = "Average Score"

  def wins = finalDistribution
    .zip(correctAnswerIndices)
    .map({ case (l, c) => l(c) })
    .filter(_ > 0.5)
    .length

  def losses = correctAnswerIndices.length - wins

  def customColumnValue: Double = {
    val scores: Vector[Double] = finalDistribution
      .zip(correctAnswerIndices)
      .zip(numTurns)
      .map({ case ((l, c), numTurns) =>
        function
          .eval(turnNumber = numTurns, distribution = l, outcome = c)
      })
      .toVector
    scores.sum / scores.length
  }
}

// Enumerated over judge, honest, and dishonest debaters
@JsonCodec
case class LeaderboardForRoleType(
    perProfile: Map[String, List[
      Double
    ]] // final probability of winning, TODO someday maybe enforce 0-1?
) // string keys are debater names
{
  def nDebatesPerProfile() = perProfile.mapValues(_.size)

  def nWins() = {
    val nDebates = nDebatesPerProfile()
    val nWins = perProfile.mapValues(_.count(_ > 0.5))
    nWins.map({ case (debater, n) =>
      (debater, n)
    })
  }

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

} */

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
  case class Intermediate(
      p: Probabilities,
      d: Assigned,
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
    ]

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
      distribution: Vector[Double]
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
              distribution = distribution.toVector
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

  def empty = {
    Leaderboard(
      judge = LeaderboardForRoleType(Map()),
      honest = LeaderboardForRoleType(Map()),
      dishonest = LeaderboardForRoleType(Map())
    )
  }
}
