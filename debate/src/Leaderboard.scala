package debate

import io.circe.generic.JsonCodec

import jjm.metrics._
import cats._
import cats.implicits._

case class DebateStats(wins: Accuracy.Stats, rewards: Numbers[Double])

object DebateStats {

  implicit val debateStatsMonoid: Monoid[DebateStats] =
    cats.derived.semiauto.monoid[DebateStats]

  def combineAssignedUsers(
      debateResult: DebateResult
  )(
      x: (DebateRole, String)
  ): Chosen[LeaderboardCategory, Chosen[
    String,
    DebateStats
  ]] = {
    val (debaterKey, inner) =
      x match {
        case (debate.Debater(index), name) =>
          val correct = if (debateResult.finalJudgement(index) > 0.5) { 1 }
          else { 0 }
          val incorrect = 1 - correct
          val reward = math.log(debateResult.finalJudgement(index))
          val debaterKey = if (index == debateResult.correctAnswerIndex) {
            LeaderboardCategory.HonestDebater
          } else {
            LeaderboardCategory.DishonestDebater
          }
          (
            debaterKey,
            Map(
              name -> DebateStats(
                Accuracy.Stats(correct, incorrect),
                Numbers(reward)
              )
            )
          )
        case (Judge, name) =>
          val correct =
            if (
              debateResult.finalJudgement(debateResult.correctAnswerIndex) > 0.5
            ) { 1 }
            else { 0 }
          val incorrect = 1 - correct
          (
            LeaderboardCategory.Judge,
            Map(
              name -> DebateStats(
                Accuracy.Stats(correct, incorrect),
                Numbers(debateResult.judgeReward)
              )
            )
          )
      }

    Chosen(Map(debaterKey -> Chosen(inner)))
  }

  def foldOverDebate(
      d: Debate
  ): Chosen[LeaderboardCategory, Chosen[
    String,
    DebateStats
  ]] = {
    d.result match {
      case None => Chosen(Map.empty)
      case Some(result) =>
        d.setup.roles.toList.foldMap(
          combineAssignedUsers(
            result
          )
        )
    }
  }

  def foldOverDebates(finishedDebates: List[Debate]): Chosen[
    LeaderboardCategory,
    Chosen[String, DebateStats]
  ] = {
    finishedDebates.foldMap(foldOverDebate)
  }
}

@JsonCodec
sealed trait LeaderboardCategory {
  import LeaderboardCategory._
  override def toString = this match {
    case Judge => "Judge"
    case HonestDebater => "Honest Debater"
    case DishonestDebater => "Dishonest Debater"
  }
}
object LeaderboardCategory {
  case object Judge extends LeaderboardCategory
  case object HonestDebater extends LeaderboardCategory
  case object DishonestDebater extends LeaderboardCategory

  def all = List(Judge, HonestDebater, DishonestDebater)

  def fromString(x: String): Option[LeaderboardCategory] =
    all.find(_.toString == x)

  import io.circe._

  implicit val keyEncoder: KeyEncoder[LeaderboardCategory] =
    KeyEncoder.instance(_.toString)
  implicit val keyDecoder: KeyDecoder[LeaderboardCategory] =
    KeyDecoder.instance(fromString)
}

@JsonCodec
case class SerializableDebateStats(
    wins: Int,
    losses: Int,
    averageReward: Double
)

object SerializableDebateStats {
  def ofDebateStats(d: DebateStats): SerializableDebateStats =
    SerializableDebateStats(
      wins = d.wins.correct,
      losses = d.wins.incorrect,
      averageReward = d.rewards.stats.mean
    )

}

@JsonCodec
case class Leaderboard(
    data: Map[
      LeaderboardCategory,
      Map[String, SerializableDebateStats]
    ]
)

object Leaderboard {

  def ofDebateStates(d: List[Debate]) = {
    Leaderboard(
      DebateStats
        .foldOverDebates(d)
        .data
        .view.mapValues(
          _.data.view.mapValues(SerializableDebateStats.ofDebateStats).toMap
        )
        .toMap
    )
  }

}
