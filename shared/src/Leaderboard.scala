package debate

import io.circe.generic.JsonCodec

import jjm.metrics._

import cats._
import cats.implicits._

@JsonCodec
case class DebateStats(wins: Proportion.Stats, rewards: Numbers[Double])

object DebateStats {

  implicit val debateStatsMonoid: Monoid[DebateStats] =
    cats.derived.semiauto.monoid[DebateStats]

  def getUserStats(
    debateResult: DebateResult,
    role: DebateRole,
    name: String
  ): Chosen[LeaderboardCategory, Chosen[
    String,
    DebateStats
  ]] = {
    def bool2int(b: Boolean) = if(b) 1 else 0
    val leaderboardCategory = role match {
        case Debater(index) =>
          if(index == debateResult.correctAnswerIndex) {
            LeaderboardCategory.HonestDebater
          } else LeaderboardCategory.DishonestDebater
        case Judge => LeaderboardCategory.Judge
    }
    val userStats = role match {
      case Debater(index) =>
        val correct = debateResult.finalJudgement(index) > 0.5
        val reward = math.log(debateResult.finalJudgement(index))
        DebateStats(
          Proportion.Stats(bool2int(correct), bool2int(!correct)),
          Numbers(reward)
        )
      case Judge =>
        val correct = debateResult.finalJudgement(debateResult.correctAnswerIndex) > 0.5
        DebateStats(
          Proportion.Stats(bool2int(correct), bool2int(!correct)),
          Numbers(debateResult.judgeReward)
        )
    }
    Chosen(Map(leaderboardCategory -> Chosen(Map(name -> userStats))))
  }

  def fromDebate(
    d: Debate
  ): Chosen[LeaderboardCategory, Chosen[
    String,
    DebateStats
  ]] = {
    d.result match {
      case None => Chosen(Map.empty)
      case Some(result) =>
        d.setup.roles.toList.foldMap { case (role, user) =>
          getUserStats(result, role, user)
        }
    }
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
case class Leaderboard(
    data: Map[
      LeaderboardCategory,
      Map[String, DebateStats]
    ]
)

object Leaderboard {
  def fromDebates[F[_]: Foldable](debates: F[Debate]) = {
    Leaderboard(
      debates.foldMap(DebateStats.fromDebate)
        .data.view.mapValues(_.data).toMap
    )
  }
}
