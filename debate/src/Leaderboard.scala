package debate

import cats._
import cats.implicits._

import io.circe.generic.JsonCodec

import jjm.metrics._

@JsonCodec
case class DebateStats(wins: Proportion.Stats, rewards: Numbers[Double])

object DebateStats {

  implicit val debateStatsMonoid: Monoid[DebateStats] = cats.derived.semiauto.monoid[DebateStats]

  def getUserStats(
    result: JudgingResult,
    role: DebateRole,
    name: String
  ): Chosen[LeaderboardCategory, Chosen[String, DebateStats]] = {
    def bool2int(b: Boolean) =
      if (b)
        1
      else
        0
    val leaderboardCategory =
      role match {
        case Debater(index) =>
          if (index == result.correctAnswerIndex) {
            LeaderboardCategory.HonestDebater
          } else
            LeaderboardCategory.DishonestDebater
        case Judge =>
          LeaderboardCategory.Judge
        case OfflineJudge =>
          LeaderboardCategory.OfflineJudge
      }
    val userStats =
      role match {
        case Debater(index) =>
          val correct = result.finalJudgement(index) > 0.5
          val reward  = math.log(result.finalJudgement(index))
          DebateStats(Proportion.Stats(bool2int(correct), bool2int(!correct)), Numbers(reward))
        case Judge | OfflineJudge =>
          val correct = result.finalJudgement(result.correctAnswerIndex) > 0.5
          DebateStats(
            Proportion.Stats(bool2int(correct), bool2int(!correct)),
            Numbers(result.judgeReward)
          )
      }
    Chosen(Map(leaderboardCategory -> Chosen(Map(name -> userStats))))
  }

  def fromDebate(d: Debate): Chosen[LeaderboardCategory, Chosen[String, DebateStats]] =
    d.result.flatMap(_.judgingInfo) match {
      case None =>
        Chosen(Map.empty)
      case Some(result) =>
        d.setup
          .roles
          .toList
          .foldMap { case (role, user) =>
            getUserStats(result, role, user)
          } |+|
          d.realOfflineJudgingResults
            .toVector
            .foldMap { case (user, offlineJudgment) =>
              offlineJudgment
                .result
                .foldMap { offlineResult =>
                  val result = JudgingResult(
                    d.setup.correctAnswerIndex,
                    offlineJudgment.numContinues,
                    offlineResult.distribution,
                    d.setup
                      .rules
                      .scoringFunction
                      .eval(
                        offlineJudgment.numContinues,
                        offlineResult.distribution,
                        d.setup.correctAnswerIndex
                      )
                  )
                }

              getUserStats(result, OfflineJudge, user)
            }
    }
}

@JsonCodec
sealed trait LeaderboardCategory extends Product with Serializable {
  import LeaderboardCategory._
  override def toString =
    this match {
      case Judge =>
        "Judge"
      case OfflineJudge =>
        "Offline Judge"
      case HonestDebater =>
        "Honest Debater"
      case DishonestDebater =>
        "Dishonest Debater"
    }

  def shortString =
    this match {
      case Judge =>
        "Judge"
      case OfflineJudge =>
        "Offline Judge"
      case HonestDebater =>
        "Honest"
      case DishonestDebater =>
        "Dishonest"
    }
}
object LeaderboardCategory {
  case object Judge            extends LeaderboardCategory
  case object OfflineJudge     extends LeaderboardCategory
  case object HonestDebater    extends LeaderboardCategory
  case object DishonestDebater extends LeaderboardCategory

  def all = Vector(Judge, OfflineJudge, HonestDebater, DishonestDebater)

  def fromString(x: String): Option[LeaderboardCategory] = all.find(_.toString == x)

  import io.circe._

  implicit val keyEncoder: KeyEncoder[LeaderboardCategory] = KeyEncoder.instance(_.toString)
  implicit val keyDecoder: KeyDecoder[LeaderboardCategory] = KeyDecoder.instance(fromString)
}

@JsonCodec
case class Leaderboard(data: Map[LeaderboardCategory, Map[String, DebateStats]]) {
  def allDebaters = data.unorderedFoldMap(_.keySet)
}

object Leaderboard {
  def fromDebates[F[_]: Foldable](debates: F[Debate]) = Leaderboard(
    debates.foldMap(DebateStats.fromDebate).data.view.mapValues(_.data).toMap
  )
}
