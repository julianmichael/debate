package debate

import cats._
import cats.implicits._

import io.circe.generic.JsonCodec

import jjm.metrics._

@JsonCodec
case class DebateStats(wins: Proportion.Stats, rewards: Numbers[Double])

object DebateStats {

  implicit val debateStatsMonoid: Monoid[DebateStats] = cats.derived.semiauto.monoid[DebateStats]

  private def bool2int(b: Boolean) =
    if (b)
      1
    else
      0

  def getDebaterStats(
    dist: Vector[Double],
    correctAnswerIndex: Int,
    role: Debater,
    name: String
  ): Chosen[LeaderboardCategory, Chosen[String, DebateStats]] = {
    val leaderboardCategory =
      if (role.answerIndex == correctAnswerIndex) {
        LeaderboardCategory.HonestDebater
      } else
        LeaderboardCategory.DishonestDebater
    val winning = dist(role.answerIndex) > 0.5
    val reward  = math.log(dist(role.answerIndex))
    val stats = DebateStats(
      Proportion.Stats(bool2int(winning), bool2int(!winning)),
      Numbers(reward)
    )
    Chosen(Map(leaderboardCategory -> Chosen(Map(name -> stats))))
  }

  def getJudgeStats(
    result: JudgingResult,
    role: JudgeRole,
    name: String
  ): Chosen[LeaderboardCategory, Chosen[String, DebateStats]] = {
    val leaderboardCategory =
      role match {
        case Judge =>
          LeaderboardCategory.Judge
        case OfflineJudge =>
          LeaderboardCategory.OfflineJudge
      }
    val correct = result.finalJudgement(result.correctAnswerIndex) > 0.5
    val stats = DebateStats(
      Proportion.Stats(bool2int(correct), bool2int(!correct)),
      Numbers(result.judgeReward)
    )
    Chosen(Map(leaderboardCategory -> Chosen(Map(name -> stats))))
  }

  def fromDebate(d: Debate): Chosen[LeaderboardCategory, Chosen[String, DebateStats]] = {
    val liveParticipantStats = d
      .setup
      .roles
      .toList
      .foldMap {
        case (Debater(i), user) =>
          d.result
            .flatMap(_.judgingInfo)
            .map(_.finalJudgement: Vector[Double])
            .orElse(
              d.offlineJudgingResults
                .values
                .toList
                .flatMap(_.result)
                .toNel
                .map(dists =>
                  Utils.normalize(
                    dists
                      .map(_.distribution)
                      .reduce((p1: Vector[Double], p2: Vector[Double]) =>
                        p1.zip(p2)
                          .map { case (x, y) =>
                            x + y
                          }
                      )
                  )
                ): Option[Vector[Double]]
            )
            .foldMap(getDebaterStats(_, d.setup.correctAnswerIndex, Debater(i), user))
        case (Judge, user) =>
          d.result.flatMap(_.judgingInfo).foldMap(getJudgeStats(_, Judge, user))
      }
    val offlineJudgingStats = d
      .offlineJudgingResults
      .toVector
      .foldMap { case (user, offlineJudgment) =>
        offlineJudgment
          .result
          .foldMap { offlineResult =>
            val result = JudgingResult(
              d.setup.correctAnswerIndex,
              offlineJudgment.judgments.size,
              offlineResult.distribution,
              d.setup
                .rules
                .scoringFunction
                .eval(
                  offlineJudgment.judgments.size,
                  offlineResult.distribution,
                  d.setup.correctAnswerIndex
                )
            )
            getJudgeStats(result, OfflineJudge, user)
          }
      }
    liveParticipantStats |+| offlineJudgingStats
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
case class Leaderboard(
  data: Map[LeaderboardCategory, Map[String, DebateStats]],
  ratings: Elo.Ratings,
  oneWeekOldRatings: Elo.Ratings
) {
  def allDebaters = data.unorderedFoldMap(_.keySet)
}

object Leaderboard {
  private val oneWeekMillis = 604800000L
  def fromDebates[F[_]: Foldable](_debates: F[Debate]) = {
    // filter out debates from pre-2023
    val debates = _debates
      .toList
      .filter(_.setup.creationTime > timeBeforeWhichToIgnoreMissingFeedback)
    val data     = debates.foldMap(DebateStats.fromDebate).data.view.mapValues(_.data).toMap
    val debaters = debates.foldMap(_.setup.participants)
    val ratings  = Elo.computeRatings(debates.toList.toVector, debaters)
    val oneWeekOldRatings = Elo.computeRatings(
      debates.toList.toVector,
      debaters,
      timeCutoff = Some(System.currentTimeMillis() - oneWeekMillis)
    )
    Leaderboard(data, ratings, oneWeekOldRatings)
  }
}
