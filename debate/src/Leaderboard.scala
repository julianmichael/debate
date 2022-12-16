package debate

import io.circe.generic.JsonCodec

object Leaderboard {
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

  object LeaderboardCategories {
    @JsonCodec
    sealed trait LeaderboardCategory

    case object Judge extends LeaderboardCategory
    case object HonestDebater extends LeaderboardCategory
    case object DishonestDebater extends LeaderboardCategory

    import io.circe._, io.circe.generic.semiauto._

    implicit val lcDecoder: Decoder[LeaderboardCategory] =
      deriveDecoder[LeaderboardCategory]
    implicit val lcEncoder: Encoder[LeaderboardCategory] =
      deriveEncoder[LeaderboardCategory]
  }

  @JsonCodec
  case class SerializableDebateStats(
      wins: Int,
      losses: Int,
      averageReward: Double
  )

  @JsonCodec
  case class T(
      judge: Map[String, SerializableDebateStats],
      honest: Map[String, SerializableDebateStats],
      dishonest: Map[String, SerializableDebateStats]
  )

  object SerializableDebateStats {
    def ofDebateStats(d: DebateStats): SerializableDebateStats =
      SerializableDebateStats(
        wins = d.wins.correct,
        losses = d.wins.incorrect,
        averageReward = d.rewards.stats.mean
      )

    import io.circe._, io.circe.generic.semiauto._
    implicit val a = deriveDecoder[SerializableDebateStats]
    implicit val b = deriveEncoder[SerializableDebateStats]
  }

  def ofDebateStates(d: List[Debate]): Map[
    LeaderboardCategories.LeaderboardCategory,
    Map[String, SerializableDebateStats]
  ] = {

    g(d).data
      .mapValues(
        _.data.mapValues(SerializableDebateStats.ofDebateStats).toMap
      )
      .toMap
  }

  import io.circe._, io.circe.generic.semiauto._
  import LeaderboardCategories._
  import SerializableDebateStats._
  implicit val a = deriveDecoder[Map[
    LeaderboardCategories.LeaderboardCategory,
    Map[String, SerializableDebateStats]
  ]]

  implicit val b = deriveEncoder[Map[
    LeaderboardCategories.LeaderboardCategory,
    Map[String, SerializableDebateStats]
  ]]
}
