package debate
package view.lobby

import cats.Id
import cats.implicits._
import cats.kernel.Order

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

import jjm.DotMap
import jjm.ui.Rgba

import debate.Utils.ClassSetInterpolator
import debate.util.Local

object LeaderboardPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  sealed trait Column {
    type Out
    def header: String
    def order: Order[Out]
    def isRightAligned: Boolean
    val show: Out => VdomTag

    def getRowOrder(isAscending: Boolean): Order[DotMap[Id, Column]] = {
      implicit val directionalOrder =
        if (isAscending)
          order
        else
          Order.reverse(order)
      Order.by[DotMap[Id, Column], Option[Out]](_.get(this))
    }
  }
  object Column {
    private case class Impl[Out0](header: String)(
      val isRightAligned: Boolean,
      val show: Out0 => VdomTag,
      val order: Order[Out0]
    ) extends Column {
      type Out = Out0
    }
    def apply[Out0](header: String, isRightAligned: Boolean, show: Out0 => VdomTag)(
      implicit order: Order[Out0]
    ): Column {
      type Out = Out0
    } = Impl(header)(isRightAligned, show, order)

    def withStringShow[Out0](header: String, isRightAligned: Boolean, show: Out0 => String)(
      implicit order: Order[Out0]
    ): Column {
      type Out = Out0
    } = Impl(header)(isRightAligned, o => <.span(show(o)), order)

    val count = Column.withStringShow[Int]("Count", true, _.toString)

    // leaderboard columns
    val name   = Column.withStringShow[String]("Name", false, identity)
    val wins   = Column.withStringShow[Int]("Wins", true, _.toString)
    val losses = Column.withStringShow[Int]("Losses", true, _.toString)
    val winPercentage = Column.withStringShow[Option[Double]](
      "Win %",
      true,
      _.fold("-")(winProp => f"${winProp * 100}%.0f%%")
    )
    val reward = Column
      .withStringShow[Option[Double]]("Avg Reward", true, _.fold("-")(reward => f"$reward%.2f"))

    def getColorFromCorrectnessScore(correctnessScore: Double) = Rgba(
      math.max(0.0, 255.0 * math.sqrt(0.5 - correctnessScore) * 2.0 * math.sqrt(0.5)).toInt,
      math.max(0.0, 255.0 * math.sqrt(correctnessScore - 0.5) * 2.0 * math.sqrt(0.5)).toInt,
      0,
      1.0
    )

    def showRating(useWinPercentage: Boolean)(rating: Double) = {
      val color = getColorFromCorrectnessScore(Elo.sigmoid2(rating)).toColorStyleString
      <.strong(
        ^.color := color,
        if (useWinPercentage)
          f"${Elo.sigmoid2(rating) * 100.0}%.0f%%"
        else
          f"${math.pow(2, rating)}%.2f"
      )
    }
    // f"${math.pow(2, rating)}%.2f (${Elo.sigmoid2(rating) * 100.0}%.0f%%)"

    def honestRating(useWinPercentage: Boolean) = Column[Double](
      "Honest",
      true,
      showRating(useWinPercentage)
    )
    val honestN = Column.withStringShow[Int]("#H", true, _.toString)
    def dishonestRating(useWinPercentage: Boolean) = Column[Double](
      "Dishonest",
      true,
      showRating(useWinPercentage)
    )
    val dishonestN = Column.withStringShow[Int]("#D", true, _.toString)
    def judgeLeadRating(useWinPercentage: Boolean) = Column[Double](
      "Steer",
      true,
      showRating(useWinPercentage)
    )
    val judgeLedN = Column.withStringShow[Int]("#S", true, _.toString)
    def judgeJudgeRating(useWinPercentage: Boolean) = Column[Double](
      "Judge",
      true,
      showRating(useWinPercentage)
    )
    val judgeJudgedN = Column.withStringShow[Int]("#J", true, _.toString)
    def rating(useWinPercentage: Boolean) = Column[Double](
      "Avg Rating",
      true,
      showRating(useWinPercentage)
    )
    def ratingDelta(useWinPercentage: Boolean) = Column[Double](
      "1wk Δ",
      true,
      showRating(useWinPercentage)
    )

    // list of all columns
    val allForRoleLeaderboard = List(name, wins, losses, winPercentage, reward)
    def allForRatingsLeaderboard(useWinPercentage: Boolean) = List(
      name,
      honestRating(useWinPercentage),
      honestN,
      dishonestRating(useWinPercentage),
      dishonestN,
      judgeLeadRating(useWinPercentage),
      judgeLedN,
      judgeJudgeRating(useWinPercentage),
      judgeJudgedN,
      rating(useWinPercentage),
      ratingDelta(useWinPercentage)
    )
  }

  type RowData = DotMap[Id, Column]
  def isRowEmpty(row: RowData) = row.get(Column.count).exists(_ == 0)

  @Lenses
  case class SortingOrder(isAscending: Boolean, column: Column)

  def sortBy(sortOrder: SortingOrder, rows: Vector[RowData]) = {
    implicit val order = sortOrder.column.getRowOrder(sortOrder.isAscending)
    rows.sorted
  }

  def makeRoleRowData(name: String, stats: DebateStats) =
    DotMap
      .empty[Id, Column]
      .put(Column.name)(name)
      .put(Column.count)(stats.wins.total)
      .put(Column.wins)(stats.wins.included)
      .put(Column.losses)(stats.wins.excluded)
      .put(Column.winPercentage)(
        if (stats.wins.total == 0)
          None
        else
          Some(stats.wins.proportion)
      )
      .put(Column.reward)(
        if (stats.rewards.values.size == 0)
          None
        else
          Some(stats.rewards.stats.mean)
      )

  def makeRatingsRowData(
    name: String,
    honestStats: DebateStats,
    dishonestStats: DebateStats,
    liveJudgingStats: DebateStats,
    allJudgingStats: DebateStats,
    ratings: Elo.Ratings,
    oneWeekOldRatings: Elo.Ratings,
    showRatingsAsWinPercentage: Boolean
  ) = {
    val rating    = ratings.averageSkills(name)
    val oldRating = oneWeekOldRatings.averageSkills(name)
    DotMap
      .empty[Id, Column]
      .put(Column.name)(name)
      .put(Column.count)(
        dishonestStats.wins.total + honestStats.wins.total + allJudgingStats.wins.total
      )
      .put(Column.honestRating(showRatingsAsWinPercentage))(ratings.honestSkills(name))
      .put(Column.honestN)(honestStats.wins.total)
      .put(Column.dishonestRating(showRatingsAsWinPercentage))(ratings.dishonestSkills(name))
      .put(Column.dishonestN)(dishonestStats.wins.total)
      .put(Column.judgeLeadRating(showRatingsAsWinPercentage))(ratings.judgeLeadingSkills(name))
      .put(Column.judgeLedN)(liveJudgingStats.wins.total)
      .put(Column.judgeJudgeRating(showRatingsAsWinPercentage))(ratings.judgeJudgingSkills(name))
      .put(Column.judgeJudgedN)(allJudgingStats.wins.total)
      .put(Column.rating(showRatingsAsWinPercentage))(rating)
      .put(Column.ratingDelta(showRatingsAsWinPercentage))(rating - oldRating)
  }

  def renderRows(
    rows: Vector[RowData],
    columns: List[Column],
    rankColumn: Column,
    sortOrder: StateSnapshot[SortingOrder]
  ) = {
    val sortedRows: Vector[(RowData, Option[Int])] = sortBy(sortOrder.value, rows).map { row =>
      implicit val o = rankColumn.order
      row -> row.get(rankColumn).map(item => rows.filter(_.get(rankColumn).exists(_ > item)).size)
    }
    <.table(c"table table-striped")(
      <.thead(
        <.tr(
          <.th(c"text-right")("#"),
          columns.toVdomArray { column =>
            val header      = column.header
            val sortIconTag = <.i(c"fa ml-2")
            val (sortIcon, thMod) =
              sortOrder.value match {
                case SortingOrder(isAscending, `column`) =>
                  val icon =
                    if (isAscending)
                      sortIconTag(c"fa-sort-up")
                    else
                      sortIconTag(c"fa-sort-down")
                  val callback = sortOrder.zoomStateL(SortingOrder.isAscending).modState(!_)
                  icon -> TagMod(c"table-active", ^.onClick --> callback)
                case _ =>
                  val icon = sortIconTag(c"fa-sort", ^.color := "#ddd")
                  icon ->
                    TagMod(
                      ^.onClick -->
                        sortOrder.setState(SortingOrder(isAscending = false, column = column))
                    )
              }
            <.th(thMod, S.simpleSelectable, c"text-center")(
              ^.key := header,
              <.div(S.row, c"w-100", ^.alignItems.center)(<.span(S.grow)(header), sortIcon)
            )
          }
        )
      ),
      <.tbody(
        sortedRows.toVdomArray { case (row, rankOpt) =>
          val trModForNoRecords =
            if (isRowEmpty(row)) {
              TagMod(S.simpleUnselectable)
            } else
              TagMod.empty
          <.tr(trModForNoRecords)(
            ^.key := row.get(Column.name).get,
            <.td(c"text-right")(rankOpt.map(r => s"${r + 1}").getOrElse[String]("-")),
            ReactFragment(
              columns.map { column =>
                <.td(c"text-right".when(column.isRightAligned))(column.show(row.get(column).get))
              }: _*
            )
          )
        }
      )
    )
  }

  val defaultRoleSortingOrder = SortingOrder(isAscending = false, Column.reward)
  def defaultRatingsSortingOrder(showRatingsAsWinPercentage: Boolean) = SortingOrder(
    isAscending = false,
    Column.rating(showRatingsAsWinPercentage)
  )

  def makeRatingsLeaderboard(
    debaters: Set[String],
    leaderboard: Leaderboard,
    refreshLeaderboard: Callback
  ) = {
    val honestStats = List(LeaderboardCategory.HonestDebater)
      .flatMap(leaderboard.data.get)
      .foldMap(data => debaters.map(d => d -> data.get(d).combineAll).toMap)

    val dishonestStats = List(LeaderboardCategory.DishonestDebater)
      .flatMap(leaderboard.data.get)
      .foldMap(data => debaters.map(d => d -> data.get(d).combineAll).toMap)

    val liveJudgingStats = List(LeaderboardCategory.Judge)
      .flatMap(leaderboard.data.get)
      .foldMap(data => debaters.map(d => d -> data.get(d).combineAll).toMap)

    val allJudgingStats = List(LeaderboardCategory.Judge, LeaderboardCategory.OfflineJudge)
      .flatMap(leaderboard.data.get)
      .foldMap(data => debaters.map(d => d -> data.get(d).combineAll).toMap)

    Local[Boolean].make(false) { showRatingsAsWinPercentage =>
      Local[SortingOrder].make(defaultRatingsSortingOrder(showRatingsAsWinPercentage.value)) {
        sortOrder =>
          Local[Boolean].make(false) { showInfo =>
            <.div(
              ^.key := s"leaderboard-ratings",
              <.h3(c"card-title")("Skill Ratings"),
              <.div(c"pb-2")(
                <.a(c"card-link")(
                  ^.href := "#",
                  if (showInfo.value)
                    "What do these ratings mean? (Hide)"
                  else
                    "What do these ratings mean?",
                  ^.onClick --> showInfo.modState(!_)
                )
              ),
              <.div(c"alert alert-info")(
                  <.p(
                    "Skill ratings are calculated using a method similar to the ",
                    <.a(
                      ^.href := "https://en.wikipedia.org/wiki/Elo_rating_system",
                      "Elo rating system"
                    ),
                    ". Your displayed skill rating is your odds of winning a debate (or judging it correctly), ",
                    " statistically corrected for the skill ratings of the other debate participants, ",
                    " the difficulty of arguing for an incorrect answer, the difficulty of particular questions, ",
                    " and the difficulty of offline versus online judging. ",
                    " For example, a debater with a skill rating of 2.0 gets a 2x higher probability assigned to their answer ",
                    " than their opponent in the average case. Your judging skill is split into two scores: \"Steer\" ",
                    " measures how effectively you steer the debate as a live judge, and \"Judge\" measures how well you judge ",
                    " the correct answer given a debate's contents. "
                  ),
                  <.p(c"mb-0")(
                    "1wk Δ (one week delta) is the change in rating (multiplicative) over the past week. ",
                    <.strong(
                      "Each meeting, we will choose the best NYU debater in one of the categories, and they will get to choose ",
                      "the food order for the following week :)."
                    )
                  )
                )
                .when(showInfo.value),
              <.div(c"pb-2")(
                <.a(c"card-link")(^.href := "#", "Refresh", ^.onClick --> refreshLeaderboard),
                <.span(c"card-link")(
                  V.Checkbox(
                    showRatingsAsWinPercentage,
                    labelOpt = Some("Display rating as win percentage")
                  )
                ),
                <.span(c"card-link")(
                  f"Global bias: ",
                  Column
                    .showRating(showRatingsAsWinPercentage.value)(leaderboard.ratings.globalBias)
                ),
                <.span(c"card-link")(
                  f"Offline judging adjustment: ",
                  Column.showRating(showRatingsAsWinPercentage.value)(
                    leaderboard.ratings.noLiveJudgeAdjustment
                  )
                ),
                <.span(c"card-link")(
                  f"No opponent adjustment: ",
                  Column.showRating(showRatingsAsWinPercentage.value)(
                    leaderboard.ratings.noOpponentAdjustment
                  )
                )
              ),
              renderRows(
                rows = debaters
                  .toVector
                  .map(debater =>
                    makeRatingsRowData(
                      debater,
                      honestStats(debater),
                      dishonestStats(debater),
                      liveJudgingStats(debater),
                      allJudgingStats(debater),
                      leaderboard.ratings,
                      leaderboard.oneWeekOldRatings,
                      showRatingsAsWinPercentage.value
                    )
                  ),
                columns = Column.allForRatingsLeaderboard(showRatingsAsWinPercentage.value),
                rankColumn = Column.rating(showRatingsAsWinPercentage.value),
                sortOrder = sortOrder
              )
            )
          }
      }
    }
  }

  def makeSingleRoleLeaderboard(
    debaters: Set[String],
    leaderboard: Leaderboard,
    category: LeaderboardCategory
  ) = leaderboard
    .data
    .get(category)
    .map { data =>
      val stats = debaters.map(d => d -> data.get(d).combineAll).toMap
      Local[SortingOrder].make(defaultRoleSortingOrder) { sortOrder =>
        <.div(
          ^.key := s"leaderboard-$category",
          <.h3(category.toString),
          renderRows(
            rows = stats
              .toVector
              .map { case (name, stats) =>
                makeRoleRowData(name, stats)
              },
            columns = Column.allForRoleLeaderboard,
            rankColumn = Column.reward,
            sortOrder = sortOrder
          )
        )
      }
    }

  def apply(lobby: Lobby, refreshLeaderboard: Callback) = {
    def makeRatingsTab = TabNav.tab(
      <.div(c"card-body", S.spaceySubcontainer)(
        makeRatingsLeaderboard(lobby.profiles.keySet, lobby.leaderboard, refreshLeaderboard)
      )
    )
    def makeRoleTab(category: LeaderboardCategory) = TabNav.tab(
      <.div(c"card-body", S.spaceySubcontainer)(
        makeSingleRoleLeaderboard(lobby.profiles.keySet, lobby.leaderboard, category)
      )
    )
    TabNav("leaderboard-tab", 0)(
      "Skill Ratings"     -> makeRatingsTab,
      "Live Judge"        -> makeRoleTab(LeaderboardCategory.Judge),
      "Offline Judge"     -> makeRoleTab(LeaderboardCategory.OfflineJudge),
      "Honest Debater"    -> makeRoleTab(LeaderboardCategory.HonestDebater),
      "Dishonest Debater" -> makeRoleTab(LeaderboardCategory.DishonestDebater)
    )

  }
}
