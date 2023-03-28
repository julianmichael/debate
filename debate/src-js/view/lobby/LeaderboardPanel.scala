package debate
package view.lobby

import cats.Id
import cats.implicits._
import cats.kernel.Order

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.util.Local
import jjm.DotMap
import japgolly.scalajs.react.feature.ReactFragment
import jjm.ui.Rgba

object LeaderboardPanel {

  val S = Styles

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
    private case class Impl[Out0](
      header: String,
      isRightAligned: Boolean,
      show: Out0 => VdomTag,
      order: Order[Out0]
    ) extends Column {
      type Out = Out0
    }
    def apply[Out0](header: String, isRightAligned: Boolean, show: Out0 => VdomTag)(
      implicit order: Order[Out0]
    ): Column {
      type Out = Out0
    } = Impl(header, isRightAligned, show, order)

    def withStringShow[Out0](header: String, isRightAligned: Boolean, show: Out0 => String)(
      implicit order: Order[Out0]
    ): Column {
      type Out = Out0
    } = Impl(header, isRightAligned, o => <.span(show(o)), order)

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

    def showRating(rating: Double) = {
      val color = getColorFromCorrectnessScore(Elo.sigmoid2(rating)).toColorStyleString
      <.strong(^.color := color, f"${math.pow(2, rating)}%.2f")
    }
    // f"${math.pow(2, rating)}%.2f (${Elo.sigmoid2(rating) * 100.0}%.0f%%)"

    val debated      = Column.withStringShow[Int]("Debated", true, _.toString)
    val debateRating = Column[Double]("Debate Rating", true, showRating)
    val judged       = Column.withStringShow[Int]("Judged", true, _.toString)
    val judgeRating  = Column[Double]("Judge Rating", true, showRating)
    val rating       = Column[Double]("Avg Rating", true, showRating)

    // list of all columns
    val allForRoleLeaderboard    = List(name, wins, losses, winPercentage, reward)
    val allForRatingsLeaderboard = List(name, debated, debateRating, judged, judgeRating, rating)
  }

  type RowData = DotMap[Id, Column]
  def isRowEmpty(row: RowData) = row.get(Column.count).exists(_.isEmpty)

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
    debatingStats: DebateStats,
    judgingStats: DebateStats,
    ratings: Elo.Ratings
  ) =
    DotMap
      .empty[Id, Column]
      .put(Column.name)(name)
      .put(Column.count)(debatingStats.wins.total + judgingStats.wins.total)
      .put(Column.debated)(debatingStats.wins.total)
      .put(Column.debateRating)(ratings.debaterSkills(name))
      .put(Column.judged)(judgingStats.wins.total)
      .put(Column.judgeRating)(ratings.judgeSkills(name))
      .put(Column.rating)((ratings.debaterSkills(name) + ratings.judgeSkills(name)) / 2)

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

  val defaultRoleSortingOrder    = SortingOrder(isAscending = false, Column.reward)
  val defaultRatingsSortingOrder = SortingOrder(isAscending = false, Column.rating)

  def makeRatingsLeaderboard(debaters: Set[String], leaderboard: Leaderboard) = {
    val debatingStats = List(
      LeaderboardCategory.HonestDebater,
      LeaderboardCategory.DishonestDebater
    ).flatMap(leaderboard.data.get)
      .foldMap(data => debaters.map(d => d -> data.get(d).combineAll).toMap)
    val judgingStats = List(LeaderboardCategory.Judge, LeaderboardCategory.OfflineJudge)
      .flatMap(leaderboard.data.get)
      .foldMap(data => debaters.map(d => d -> data.get(d).combineAll).toMap)

    Local[SortingOrder].make(defaultRatingsSortingOrder) { sortOrder =>
      <.div(
        ^.key := s"leaderboard-ratings",
        <.h3("Skill Ratings"),
        renderRows(
          rows = debaters
            .toVector
            .map(debater =>
              makeRatingsRowData(
                debater,
                debatingStats(debater),
                judgingStats(debater),
                leaderboard.ratings
              )
            ),
          columns = Column.allForRatingsLeaderboard,
          rankColumn = Column.rating,
          sortOrder = sortOrder
        )
      )
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

  def apply(lobby: Lobby) = {
    def makeRatingsTab = TabNav.tab(
      <.div(c"card-body", S.spaceySubcontainer)(
        makeRatingsLeaderboard(lobby.profiles.keySet, lobby.leaderboard)
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
