package debate
package view.lobby

import cats.implicits._
import cats.kernel.Monoid
import cats.kernel.Order

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.util.Local

object LeaderboardPanel {

  val S = Styles

  sealed trait SortableColumn {
    import SortableColumn._
    def getColumnHeader: String =
      this match {
        case Name =>
          "Name"
        case Wins =>
          "Wins"
        case Losses =>
          "Losses"
        case WinPercentage =>
          "Win %"
        case Reward =>
          "Avg Reward"
      }

    def getOrdering(isAscending: Boolean): Order[RowData] = {
      val fieldOrder =
        this match {
          case SortableColumn.Name =>
            Order.by[RowData, String](_.name)
          case SortableColumn.Wins =>
            Order.by[RowData, Int](_.stats.wins.included)
          case SortableColumn.Losses =>
            Order.by[RowData, Int](_.stats.wins.excluded)
          case SortableColumn.WinPercentage =>
            Order.by[RowData, Option[Double]](_.winProportion)
          case SortableColumn.Reward =>
            Order.by[RowData, Option[Double]](_.avgReward)
        }
      val directionalFieldOrder =
        if (isAscending)
          fieldOrder
        else
          Order.reverse(fieldOrder)
      Order.whenEqual(Order.by[RowData, Boolean](_.isEmpty), directionalFieldOrder)
    }
  }
  object SortableColumn {
    case object Name          extends SortableColumn
    case object Wins          extends SortableColumn
    case object Losses        extends SortableColumn
    case object WinPercentage extends SortableColumn
    case object Reward        extends SortableColumn

    def all = List(Name, Wins, Losses, WinPercentage, Reward)
  }

  @Lenses
  case class SortingOrder(isAscending: Boolean, column: SortableColumn)

  def sortBy(sortOrder: SortingOrder, rows: Vector[RowData]) = {
    implicit val order = sortOrder.column.getOrdering(sortOrder.isAscending)
    rows.sorted
  }

  case class RowData(name: String, stats: DebateStats) {
    def isEmpty = stats == Monoid[DebateStats].empty
    def winProportion =
      if (stats.wins.total == 0)
        None
      else
        Some(stats.wins.proportion)

    def avgReward =
      if (stats.rewards.values.size == 0)
        None
      else
        Some(stats.rewards.stats.mean)
  }

  def renderRows(stats: Map[String, DebateStats], sortOrder: StateSnapshot[SortingOrder]) = {
    val rows = stats
      .toVector
      .map { case (name, stats) =>
        RowData(name, stats)
      }
    val sortedRows = sortBy(sortOrder.value, rows)
    <.table(c"table table-striped")(
      <.thead(
        <.tr(
          <.th(c"text-right")("#"),
          SortableColumn
            .all
            .toVdomArray { column =>
              val header      = column.getColumnHeader
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
        sortedRows
          .map(row =>
            row ->
              row.avgReward.map(reward => sortedRows.filter(_.avgReward.exists(_ > reward)).size)
          )
          .toVdomArray { case (row, rankOpt) =>
            val trModForNoRecords =
              if (row.isEmpty) {
                TagMod(S.simpleUnselectable)
              } else
                TagMod.empty
            <.tr(trModForNoRecords)(
              ^.key := row.name,
              <.td(c"text-right")(rankOpt.fold("-")(r => s"${r + 1}")),
              <.td(row.name),
              <.td(c"text-right")(row.stats.wins.included),
              <.td(c"text-right")(row.stats.wins.excluded),
              <.td(c"text-right")(
                row.winProportion.fold("-")(winProp => f"${winProp * 100}%.0f%%")
              ),
              <.td(c"text-right")(row.avgReward.fold("-")(reward => f"$reward%.2f"))
            )
          }
      )
    )
  }

  val defaultSortingOrder = SortingOrder(isAscending = false, SortableColumn.Reward)

  def renderSingleLeaderboard(category: LeaderboardCategory, stats: Map[String, DebateStats]) =
    Local[SortingOrder].make(defaultSortingOrder) { sortOrder =>
      <.div(
        ^.key := s"leaderboard-$category",
        <.h3(category.toString),
        renderRows(stats = stats, sortOrder = sortOrder)
      )
    }

  def makeSingle(debaters: Set[String], leaderboard: Leaderboard, category: LeaderboardCategory) =
    leaderboard
      .data
      .get(category)
      .map { data =>
        val fullData = debaters.map(d => d -> data.get(d).combineAll).toMap
        renderSingleLeaderboard(category, fullData)
      }

  def apply(lobby: Lobby) = {
    def makeTab(category: LeaderboardCategory) = TabNav.tab(
      <.div(c"card-body", S.spaceySubcontainer)(
        makeSingle(lobby.profiles.keySet, lobby.leaderboard, category)
      )
    )
    TabNav("leaderboard-tab", 0)(
      "Live Judge"        -> makeTab(LeaderboardCategory.Judge),
      "Offline Judge"     -> makeTab(LeaderboardCategory.OfflineJudge),
      "Honest Debater"    -> makeTab(LeaderboardCategory.HonestDebater),
      "Dishonest Debater" -> makeTab(LeaderboardCategory.DishonestDebater)
    )

  }
}
