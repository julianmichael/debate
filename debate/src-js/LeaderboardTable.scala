package debate

import jjm.ui.LocalState
// import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import Helpers.ClassSetInterpolator
import japgolly.scalajs.react.extra.StateSnapshot

object LeaderboardTable {

  sealed trait SortableColumn {
    import SortableColumn._
    def getColumnHeader(category: LeaderboardCategory): String = this match {
      case Name => "Name"
      case Wins => "Wins"
      case Losses => "Losses"
      case WinPercentage => "Win %"
      case CustomRewardValue => category match {
        case LeaderboardCategory.Judge => "Avg Reward"
        case _ => "Avg Log Prob"
      }
    }

    def getOrdering: Ordering[RowData] = this match {
      case SortableColumn.Name   => Ordering.by[RowData, String](_.name)
      case SortableColumn.Wins   => Ordering.by[RowData, Int](_.stats.wins.included)
      case SortableColumn.Losses => Ordering.by[RowData, Int](_.stats.wins.excluded)
      case SortableColumn.WinPercentage =>
        Ordering.by[RowData, Double](_.stats.wins.proportion)
      case SortableColumn.CustomRewardValue =>
        Ordering.by[RowData, Double](_.stats.rewards.stats.mean)
    }
  }
  object SortableColumn {
    case object Name extends SortableColumn
    case object Wins extends SortableColumn
    case object Losses extends SortableColumn
    case object WinPercentage extends SortableColumn
    case object CustomRewardValue extends SortableColumn

    def all = List(Name, Wins, Losses, WinPercentage, CustomRewardValue)
  }
  case class SortingOrder(isAscending: Boolean, column: SortableColumn)

  def sortButtons[T, B](
      sortOrder: StateSnapshot[SortingOrder],
      sortableColumn: SortableColumn
  ) = {
    <.div()(
      <.i(c"bi bi-caret-down-fill")(
        ^.onClick --> {
          sortOrder.setState(
            SortingOrder(isAscending = false, column = sortableColumn)
          )
        }
      ),
      <.i(c"bi bi-caret-up-fill")(
        ^.onClick --> {
          sortOrder.setState(
            SortingOrder(isAscending = true, column = sortableColumn)
          )
        }
      )
    )
  }

  def sortBy(sortOrder: SortingOrder, rows: Vector[RowData]) = {
    val ordering = sortOrder.column.getOrdering
    if (sortOrder.isAscending) rows.sorted(ordering)
    else rows.sorted(ordering.reverse)
  }

  case class RowData(
    name: String,
    stats: DebateStats
  )

  def renderRows(
    category: LeaderboardCategory,
    stats: Map[String, DebateStats],
    sortOrder: StateSnapshot[SortingOrder]
  ) = {
    val rows = stats.toVector.map { case (name, stats) => RowData(name, stats) }
    val sortedRows = sortBy(sortOrder.value, rows)
    <.table(c"table table-striped")(
      <.thead(
        <.tr(
          SortableColumn.all.toVdomArray { column =>
            val header = column.getColumnHeader(category)
            <.th(
              ^.key := header,
              header,
              sortButtons(
                sortableColumn = column,
                sortOrder = sortOrder
              )
            )
          }
        )
      ),
      <.tbody(
        sortedRows
          .toVdomArray { case row =>
            <.tr(
              <.td(row.name),
              <.td(row.stats.wins.included),
              <.td(row.stats.wins.excluded),
              <.td(f"${row.stats.wins.proportion * 100}%.1f"),
              <.td(f"${row.stats.rewards.stats.mean}%.2f")
            )
          }
      )
    )
  }

  val defaultSortingOrder =
    SortingOrder(isAscending = true, SortableColumn.Name)

  val LocalSortingOrder = new LocalState[SortingOrder]

  def renderSingleLeaderboard(
    category: LeaderboardCategory,
    stats: Map[String, DebateStats]
  ) = {
    LocalSortingOrder.make(defaultSortingOrder) { sortOrder =>
      <.div(
        ^.key := s"leaderboard-$category",
        <.h3(category.toString),
        renderRows(
          category = category,
          stats = stats,
          sortOrder = sortOrder,
        )
      )
    }
  }

  def make(leaderboard: Leaderboard): japgolly.scalajs.react.vdom.VdomElement = {
    import LeaderboardCategory._
    <.div(
      List(Judge, HonestDebater, DishonestDebater).flatMap(category =>
        leaderboard.data.get(category).map(rows =>
          renderSingleLeaderboard(category, rows)
        )
      ).toVdomArray
    )
  }
}
