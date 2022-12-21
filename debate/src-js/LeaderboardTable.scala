package debate

import scala.language.existentials

import jjm.ui.CacheCallContent
import jjm.ui.LocalState
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.Future
import Helpers.ClassSetInterpolator
import jjm.OrWrapped
import japgolly.scalajs.react.extra.StateSnapshot

object LeaderboardTable {
  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  /** performs a GET request to /leaderboard and returns a parsed Leaderboard
    * object
    */
  def loadLeaderboard(): Future[Leaderboard] = {
    org.scalajs.dom.ext.Ajax
      .get(url = "/leaderboard")
      .map(resp => io.circe.parser.decode[Leaderboard](resp.responseText))
      .flatMap {
        case Right(res) => Future.successful(res)
        case Left(fail) => Future.failed(new RuntimeException(fail))
      }
  }

  case class RowEntry(
      name: String,
      wins: Int,
      losses: Int,
      customRewardValue: Double // e.g. either log prob or the application or the scoring function
  )
  case class ParsedResponse(
      judge: List[RowEntry],
      honest: List[RowEntry],
      dishonest: List[RowEntry]
  )

  sealed trait SortableColumn
  object SortableColumn {
    case object Name extends SortableColumn
    case object Wins extends SortableColumn
    case object Losses extends SortableColumn
    case object WinPercentage extends SortableColumn
    case object CustomRewardValue extends SortableColumn
  }
  case class SortingOrder(isAscending: Boolean, column: SortableColumn)

  def sortButtons[T, B](
      sortOrderRef: StateSnapshot[SortingOrder],
      sortableColumn: SortableColumn
  ) = {
    <.div()(
      <.i(c"bi bi-caret-down-fill")(
        ^.onClick --> {
          sortOrderRef.setState(
            SortingOrder(isAscending = true, column = sortableColumn)
          )
        }
      ),
      <.i(c"bi bi-caret-up-fill")(
        ^.onClick --> {
          sortOrderRef.setState(
            SortingOrder(isAscending = false, column = sortableColumn)
          )
        }
      )
    )
  }

  def make() = {
    def sortBy(s: SortingOrder, rows: List[RowEntry]) = {
      val (isAscending, column) = (s.isAscending, s.column)
      val ordering = column match {
        case SortableColumn.Name   => Ordering.by[RowEntry, String](_.name)
        case SortableColumn.Wins   => Ordering.by[RowEntry, Int](_.wins)
        case SortableColumn.Losses => Ordering.by[RowEntry, Int](_.losses)
        case SortableColumn.WinPercentage =>
          Ordering.by[RowEntry, Double] { rowEntry =>
            rowEntry.wins.toDouble / (rowEntry.wins + rowEntry.losses)
          }
        case SortableColumn.CustomRewardValue =>
          Ordering.by[RowEntry, Double](_.customRewardValue)
      }
      if (isAscending) rows.sorted(ordering)
      else rows.sorted(ordering.reverse)
    }

    def renderRowEntries(
        rows: List[RowEntry],
        sortOrderRef: StateSnapshot[SortingOrder],
        customColumnName: String
    ) = {
      def ourSortButtons(x: SortableColumn) =
        sortButtons(
          sortableColumn = x,
          sortOrderRef = sortOrderRef
        )
      val sortedRows = sortBy(sortOrderRef.value, rows)
      <.table(c"table table-striped")(
        <.thead(
          <.tr(
            <.th(
              "Name",
              ourSortButtons(SortableColumn.Name)
            ),
            <.th(
              "Wins",
              ourSortButtons(SortableColumn.Wins)
            ),
            <.th(
              "Losses",
              ourSortButtons(SortableColumn.Losses)
            ),
            // TODO someday unify this code with the sortBy bits and the rows; one nasty bit is handling the variable printf specifications
            <.th(
              "Win %",
              ourSortButtons(SortableColumn.WinPercentage)
            ),
            <.th(
              customColumnName,
              ourSortButtons(SortableColumn.CustomRewardValue)
            )
          )
        ),
        <.tbody(
          sortedRows
            .toVdomArray { case rowEntry =>
              <.tr(
                <.td(rowEntry.name),
                <.td(rowEntry.wins),
                <.td(rowEntry.losses),
                <.td(
                  f"${rowEntry.wins.toDouble / (rowEntry.losses + rowEntry.wins) * 100}%.1f"
                ),
                <.td(f"${rowEntry.customRewardValue}%.2f")
              )
            }
        )
      )
    }

    def renderLoaded(parsedResponse: ParsedResponse) = {
      // TODO add the ability to highlight the sorted column
      val baseSortingOrder =
        SortingOrder(isAscending = true, SortableColumn.Name)
      (new LocalState[SortingOrder]).make(baseSortingOrder) { judgeSortOrder =>
        (new LocalState[SortingOrder]).make(baseSortingOrder) {
          honestSortOrder =>
            (new LocalState[SortingOrder]).make(baseSortingOrder) {
              dishonestSortOrder =>
                import LeaderboardCategories._
                def f(x: LeaderboardCategory, name: String) = {
                  val (sortOrder, rows) = x match {
                    case Judge => (judgeSortOrder, parsedResponse.judge)
                    case HonestDebater =>
                      (honestSortOrder, parsedResponse.honest)
                    case DishonestDebater =>
                      (dishonestSortOrder, parsedResponse.dishonest)
                  }
                  renderRowEntries(
                    rows = rows,
                    sortOrderRef = sortOrder,
                    customColumnName = name
                  )
                }
                <.div(
                  <.h3("Judge"),
                  f(Judge, "Average Reward"),
                  <.h3("Honest"),
                  f(HonestDebater, "Average Log Prob"),
                  <.h3("Dishonest"),
                  f(DishonestDebater, "Average Log Prob")
                )
            }
        }
      }
    }

    val C = new CacheCallContent[Unit, ParsedResponse]()
    def render(state: C.State): japgolly.scalajs.react.vdom.VdomElement = {
      state match {
        case C.Loading =>
          <.div(
            <.h3("Loading Leaderboard...")
          )
        case C.Loaded(x) =>
          renderLoaded(x)
      }
    }

    def leaderboardToRows(
        x: Map[String, SerializableDebateStats]
    ) = {
      val init: List[RowEntry] = List()
      x.foldLeft(init) {
        case ((acc: List[RowEntry]), (k: String, v: SerializableDebateStats)) =>
          acc :+ RowEntry(
            name = k,
            wins = v.wins,
            losses = v.losses,
            customRewardValue = v.averageReward
          )
      }
    }

    def sendRequest = {
      import LeaderboardCategories._
      val f = for {
        f <- AsyncCallback.fromFuture(loadLeaderboard())
        safeLeaderboardGenerator =
          (key: LeaderboardCategory) =>
            leaderboardToRows(f.data.getOrElse(key, Map()))
        result = ParsedResponse(
          judge = safeLeaderboardGenerator(Judge),
          honest = safeLeaderboardGenerator(HonestDebater),
          dishonest = safeLeaderboardGenerator(DishonestDebater)
        )
      } yield result

      OrWrapped.wrapped(f)
    }

    C.make(request = (), sendRequest = _ => sendRequest)(render)

  }
}
