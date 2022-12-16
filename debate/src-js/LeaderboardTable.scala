package debate

import scala.language.existentials

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scala.concurrent.Future
import Helpers.ClassSetInterpolator

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

  case class RowEntry(name: String, wins: Int, losses: Int)
  case class State(
      judge: List[RowEntry],
      honest: List[RowEntry],
      dishonest: List[RowEntry]
  )

  def sortTableBy[T, B](
      ref: StateSnapshot[State],
      getter: State => List[T],
      maker: (State, List[T]) => State,
      by: T => B
  )(implicit ordering: Ordering[B]) = {
    <.div()(
      <.i(c"bi bi-caret-down-fill")(
        ^.onClick --> {
          val newList = getter(ref.value).sortBy(by)
          val newValue = maker(ref.value, newList)
          ref.setState(newValue)
        }
      ),
      // TODO add average score
      <.i(c"bi bi-caret-up-fill")(
        ^.onClick --> {
          implicit val reverseOrdering: Ordering[B] =
            ordering.reverse
          val newList = getter(ref.value).sortBy(by)(reverseOrdering)
          val newValue = maker(ref.value, newList)
          ref.setState(newValue)
        }
      )
    )
  }

  def make() = {
    val Cls = new MountingWithLocalState[State]

    def renderRowEntries(
        ref: StateSnapshot[
          State
        ], // used to make callbacks for sorting,
        getter: State => List[RowEntry],
        maker: (State, List[RowEntry]) => State
    ) = {
      def ourSort[B](
          by: RowEntry => B
      )(implicit ordering: Ordering[B]) =
        sortTableBy(ref, getter, maker, by)

      <.table(c"table table-striped")(
        <.thead(
          <.tr(
            <.th(
              "Name",
              ourSort(_.name)
            ),
            <.th(
              "Wins",
              ourSort(_.wins)
            ),
            <.th(
              "Losses",
              ourSort(_.losses)
            ),
            // TODO someday unify this code with the sortBy bits and the rows
            <.th(
              "Win %",
              ourSort(x => { x.wins.toDouble / (x.losses + x.wins) })
            )
          )
        ),
        <.tbody(
          getter(ref.value)
            .toVdomArray { case rowEntry =>
              <.tr(
                <.td(rowEntry.name),
                <.td(rowEntry.wins),
                <.td(rowEntry.losses),
                <.td(
                  f"${rowEntry.wins.toDouble / (rowEntry.losses + rowEntry.wins) * 100}%.1f"
                )
              )
            }
        )
      )
    }

    def render(s: StateSnapshot[State]) = {
      <.div(
        <.h3("Judge"),
        renderRowEntries(
          ref = s,
          getter = _.judge,
          maker = (x, y) => x.copy(judge = y)
        ),
        <.h3("Honest"),
        renderRowEntries(
          ref = s,
          getter = _.honest,
          maker = (x, y) => x.copy(honest = y)
        ),
        <.h3("Dishonest"),
        renderRowEntries(
          ref = s,
          getter = _.dishonest,
          maker = (x, y) => x.copy(dishonest = y)
        )
      )
    }

    def leaderboardToRows(
        x: LeaderboardForRoleType
    ) = {
      val init: List[RowEntry] = List()
      x.perProfile.foldLeft(init) {
        case ((acc: List[RowEntry]), (k: String, v: List[Double])) =>
          val wins = v.filter(_ > 0.5).length
          val losses = v.filter(_ < 0.5).length
          acc :+ RowEntry(k, wins, losses)
      }
    }

    def onMount: AsyncCallback[State] = {
      (for {
        f <- AsyncCallback.fromFuture(loadLeaderboard())
      } yield State(
        judge = leaderboardToRows(f.judge),
        honest = leaderboardToRows(f.honest),
        dishonest = leaderboardToRows(f.dishonest)
      ))
    }

    Cls.make(
      initialValue = State(judge = List(), honest = List(), dishonest = List()),
      render = render,
      onMount = onMount,
      shouldRefresh = _ => true
    )
  }
}
