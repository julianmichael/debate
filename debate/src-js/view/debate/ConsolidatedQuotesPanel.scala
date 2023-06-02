package debate
package view.debate

import cats.data.NonEmptyList
import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.ling.ESpan
import jjm.ling.Span
import jjm.ui.Rgba

object ConsolidatedQuotesPanel {

//   import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  // TODO move all the coloring decisions into one place
  val midHighlightColor = Rgba(255, 128, 0, 0.8)

  def apply(tokens: Vector[String], highlights: Vector[(ESpan, Rgba)]) = Component(
    Props(tokens, highlights)
  )

  case class Props(tokens: Vector[String], highlights: Vector[(ESpan, Rgba)])
  object Props {
    implicit val propsReuse =
      Reusability.by[Props, Vector[(ESpan, Rgba)]](_.highlights)(Reusability.by_==)
  }

  def renderTokenHighlights(
    tokens: Vector[String],
    highlights: NonEmptyList[(ESpan, Rgba)]
  ): VdomArray = {
    val orderedHighlights = highlights.sortBy(_._1)
    case class GroupingState(
      completeGroups: List[
        Either[Boolean, NonEmptyList[(ESpan, Rgba)]]
      ], // Boolean represents whether there are paragraph breaks in between
      currentGroup: NonEmptyList[(ESpan, Rgba)]
    )
    val groupingState =
      orderedHighlights
        .tail
        .foldLeft(GroupingState(Nil, NonEmptyList.of(orderedHighlights.head))) {
          case (GroupingState(groups, curGroup), (span, color)) =>
            // include cases where it doesn't overlap but together they fully cover the text
            if (curGroup.exists(_._1.overlaps(span - 1)) || curGroup.exists(_._1.overlaps(span))) {
              GroupingState(groups, (span -> color) :: curGroup)
            } else {
              val hasParagraphBreak = tokens
                .slice(curGroup.map(_._1.end).maximum, span.begin)
                .contains("\n")
              val groupsToAdd = List(Left(hasParagraphBreak), Right(curGroup))
              GroupingState(groupsToAdd ++ groups, NonEmptyList.of(span -> color))
            }
        }
    val contigSpanLists = NonEmptyList(
      Right(groupingState.currentGroup),
      groupingState.completeGroups
    )
    val answerHighlighties =
      contigSpanLists
        .reverse
        .map {
          case Left(hasParagraphBreak) =>
            if (hasParagraphBreak)
              List(<.br(), <.span("..."), <.br())
            else
              List(<.span(" ... "))
          case Right(spanList) =>
            val (groupTokens, groupSpans) = V
              .Spans
              .cropPassageAndSpans(tokens, spanList, Optics.first[(Span, Rgba), Span])
            List(
              <.span(V.Spans.renderHighlightedTokens(groupTokens, groupSpans.toList)),
              <.span(S.quoteCitation)(
                s" (${spanList.map(_._1.begin).minimum}–${spanList.map(_._1.end).maximum})"
              )
            )
        }
        .combineAll
    answerHighlighties
      .zipWithIndex
      .toVdomArray { case (a, i) =>
        a(^.key := s"answerString-$i")
      }
  }

  val Component =
    ScalaComponent
      .builder[Props]("Consolidated Quotes Panel")
      .render { $ =>
        <.div(S.sourceMaterialSubpanel)(
          $.props
            .highlights
            .toList
            .toNel
            .map(highlights => renderTokenHighlights($.props.tokens, highlights))
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
