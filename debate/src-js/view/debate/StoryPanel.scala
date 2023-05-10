package debate
package view.debate

import cats.data.NonEmptyList

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.ling.ESpan
import jjm.ling.ISpan
import jjm.ling.Span
import jjm.ui.Rgba

import debate.util._

object StoryPanel {

//   import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  // TODO move all the coloring decisions into one place
  val midHighlightColor = Rgba(255, 128, 0, 0.8)

  def apply(tokens: Vector[String], highlights: Vector[(ESpan, Rgba)], addSpan: ESpan => Callback) =
    Component(Props(tokens, highlights, addSpan))

  case class Props(
    tokens: Vector[String],
    highlights: Vector[(ESpan, Rgba)],
    addSpan: ESpan => Callback
  )
  object Props {
    implicit val propsReuse =
      Reusability.by[Props, Vector[(ESpan, Rgba)]](_.highlights)(Reusability.by_==)
  }

  val minimumSegmentLength = 100

  val Component =
    ScalaComponent
      .builder[Props]("Story Panel")
      .render { $ =>
        val emptySeg = Vector[(String, Int)]()
        val initSegs = NonEmptyList.of(emptySeg)
        val segments =
          $.props
            .tokens
            .zipWithIndex
            .foldLeft(initSegs) { case (NonEmptyList(headSeg, tailSegs), token) =>
              if (token._1 == "\n" && headSeg.length > minimumSegmentLength) {
                NonEmptyList(emptySeg, headSeg :: tailSegs)
              } else
                NonEmptyList(headSeg :+ token, tailSegs)
            }
            .reverse
        def intersectSpans(span1: Span, span2: Span): ESpan = ESpan(
          math.max(span1.begin, span2.begin),
          math.min(span1.endExclusive, span2.endExclusive)
        )
        val segmentsWithHighlights = segments.map { seg =>
          val segSpan = ISpan(seg.head._2, seg.last._2)
          seg ->
            $.props
              .highlights
              .filter(_._1.overlaps(segSpan))
              .map(Optics.first[(Span, Rgba), Span].modify(intersectSpans(segSpan, _)))
              .map(Optics.first[(Span, Rgba), Span].modify(_.translate(-segSpan.begin)))
        }
        <.div(S.sourceMaterialSubpanel)(
          segmentsWithHighlights
            .toList
            .map { case (segment, highlights) =>
              val offset = segment.head._2
              TagMod(
                SpanSelection2
                  .make(true, ispan => $.props.addSpan(ispan.toExclusive.translate(offset))) {
                    case (status, context) =>
                      val selectingSpanColorOpt = SpanSelection2
                        .Status
                        .selecting
                        .getOption(status)
                        .map { case SpanSelection2.Selecting(begin, end) =>
                          ISpan(begin, end) -> midHighlightColor
                        }
                      val allHighlights = highlights ++ selectingSpanColorOpt

                      <.div(
                        V.Spans
                          .renderHighlightedTokens(
                            segment.map(_._1),
                            allHighlights.toList,
                            segment
                              .indices
                              .map(i =>
                                i ->
                                  ((el: VdomTag) =>
                                    if (segment(i)._1 == "\n") {
                                      <.br()
                                    } else
                                      el(
                                        ^.onMouseMove --> context.hover(i),
                                        ^.onClick --> context.touch(i)
                                      )
                                  )
                              )
                              .toMap
                          ),
                        <.br()
                      )
                  }
              )
            }: _*
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
