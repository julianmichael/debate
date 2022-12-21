package debate
import debate.util._

// import org.scalajs.dom

import scalajs.js

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
// import japgolly.scalajs.react.extra.StateSnapshot
// // import japgolly.scalajs.react.MonocleReact._

// import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle.function.{all => Optics}

import cats.implicits._
import jjm.ling.ESpan
import jjm.ui.Rgba
import jjm.ling.Span

import jjm.implicits._
import cats.data.NonEmptyList
import jjm.ling.ISpan

object StoryPanel {

//   import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  // TODO move all the coloring decisions into one place
  val midHighlightColor = Rgba(255, 128, 0, 0.8)

  /** Render a list of tokens into HTML, highlighting subspans of that list with
    * various colors.
    *
    * @param tokens
    * @param highlights
    * @param getRenderer
    * @return
    *   a VdomArray of token spans
    */
  def renderHighlightedTokens(
      tokens: Vector[String],
      highlights: Vector[(Span, Rgba)],
      getRenderer: Int => (VdomTag => VdomTag) = Map()
  ) = {
    case class ColorLayering(rgba: Rgba, add: Boolean) // false == remove

    val colorLayerings = highlights
      .flatMap { case (span, color) =>
        Vector(
          span.begin -> ColorLayering(color, true),
          span.endExclusive -> ColorLayering(color, false)
        )
      }
      .sortBy(_._1) :+ (tokens.length -> ColorLayering(
      Rgba.transparent,
      true
    )) // final fake color stack to get it to render out the full content
    case class Acc(
        remainingTokens: Vector[String],
        curOffset: Int,
        colorStack: Vector[Rgba],
        spans: Vector[VdomTag]
    )
    colorLayerings
      .foldLeft(Acc(tokens, 0, Vector(), Vector())) {
        case (
              Acc(text, offset, colors, spans),
              (index, ColorLayering(color, shouldAdd))
            ) =>
          val nextColorSet =
            if (shouldAdd) colors :+ color
            else colors.remove(colors.indexOf(color))
          require(index >= offset) // should never fail bc of sorting property
          if (index == offset) {
            Acc(text, offset, nextColorSet, spans)
          } else {
            val nextIndex = index - offset
            val colorStr = NonEmptyList(Rgba.transparent, colors.toList)
              .reduce((x: Rgba, y: Rgba) => x add y)
              .toColorStyleString
            val endingColorStr =
              if (shouldAdd) colorStr
              else {
                NonEmptyList(Rgba.transparent, nextColorSet.toList)
                  .reduce((x: Rgba, y: Rgba) => x add y)
                  .toColorStyleString
              }
            Acc(
              text.drop(nextIndex),
              index,
              nextColorSet,
              spans ++ text.take(nextIndex).zipWithIndex.flatMap {
                case (token, i) =>
                  val nextSpaceColorStr =
                    if (i == nextIndex - 1) endingColorStr else colorStr
                  if (token == "\n")
                    Vector(<.br(^.key := s"word-${i + offset}"))
                  else
                    Vector[Option[VdomTag]](
                      Option(
                        getRenderer(i + offset)(
                          <.span(
                            ^.key := s"word-${i + offset}",
                            ^.style := js.Dynamic
                              .literal("backgroundColor" -> colorStr),
                            token
                          )
                        )
                      ),
                      Option(
                        <.span(
                          ^.key := s"space-${i + offset}",
                          ^.style := js.Dynamic.literal(
                            "backgroundColor" -> nextSpaceColorStr
                          ),
                          " "
                        )
                      ).filter(_ => i + offset != tokens.size - 1)
                    ).flatten[VdomTag]
              }
            )
          }
      }
      .spans
      .toVdomArray
  }

  case class Props(
    tokens: Vector[String],
    highlights: Vector[(ESpan, Rgba)],
    addSpan: ESpan => Callback
  )

  val minimumSegmentLength = 100

  val Component = ScalaComponent
    .builder[Props]("Story Panel")
    .render { $ =>
        val emptySeg = Vector[(String, Int)]()
        val initSegs = NonEmptyList.of(emptySeg)
        val segments = $.props.tokens.zipWithIndex.foldLeft(initSegs) {
            case (NonEmptyList(headSeg, tailSegs), token) =>
                if (token._1 == "\n" && headSeg.length > minimumSegmentLength) {
                    NonEmptyList(emptySeg, headSeg :: tailSegs)
                } else NonEmptyList(headSeg :+ token, tailSegs)
        }.reverse
        def intersectSpans(span1: Span, span2: Span): ESpan = {
            ESpan(
                math.max(span1.begin, span2.begin),
                math.min(span1.endExclusive, span2.endExclusive)
            )
        }
        val segmentsWithHighlights = segments.map { seg =>
            val segSpan = ISpan(seg.head._2, seg.last._2)
            seg -> $.props.highlights
                .filter(_._1.overlaps(segSpan))
                .map(Optics.first[(Span, Rgba), Span].modify(intersectSpans(segSpan, _)))
                .map(Optics.first[(Span, Rgba), Span].modify(_.translate(-segSpan.begin)))
        }
        <.div(S.debateSubpanel)(
            <.div(S.sourceMaterialSubpanel)(
                segmentsWithHighlights.toList.toVdomArray { case (segment, highlights) =>
                    SpanSelection2.make(
                        true,
                        ispan => $.props.addSpan(ispan.toExclusive.translate(segment.head._2))
                    ) { case (status, context) =>
                        val selectingSpanColorOpt =
                            SpanSelection2.Status.selecting.getOption(status).map {
                            case SpanSelection2.Selecting(begin, end) =>
                                ISpan(begin, end) -> midHighlightColor
                            }
                        val allHighlights = highlights ++ selectingSpanColorOpt

                        <.div(
                            renderHighlightedTokens(
                                segment.map(_._1),
                                allHighlights,
                                segment.indices
                                    .map(i =>
                                    i -> ((el: VdomTag) =>
                                        el(
                                        ^.onMouseMove --> context.hover(i),
                                        ^.onClick --> context.touch(i)
                                        ))
                                    )
                                    .toMap
                            ),
                            <.br()
                        )
                    }
                }
            )
        )
    }
    .build
}
