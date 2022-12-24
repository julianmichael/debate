package debate

// import japgolly.scalajs.react._
import java.time.Instant
import java.time.ZoneId

import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import jjm.ling.ESpan

object DebateRoundView {
  // import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  // import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def makeSimpleVdomFromText(text: String) = <.span(
    text
      .split("\n")
      .toVector
      .map(x => Vector(<.span(x): VdomElement))
      .intercalate(Vector(<.br()))
      .toVdomArray
  )

  def breakNewlines(x: String) =
    x.split("\n")
      .toVector
      .map(seg => Vector[VdomNode](<.span(seg)))
      .intercalate(Vector(<.br()))
      .toVdomArray

  def minSecTime(millis: Long): String = {
    val secs    = millis / 1000
    val mins    = secs / 60
    val secsRem = secs % 60
    s"${mins}m ${secsRem}s"
  }

  def timestampHTML(startTime: Long, timestamp: Long) = {
    val relTime = timestamp - startTime
    val humanReadableTimeUTC =
      Instant
        .ofEpochMilli(timestamp)
        // TODO this should perhaps display it in the client's timezone
        .atZone(ZoneId.of("Z")) // see "time zones" on http://cquiroz.github.io/scala-java-time/
        .toLocalTime.toString
    <.span(S.speechTimestamp)(
      TagMod(minSecTime(relTime), " into the debate at ").when(relTime > 0),
      humanReadableTimeUTC + " UTC"
    )
  }

  def speechToHTML(speech: DebateSpeech, startTimeOpt: Option[Long], userRoleOpt: Option[Role]) = {
    val roleString = speech.speaker.role.toString
    <.div(S.speechHeader)(
      speech.speaker.name,
      s" ($roleString) ",
      startTimeOpt.whenDefined(startTime =>
        timestampHTML(startTime, speech.timestamp).when(
          userRoleOpt
            .collect { case Facilitator | Debater(_) =>
              ()
            }
            .nonEmpty
        )
      )
    )
  }

  def quoteToHTML(source: Vector[String], span: ESpan) = <.span(
    <.span(S.quoteText)(breakNewlines(Utils.renderSpan(source, span))),
    <.span(S.quoteCitation)(s" (${span.begin}–${span.end})")
  )

  def makeSimultaneousSpeechesHtml(
    source: Vector[String],
    speeches: Map[Int, DebateSpeech],
    startTimeOpt: Option[Long],
    userRoleOpt: Option[Role]
  ) =
    <.div(S.speechRow)(
      speeches
        .toVector
        .sortBy(_._1)
        .toVdomArray { case (debaterIndex, speech) =>
          <.div(S.speechBox, S.answerBg(debaterIndex))(
            ^.key := s"speech-$debaterIndex",
            speechToHTML(speech, startTimeOpt, userRoleOpt),
            speech
              .content
              .toVdomArray {
                case SpeechSegment.Text(text) =>
                  makeSimpleVdomFromText(text)
                case SpeechSegment.Quote(span) =>
                  quoteToHTML(source, span)
              }
          )
        }
    )

  def makeSpeechHtml(
    source: Vector[String],
    speech: DebateSpeech,
    startTimeOpt: Option[Long],
    userRoleOpt: Option[Role],
    style: TagMod
    // speechIndex: Int
  ) =
    <.div(S.speechBox, style)(
      speechToHTML(speech, startTimeOpt, userRoleOpt),
      speech
        .content
        .toVdomArray {
          case SpeechSegment.Text(text) =>
            makeSimpleVdomFromText(text)
          case SpeechSegment.Quote(span) =>
            quoteToHTML(source, span)
        }
    )

  def makeRoundHtml(
    source: Vector[String],
    roleOpt: Option[Role],
    debateStartTime: Option[Long],
    numDebaters: Int,
    round: DebateRound
  ) = <.div(
    round
      .timestamp(numDebaters)
      .whenDefined(roundTime =>
        debateStartTime.whenDefined(startTime => timestampHTML(startTime, roundTime))
      )
      .when(
        roleOpt
          .collect { case Facilitator | Debater(_) =>
            ()
          }
          .isEmpty
      ),
    round match {
      case SimultaneousSpeeches(speeches) =>
        if (speeches.size < numDebaters) {
          roleOpt
            .collect { case Debater(index) =>
              speeches
                .get(index)
                .map { speech =>
                  val speechStyle = TagMod(
                    S.answerOutline(index),
                    S.pendingBg,
                    S.debateWidthOffset(index)
                  )
                  makeSpeechHtml(source, speech, debateStartTime, roleOpt, speechStyle)
                }
            }
            .flatten
            .toVector
            .toVdomArray
        } else {
          Vector(makeSimultaneousSpeechesHtml(source, speeches, debateStartTime, roleOpt))
            .toVdomArray
        }
      case SequentialSpeeches(speeches) =>
        val speechesToShow =
          if (speeches.size < numDebaters) {
            if (
              roleOpt
                .collect { case Facilitator | Debater(_) =>
                  ()
                }
                .nonEmpty
            ) {
              speeches.toVector.sortBy(_._1).map(_._2)
            } else
              Vector()
          } else
            speeches.values.toVector

        speechesToShow.toVdomArray { case speech =>
          val speechStyle =
            speech.speaker.role match {
              case Facilitator =>
                TagMod(S.facilitatorBg)
              case Observer =>
                TagMod(S.observerBg)
              case Judge =>
                TagMod(S.judgeFeedbackBg)
              case Debater(index) =>
                TagMod(S.answerBg(index), S.debateWidthOffset(index))
            }
          makeSpeechHtml(source, speech, debateStartTime, roleOpt, speechStyle)
        }
      case JudgeFeedback(probabilities, speech, endsDebate) =>
        val speechStyle =
          speech.speaker.role match {
            case Facilitator =>
              TagMod(S.facilitatorBg)
            case Observer =>
              TagMod(S.observerBg)
            case Judge =>
              TagMod(S.judgeFeedbackBg, S.judgeDecision.when(endsDebate))
            case Debater(index) =>
              TagMod(S.answerBg(index), S.debateWidthOffset(index))
          }
        Vector(
          Option(makeSpeechHtml(source, speech, debateStartTime, roleOpt, speechStyle)),
          Option(
            <.div(
              ^.display       := "flex",
              ^.flexDirection := "row",
              probabilities
                .zipWithIndex
                .toVdomArray { case (prob, index) =>
                  val pct = f"${prob * 100.0}%.0f%%"
                  <.div(
                    S.answerBg(index),
                    ^.width      := pct,
                    ^.color      := "white",
                    ^.fontWeight := "bold",
                    ^.flexGrow   := "1"
                  )(pct)
                }
            )
          ).filter(_ => probabilities.size > 1)
        ).flatten.toVdomArray
    }
  )
}
