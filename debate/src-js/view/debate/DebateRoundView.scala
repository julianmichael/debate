package debate
package view.debate

// import japgolly.scalajs.react._
import java.time.Instant
import java.time.ZoneId

import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import jjm.ling.ESpan

object DebateRoundView {
  // import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  // import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def breakNewlines(x: String) = x
    .split("\n")
    .toVector
    .map(seg => Vector[VdomTag](<.span(seg)))
    .intercalate(Vector(<.br()))
    .zipWithIndex
    .toVdomArray { case (el, i) =>
      el(^.key := s"text-$i")
    }

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

  def speechHeaderHTML(
    role: Role,
    speech: DebateSpeech,
    startTimeOpt: Option[Long],
    userRoleOpt: Option[Role]
  ) = {
    val roleString = role.toString
    <.div(S.speechHeader)(
      speech.speaker,
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
            speechHeaderHTML(Debater(debaterIndex), speech, startTimeOpt, userRoleOpt),
            speech
              .content
              .map {
                case SpeechSegment.Text(text) =>
                  <.span(breakNewlines(text))
                case SpeechSegment.Quote(span) =>
                  quoteToHTML(source, span)
              }
              .zipWithIndex
              .toVdomArray { case (el, i) =>
                el(^.key := s"text-$i")
              }
          )
        }
    )

  def makeSpeechHtml(
    source: Vector[String],
    role: Role,
    speech: DebateSpeech,
    startTimeOpt: Option[Long],
    userRoleOpt: Option[Role],
    style: TagMod
    // speechIndex: Int
  ) =
    <.div(S.speechBox, style)(
      speechHeaderHTML(role, speech, startTimeOpt, userRoleOpt),
      speech
        .content
        .map {
          case SpeechSegment.Text(text) =>
            <.span(breakNewlines(text))
          case SpeechSegment.Quote(span) =>
            quoteToHTML(source, span)
        }
        .zipWithIndex
        .toVdomArray { case (el, i) =>
          el(^.key := s"text-$i")
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
                  makeSpeechHtml(
                    source,
                    Debater(index),
                    speech,
                    debateStartTime,
                    roleOpt,
                    speechStyle
                  )
                }
            }
            .flatten
            .toVector
            .zipWithIndex
            .toVdomArray { case (el, i) =>
              el(^.key := s"text-$i")
            }
        } else {
          Vector(makeSimultaneousSpeechesHtml(source, speeches, debateStartTime, roleOpt))
            .zipWithIndex
            .toVdomArray { case (el, i) =>
              el(^.key := s"text-$i")
            }
        }
      case SequentialSpeeches(speeches) =>
        val speechesToShow =
          if (
            speeches.size < numDebaters &&
            !roleOpt.existsAs { case Facilitator | Debater(_) =>
              true
            }
          ) {
            Map[Int, DebateSpeech]()
          } else
            speeches

        speechesToShow
          .toVector
          .sortBy(_._1)
          .toVdomArray { case (debaterIndex, speech) =>
            val speechStyle = TagMod(S.answerBg(debaterIndex), S.debateWidthOffset(debaterIndex))
            // val speechStyle =
            //   speech.speaker.role match {
            //     case Facilitator =>
            //       TagMod(S.facilitatorBg)
            //     case Observer =>
            //       TagMod(S.observerBg)
            //     case Judge =>
            //       TagMod(S.judgeFeedbackBg)
            //     case Debater(index) =>
            //       TagMod(S.answerBg(index), S.debateWidthOffset(index))
            //   }
            makeSpeechHtml(
              source,
              Debater(debaterIndex),
              speech,
              debateStartTime,
              roleOpt,
              speechStyle
            )(^.key := s"speech-$debaterIndex")
          }
      case JudgeFeedback(probabilities, speech, endsDebate) =>
        val speechStyle = TagMod(S.judgeFeedbackBg, S.judgeDecision.when(endsDebate))
        Vector(
          Option(makeSpeechHtml(source, Judge, speech, debateStartTime, roleOpt, speechStyle)),
          Option(
            <.div(
              ^.display       := "flex",
              ^.flexDirection := "row",
              probabilities
                .zipWithIndex
                .toVdomArray { case (prob, index) =>
                  val pct = f"${prob * 100.0}%.0f%%"
                  <.div(
                    ^.key := s"prob-$index",
                    S.answerBg(index),
                    ^.width      := pct,
                    ^.color      := "white",
                    ^.fontWeight := "bold",
                    ^.flexGrow   := "1"
                  )(pct)
                }
            )
          ).filter(_ => probabilities.size > 1)
        ).flatten
          .zipWithIndex
          .toVdomArray { case (speechBox, index) =>
            speechBox(^.key := s"speechbox-$index")
          }
      case NegotiateEnd(votes) =>
        if (votes.size < numDebaters) {
          roleOpt
            .collect { case Debater(index) =>
              votes
                .get(index)
                .map { votedToEnd =>
                  if (votedToEnd) {
                    <.div("You voted to ", <.strong("end"), " the debate.")
                  } else {
                    <.div("You voted to ", <.strong("continue"), " the debate.")
                  }
                }
            }
            .flatten
            .toVector
            .zipWithIndex
            .toVdomArray { case (el, i) =>
              el(^.key := s"text-$i")
            }
        } else {
          if (
            roleOpt.existsAs { case Debater(_) | Facilitator =>
              true
            }
          ) {
            if (votes.values.forall(identity)) {
              <.div("All debaters voted to ", <.strong("end"), " the debate.")
            } else if (votes.values.forall(!_)) {
              <.div("All debaters voted to ", <.strong("continue"), " the debate.")
            } else {
              <.div(
                "Continue votes: ",
                Utils
                  .delimitedSpans(votes.filter(!_._2).keySet.toVector.sorted.map(answerLetter))
                  .toVdomArray,
                ". ",
                "End votes: ",
                Utils
                  .delimitedSpans(votes.filter(_._2).keySet.toVector.sorted.map(answerLetter))
                  .toVdomArray
              )
            }
          } else {
            if (votes.values.forall(identity)) {
              <.div("All debaters have mutually agreed to end the debate.")
            } else {
              <.div("Debaters did not agree to end the debate.")
            }
          }
        }
    }
  )
}
