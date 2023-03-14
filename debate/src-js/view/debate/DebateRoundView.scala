package debate
package view.debate

// import japgolly.scalajs.react._
import java.time.Instant
import java.time.ZoneId

import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import jjm.ling.ESpan
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.feature.ReactFragment

object DebateRoundView {
  // import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  import Utils.ClassSetInterpolator
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
    userRole: Role,
    userName: String,
    anonymize: Boolean
  ) =
    <.div(S.speechHeader)(
      DebatePage.renderDebateParticipant(anonymize, userRole, userName, role, speech.speaker),
      " ",
      startTimeOpt.whenDefined(startTime =>
        timestampHTML(startTime, speech.timestamp).when(userRole.canSeeIntermediateArguments)
      )
    )

  def quoteToHTML(source: Vector[String], span: ESpan) = <.span(
    <.span(S.quoteText)(breakNewlines(Utils.renderSpan(source, span))),
    <.span(S.quoteCitation)(s" (${span.begin}–${span.end})")
  )

  def makeSimultaneousSpeechesHtml(
    source: Vector[String],
    speeches: Map[Int, DebateSpeech],
    startTimeOpt: Option[Long],
    userRole: Role,
    userName: String,
    anonymize: Boolean
  ) =
    <.div(S.speechRow)(
      speeches
        .toVector
        .sortBy(_._1)
        .toVdomArray { case (debaterIndex, speech) =>
          <.div(S.speechBox, S.answerBg(debaterIndex))(
            ^.key := s"speech-$debaterIndex",
            speechHeaderHTML(
              Debater(debaterIndex),
              speech,
              startTimeOpt,
              userRole,
              userName,
              anonymize
            ),
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
    userRole: Role,
    userName: String,
    anonymize: Boolean,
    style: TagMod
  ) =
    <.div(S.speechBox, style)(
      speechHeaderHTML(role, speech, startTimeOpt, userRole, userName, anonymize),
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
    userName: String,
    role: Role,
    anonymize: Boolean,
    debateStartTime: Option[Long],
    numDebaters: Int,
    numPreviousContinues: Int,
    getRewardForJudgment: (Int, Vector[Double]) => Option[Double],
    round: DebateRound,
    modifyRound: Option[DebateRound] => Callback
  ) = <.div(
    round
      .timestamp(numDebaters)
      .whenDefined(roundTime =>
        debateStartTime.whenDefined(startTime => timestampHTML(startTime, roundTime))
      )
      .when(role.seesRoundTimestamp),
    round match {
      case SimultaneousSpeeches(speeches) =>
        if (speeches.size < numDebaters) {
          speeches
            .toVector
            .filter { case (index, _) =>
              role.canSeeWhatDebaterSees(index)
            }
            .map { case (index, speech) =>
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
                role,
                userName,
                anonymize,
                speechStyle
              )
            }
            .zipWithIndex
            .toVdomArray { case (el, i) =>
              el(^.key := s"text-$i")
            }
        } else {
          Vector(
            makeSimultaneousSpeechesHtml(
              source,
              speeches,
              debateStartTime,
              role,
              userName,
              anonymize
            )
          ).zipWithIndex
            .toVdomArray { case (el, i) =>
              el(^.key := s"text-$i")
            }
        }
      case SequentialSpeeches(speeches) =>
        val speechesToShow =
          if (speeches.size < numDebaters && !role.canSeeIntermediateArguments) {
            Map[Int, DebateSpeech]()
          } else
            speeches

        speechesToShow
          .toVector
          .sortBy(_._1)
          .toVdomArray { case (debaterIndex, speech) =>
            val speechStyle = TagMod(S.answerBg(debaterIndex), S.debateWidthOffset(debaterIndex))
            makeSpeechHtml(
              source,
              Debater(debaterIndex),
              speech,
              debateStartTime,
              role,
              userName,
              anonymize,
              speechStyle
            )(^.key := s"speech-$debaterIndex")
          }
      case JudgeFeedback(probabilities, speech, endsDebate) =>
        val speechStyle = TagMod(S.judgeFeedbackBg, S.judgeDecision.when(endsDebate))
        Vector(
          Option(
            makeSpeechHtml(
              Vector(),
              Judge,
              speech,
              debateStartTime,
              role,
              userName,
              anonymize,
              speechStyle
            )
          ),
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
          ).filter(_ => probabilities.size > 1),
          Option {
            val continueOrEnd =
              if (endsDebate)
                "end"
              else
                "continue"
            <.div(s"The judge decided to ", <.strong(continueOrEnd), " the debate.")
          },
          getRewardForJudgment(numPreviousContinues, probabilities).map { reward =>
            <.div(f"Reward: $reward%.3f")
          }
        ).flatten
          .zipWithIndex
          .toVdomArray { case (speechBox, index) =>
            speechBox(^.key := s"speechbox-$index")
          }
      case NegotiateEnd(votes) =>
        if (votes.size == numDebaters || role == Facilitator) {
          if (role.canSeeVotes) {
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
              <.div("All debaters have mutually agreed to ", <.strong("end"), " the debate.")
            } else {
              <.div("Debaters did not agree to end the debate.")
            }
          }
        } else {
          Option(role)
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
        }
      case OfflineJudgments(judgments) =>
        val canSeeOfflineJudgingResults = !anonymize
        // role != OfflineJudge || judgments.get(userName).exists(_.result.nonEmpty)

        // TODO: display info about num continues and time taken to judge
        // TODO: display info about people currently judging? (maybe facilitator only)
        val speechStyle = TagMod(S.offlineJudgeBg)
        TagMod(
          judgments
            .toVector
            .sortBy(_._2.result.isEmpty)
            .filter { case (judge, _) =>
              judge == userName || canSeeOfflineJudgingResults
            }
            .flatMap { case (judge, OfflineJudgment(_, startTimeMillis, numContinues, resultOpt)) =>
              Vector(
                Option(
                  resultOpt
                    .map(result =>
                      makeSpeechHtml(
                        Vector(),
                        OfflineJudge,
                        DebateSpeech(
                          judge,
                          result.timestamp,
                          Vector(SpeechSegment.Text(result.explanation))
                        ),
                        debateStartTime,
                        role,
                        userName,
                        anonymize,
                        speechStyle
                      )
                    )
                    .getOrElse(
                      ReactFragment(
                        makeSpeechHtml(
                          Vector(),
                          OfflineJudge,
                          DebateSpeech(
                            judge,
                            startTimeMillis,
                            Vector(SpeechSegment.Text("<pending>"))
                          ),
                          debateStartTime,
                          role,
                          userName,
                          anonymize,
                          speechStyle
                        ),
                        <.div(
                          <.a(c"text-danger")("Click to delete the judgment above"),
                          ^.onClick --> modifyRound(None)
                        )
                      )
                    )
                ),
                resultOpt
                  .filter(_.distribution.size > 1)
                  .map(result =>
                    <.div(
                      ^.display       := "flex",
                      ^.flexDirection := "row",
                      result
                        .distribution
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
                  ),
                resultOpt
                  .map(_.distribution)
                  .flatMap(getRewardForJudgment(numContinues, _))
                  .map { reward =>
                    <.div(f"Reward: $reward%.3f")
                  }
              ).flatten
            }: _*
        )
    }
  )
}
