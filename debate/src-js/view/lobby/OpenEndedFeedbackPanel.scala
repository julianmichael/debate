package debate
package view.lobby

import cats.implicits._

import japgolly.scalajs.react.AsyncCallback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import org.scalajs.dom
import scalacss.ScalaCssReact._

import jjm.ui.Mounting

import debate.util.ListConfig
import debate.util.SyncedState
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.extra.StateSnapshot
import debate.util.Local

object OpenEndedFeedbackPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  val SyncedFeedback = SyncedState
    .forJsonString[OpenEndedFeedback, OpenEndedFeedback, OpenEndedFeedback](
      getRequestFromState = identity,
      getStateUpdateFromResponse = responseState => _ => responseState
    )

  val openEndedFeedbackUri: String =
    s"${Utils.wsProtocol}//${dom.document.location.host}/feedback-ws"

  import Utils.ClassSetInterpolator

//   import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  def editableText(text: StateSnapshot[String]) =
    Local[Option[String]].make(Option(text.value).filter(_.isEmpty)) { localTextOpt =>
      localTextOpt.value match {
        case None =>
          <.span(text.value, ^.onClick --> localTextOpt.setState(Some(text.value)))
        case Some(localText) =>
          V.TextField
            .String(
              StateSnapshot[String](localText)((xOpt: Option[String], cb) =>
                xOpt.foldMap(x => text.setState(x, localTextOpt.setState(None, cb)))
              )
            )
      }
    }

  def apply(userName: String) =
    SyncedFeedback.make(websocketURI = openEndedFeedbackUri) {
      case SyncedFeedback.Disconnected(reconnect, reason) =>
        Mounting.make(AsyncCallback.unit.delayMs(5000).completeWith(_ => reconnect))(
          <.div(S.loading)("""You've been disconnected. Will attempt to reconnect every 5 seconds.
                    If you don't reconnect after a few seconds,
                    Please refresh the page. """ + reason)
        )
      case SyncedFeedback.Connecting =>
        <.div(S.loading)("Connecting to feedback data service...")
      case SyncedFeedback.Connected(_, None) =>
        <.div(S.loading)("Waiting for feedback data...")
      case SyncedFeedback.Connected(_, Some(feedback)) =>
        ListConfig[OpenEndedFeedbackSection].nice(
          feedback.zoomStateL(OpenEndedFeedback.sections),
          OpenEndedFeedbackSection("", Vector()),
          0,
          hideDeleteButtons = true
        ) { case ListConfig.Context(section, _) =>
          ReactFragment(
            <.div(c"card-header")(editableText(section.zoomStateL(OpenEndedFeedbackSection.name))),
            <.div(c"card-body", ^.backgroundColor := "lightcyan")(
              <.div(c"mx-1 mt-1")(
                ListConfig[OpenEndedFeedbackQuestion].nice(
                  section.zoomStateL(OpenEndedFeedbackSection.questions),
                  OpenEndedFeedbackQuestion("", Vector()),
                  0,
                  hideDeleteButtons = true
                ) { case ListConfig.Context(question, _) =>
                  ReactFragment(
                    <.div(c"card-header")(
                      editableText(question.zoomStateL(OpenEndedFeedbackQuestion.question))
                    ),
                    <.div(c"mx-1 mt-1")(
                      ListConfig[OpenEndedFeedbackAnswer].nice(
                        question.zoomStateL(OpenEndedFeedbackQuestion.answers),
                        OpenEndedFeedbackAnswer("", Map()),
                        0,
                        hideDeleteButtons = true
                      ) { case ListConfig.Context(answer, _) =>
                        def makeVoteDiv(vote: Boolean) =
                          <.div(c"mt-1")(
                            <.button(c"mx-1")(
                              c"btn",
                              if (answer.value.responses.get(userName).exists(_ == vote))
                                if (vote)
                                  c"btn-success"
                                else
                                  c"btn-danger"
                              else if (vote)
                                c"btn-outline-success"
                              else
                                c"btn-outline-danger"
                            )(
                              if (vote)
                                <.i(c"bi bi-hand-thumbs-up")
                              else
                                <.i(c"bi bi-hand-thumbs-down"),
                              ^.onClick -->
                                answer.modState(
                                  OpenEndedFeedbackAnswer
                                    .responses
                                    .composeLens(Optics.at(userName))
                                    .modify {
                                      case Some(`vote`) =>
                                        None
                                      case _ =>
                                        Some(vote)
                                    }
                                )
                            ),
                            Utils
                              .delimitedSpans(
                                answer.value.responses.filter(_._2 == vote).keySet.toVector.sorted
                              )
                              .toVdomArray
                          )

                        <.div(c"pb-1")(
                          <.div(c"card-header")(
                            editableText(answer.zoomStateL(OpenEndedFeedbackAnswer.answer))
                          ),
                          makeVoteDiv(true),
                          makeVoteDiv(false)
                        )
                      }
                    )
                  )
                }
              )
            )
          )
        }

    }

}
