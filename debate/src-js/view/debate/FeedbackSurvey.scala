package debate
package view.debate
import cats.Id

import japgolly.scalajs.react.vdom.html_<^._
import scala.annotation.nowarn
import japgolly.scalajs.react.extra.StateSnapshot
import debate.util.Local
import jjm.DotMap
import jjm.DotPair
import japgolly.scalajs.react.feature.ReactFragment

@nowarn("msg=[unused]") // TODO get rid of in a bit
object FeedbackSurvey {

  import Feedback._

  def renderQuestion[Answer](answerOpt: StateSnapshot[Option[Answer]], question: Question[Answer]) =
    <.div(
      question match {
        case Question.ComparativeLikert(
              debaterQuestionOpt,
              judgeQuestionOpt,
              questionDetailsOpt,
              numOptions,
              minDescription,
              maxDescription
            ) =>
          <.div()
        case Question.Likert(
              debaterQuestionOpt,
              judgeQuestionOpt,
              questionDetailsOpt,
              numOptions,
              minDescription,
              maxDescription
            ) =>
          <.div()
        case Question.FreeText(debaterQuestionOpt, judgeQuestionOpt, questionDetailsOpt) =>
          <.div()
      }
    )

  def apply(
    uploadedAnswers: Option[DotMap[Id, Key]] = None // TODO
  ) = {
    val workingAnswers: DotMap[Option, Key] = uploadedAnswers
      .map { answers =>
        DotMap(answers.iterator.toList.map(pair => DotPair[Option](pair.fst)(None)): _*)
      }
      .getOrElse(Feedback.initAnswers)
    <.div(
      Local[DotMap[Option, Key]].make(workingAnswers) { surveyAnswers =>
        ReactFragment(
          Feedback
            .survey
            .flatMap { keyedQuestion =>
              val key: Key = keyedQuestion.fst
              val question = keyedQuestion.snd
              surveyAnswers
                .zoomStateOption[Option[key.Out]](_.get(key))(v => map => map.put(key)(v))
                // .zoomStateO[key.Out](DotMap.dotMapIndex[Option, key.type, key.Out].index(key))
                .map { answerOpt =>
                  <.div(^.key := key.key)(
                    // TODO get rid of the cast..
                    renderQuestion[key.Out](answerOpt, question.asInstanceOf[Question[key.Answer]])
                  )
                }
            }
            .toVdomArray
        )
      }
    )
  }
}
