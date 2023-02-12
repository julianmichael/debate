package debate
package view.debate
import cats.Id

import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import debate.util.Local
import jjm.DotMap
import jjm.DotPair
import japgolly.scalajs.react.feature.ReactFragment

import debate.Utils.ClassSetInterpolator

import scalacss.ScalaCssReact._
import japgolly.scalajs.react.Callback

object FeedbackSurvey {

  val S = Styles
  val V = new jjm.ui.View(S)

  import Feedback._

  def likertScale(
    numOptions: Int,
    minLabel: String,
    maxLabel: String,
    answer: StateSnapshot[Int],
    bigger: Boolean = false
  ) = {
    val minMaxLikertLabelStyle =
      if (bigger)
        S.minMaxLikertLabelBigger
      else
        S.minMaxLikertLabel
    <.div(S.row, c"align-items-center justify-content-center")(
      <.div(c"text-right", minMaxLikertLabelStyle)(minLabel),
      (0 until numOptions).toVdomArray(i =>
        <.div(S.numberedLikertButton)(
          <.div(c"mx-auto p-1 text-center")(i + 1),
          <.input(c"p-1")(^.key := s"$i")(
            ^.`type`  := "radio",
            ^.checked := answer.value == i,
            ^.onClick --> answer.setState(i)
          )
        )
      ),
      <.div(minMaxLikertLabelStyle)(maxLabel)
    )
  }

  def renderQuestion[Answer](
    role: Role,
    answerOpt: StateSnapshot[Option[Answer]],
    question: Question[Answer]
  ) = {
    val questionTextOpt =
      role match {
        case Debater(_) =>
          question.debaterQuestion
        case Judge =>
          question.judgeQuestion
        case _ =>
          None
      }
    questionTextOpt.map(questionText =>
      <.div(c"card")(
        <.div(c"card-body")(
          <.p(c"card-text")(questionText),
          question.questionDetails.whenDefined(details => <.p(c"card-text small")(details)),
          question match {
            case Question.ComparativeLikert(_, _, _, numOptions, minLabel, maxLabel) =>
              val judgment =
                answerOpt
                  //   .asInstanceOf[StateSnapshot[Option[ComparativeJudgment]]]
                  .zoomState[ComparativeJudgment](_.getOrElse(ComparativeJudgment(-1, -1)))(j =>
                    _ => Some(j)
                  )
              <.div(
                <.div(S.comparativeLikertRow, c"mb-2")(
                  <.div(S.comparativeLikertLabel)(
                    role match {
                      case Debater(_) =>
                        "You: "
                      case Judge =>
                        "Debater A: "
                      case _ =>
                        "Error" // TODO
                    }
                  ),
                  <.div(S.grow)(
                    likertScale(
                      numOptions,
                      minLabel,
                      maxLabel,
                      judgment.zoomStateL(ComparativeJudgment.first)
                    )
                  )
                ),
                <.div(S.comparativeLikertRow)(
                  <.div(S.comparativeLikertLabel)(
                    role match {
                      case Debater(_) =>
                        "Opponent: "
                      case Judge =>
                        "Debater B: "
                      case _ =>
                        "Error" // TODO
                    }
                  ),
                  <.div(S.grow)(
                    likertScale(
                      numOptions,
                      minLabel,
                      maxLabel,
                      judgment.zoomStateL(ComparativeJudgment.second)
                    )
                  )
                )
              )
            case Question.Likert(_, _, _, numOptions, minLabel, maxLabel) =>
              val judgment =
                answerOpt
                  //   .asInstanceOf[StateSnapshot[Option[Int]]]
                  .zoomState[Int](_.getOrElse(-1))(j => _ => Some(j))
              likertScale(numOptions, minLabel, maxLabel, judgment, bigger = true)
            case Question.FreeText(_, _, _) =>
              V.LiveTextArea
                .String(
                  answerOpt
                    //   .asInstanceOf[StateSnapshot[Option[String]]]
                    .zoomState[String](_.getOrElse(""))(str => _ => Some(str))
                )
          }
        )
      )
    )
  }

  def apply(
    role: Role,
    uploadedAnswers: Option[DotMap[Id, Key]],
    submit: SurveyResponse => Callback
  ) = {
    val workingAnswers: DotMap[Option, Key] = uploadedAnswers
      .map { answers =>
        DotMap(answers.iterator.toList.map(pair => DotPair[Option](pair.fst)(Option(pair.snd))): _*)
      }
      .getOrElse(Feedback.initAnswers(role))
    <.div(S.feedbackSurveySubpanel)(
      Local[DotMap[Option, Key]].make(workingAnswers) { surveyAnswers =>
        val responseOpt = {
          val answersOpt = surveyAnswers
            .value
            .iterator
            .toVector
            .traverse { pair =>
              pair.fst match {
                case Key.ComparativeLikert(_) =>
                  pair
                    .snd
                    .filter(_.asInstanceOf[ComparativeJudgment].isValid)
                    .map(DotPair[Id](pair.fst)(_))
                case Key.Likert(_) =>
                  pair.snd.filter(_.asInstanceOf[Int] > -1).map(DotPair[Id](pair.fst)(_))
                case Key.FreeText(_) =>
                  Option(pair.snd.getOrElse("").asInstanceOf[pair.fst.Out])
                    .map(DotPair[Id](pair.fst)(_))
              }
            }
            .map(pairs => DotMap[Id, Key](pairs: _*))

          val respOpt = answersOpt.flatMap(answers =>
            role match {
              case Debater(_) =>
                Some(SurveyResponse.Debater(answers))
              case Judge =>
                Some(SurveyResponse.Judge(answers))
              case _ =>
                None
            }
          )
          respOpt
        }
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
                    renderQuestion[key.Out](
                      role,
                      answerOpt,
                      question.asInstanceOf[Question[key.Answer]]
                    )
                  )
                }
            }
            .toVdomArray,
          <.button(c"btn btn-primary btn-block")(
            "Submit",
            ^.disabled := responseOpt.isEmpty,
            responseOpt.whenDefined(resp => ^.onClick --> submit(resp))
          )
        )
      }
    )
  }
}
