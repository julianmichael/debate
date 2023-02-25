package debate
package view.debate
import cats.Id

import cats.data.NonEmptyChain
import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import jjm.DotMap
import jjm.DotPair
import japgolly.scalajs.react.feature.ReactFragment

import debate.Utils.ClassSetInterpolator

import scalacss.ScalaCssReact._
import japgolly.scalajs.react.Callback
import cats.data.Validated

import monocle.function.{all => Optics}

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
          ^.key := s"likert-$i",
          <.div(c"mx-auto p-1 text-center")(i + 1),
          <.input(c"p-1")(^.key := s"$i")(
            ^.`type`  := "radio",
            ^.checked := answer.value == i,
            ^.onChange --> answer.setState(i)
          )
        )
      ),
      <.div(minMaxLikertLabelStyle)(maxLabel)
    )
  }

  val DebaterGuessSelect = V.OptionalSelect[String](x => x, "(no guess)")

  def renderQuestion[Answer](
    profiles: Set[String],
    assignedRoles: Set[LiveDebateRole],
    role: Role,
    answerOpt: StateSnapshot[Option[Answer]],
    question: Question[Answer]
  ) = {
    val questionSpanOpt = question
      .questionText(role)
      .map(q =>
        if (!question.required)
          <.span(<.span(c"text-muted")("(Optional) "), q)
        else
          <.span(q)
      )
    val needsToBeDone = question.required && answerOpt.value.isEmpty
    questionSpanOpt.map(questionSpan =>
      <.div(c"card", S.attentionBackground.when(needsToBeDone))(
        <.div(c"card-body")(
          <.p(c"card-text")(questionSpan),
          question.questionDetails.whenDefined(details => <.p(c"card-text small")(details)),
          question match {
            case Question.ComparativeLikert(_, _, _, _, numOptions, minLabel, maxLabel, _) =>
              val judgment =
                answerOpt
                  .zoomState[ComparativeJudgment](_.getOrElse(ComparativeJudgment(-1, -1)))(j =>
                    _ => Some(j)
                  )
              <.div(
                <.div(S.comparativeLikertRow, c"mb-2")(
                  <.div(S.comparativeLikertLabel)(
                    role match {
                      case Debater(_) =>
                        "You: "
                      case Judge | OfflineJudge =>
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
                      case Judge | OfflineJudge =>
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
            case Question.Likert(_, _, _, _, numOptions, minLabel, maxLabel, _) =>
              val judgment = answerOpt.zoomState[Int](_.getOrElse(-1))(j => _ => Some(j))
              likertScale(numOptions, minLabel, maxLabel, judgment, bigger = true)
            case Question.FreeText(_, _, _, _, _) =>
              V.LiveTextArea
                .String(
                  answerOpt
                    .zoomState[String](_.getOrElse(""))(str => _ => Option(str).filter(_.nonEmpty))
                )
            case Question.RoleSelect(_, _, _, _, _) =>
              <.div(
                assignedRoles
                  .filter(_ != role)
                  .toVdomArray(role =>
                    <.div(^.key := role.toString)(
                      <.div(S.comparativeLikertRow, c"mb-2")(
                        <.div(S.comparativeLikertLabel)(role.toString),
                        <.div(S.grow)(
                          DebaterGuessSelect.apply(
                            profiles,
                            answerOpt
                              .zoomState[Map[LiveDebateRole, String]](_.getOrElse(Map()))(m =>
                                _ => Option(m).filter(_.nonEmpty)
                              )
                              .zoomStateL(Optics.at(role))
                          )
                        )
                      )
                    )
                  )
              )

          }
        )
      )
    )
  }

  def apply(
    profiles: Set[String],
    assignedRoles: Set[LiveDebateRole],
    role: Role,
    uploadedResponseOpt: Option[SurveyResponse],
    surveyAnswers: StateSnapshot[DotMap[Option, Key]],
    submit: SurveyResponse => Callback
  ) = {

    val responseEither: Either[Either[String, NonEmptyChain[Feedback.Key]], SurveyResponse] = {
      val answersVal = surveyAnswers
        .value
        .iterator
        .toVector
        .traverse { pair =>
          val answerOpt =
            pair.fst match {
              case Key.ComparativeLikert(_) =>
                pair
                  .snd
                  .filter(_.asInstanceOf[ComparativeJudgment].isValid)
                  .map(DotPair[Id](pair.fst)(_))
              case Key.Likert(_) =>
                pair.snd.filter(_.asInstanceOf[Int] > -1).map(DotPair[Id](pair.fst)(_))
              case Key.FreeText(_) =>
                pair.snd.map(DotPair[Id](pair.fst)(_))
              case Key.RoleSelect(_) =>
                pair.snd.map(DotPair[Id](pair.fst)(_))
            }

          if (answerOpt.nonEmpty)
            Validated.Valid(answerOpt)
          else {
            if (Feedback.questions.get(pair.fst).exists(_.required)) {
              Validated.Invalid(NonEmptyChain(pair.fst))
            } else
              Validated.Valid(None)
          }
        }
        .map(_.flatten)
        .map(pairs => DotMap[Id, Key](pairs: _*))

      val respEither =
        role match {
          case Debater(_) =>
            answersVal.map(SurveyResponse.Debater(_)).toEither.leftMap(Right(_))
          case Judge =>
            answersVal.map(SurveyResponse.Judge(_)).toEither.leftMap(Right(_))
          case OfflineJudge =>
            answersVal.map(SurveyResponse.OfflineJudge(_)).toEither.leftMap(Right(_))
          case _ =>
            Left(Left("Must be a debater or a judge to submit feedback"))
        }
      respEither
    }
    val responseOpt = responseEither.toOption

    ReactFragment(
      <.div(S.feedbackSurveySubpanel)(
        ReactFragment(
          Feedback
            .survey
            .flatMap { keyedQuestion =>
              val key: Key = keyedQuestion.fst
              val question = keyedQuestion.snd
              surveyAnswers
                .zoomStateOption[Option[key.Out]](_.get(key))(v => map => map.put(key)(v))
                .map { answerOpt =>
                  <.div(^.key := key.key)(
                    renderQuestion[key.Out](
                      profiles,
                      assignedRoles,
                      role,
                      answerOpt,
                      question.asInstanceOf[Question[key.Answer]]
                    )
                  )
                }
            }
            .toVdomArray
        )
      ),
      <.button(
        S.bottomOfDivButton,
        if (responseOpt.exists(r => uploadedResponseOpt.exists(_ == r))) {
          c"btn-success"
        } else
          c"btn-primary"
      )(
        responseEither match {
          case Left(Left(msg)) =>
            msg
          case Left(Right(_)) =>
            "Answer all required questions to submit"
          case Right(response) =>
            uploadedResponseOpt match {
              case Some(uploadedResponse) =>
                if (uploadedResponse == response) {
                  "Submitted"
                } else {
                  "Update"
                }
              case None =>
                "Submit"
            }
        },
        ^.disabled := responseOpt == uploadedResponseOpt,
        responseOpt.whenDefined(resp => ^.onClick --> submit(resp))
      )
    )
  }
}
