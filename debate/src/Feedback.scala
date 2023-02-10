package debate

import jjm.DotEncoder
import jjm.DotDecoder
import cats.Id
import jjm.DotPair
import jjm.DotMap
import io.circe.generic.JsonCodec
import io.circe.KeyEncoder
import io.circe.KeyDecoder
import io.circe.Encoder
import io.circe.Decoder

object Feedback {

  @JsonCodec
  sealed trait SurveyResponse
  object SurveyResponse {
    case class Debater(answers: DotMap[Id, Key]) extends SurveyResponse
    case class Judge(answers: DotMap[Id, Key])   extends SurveyResponse
  }

  // self/opponent for debaters
  // A/B for judges
  @JsonCodec
  case class ComparativeJudgment(first: Int, second: Int)

  sealed trait Key {
    def key: String
    type Answer
    type Out = Answer
  }
  object Key {
    case class ComparativeLikert(key: String) extends Key {
      type Answer = ComparativeJudgment
    }
    case class Likert(key: String) extends Key {
      type Answer = Int
    }
    case class FreeText(key: String) extends Key {
      type Answer = String
    }
    // case class FreeTextWithOptOut(key: String) extends Key {
    //   type Answer = Option[String]
    // }

    lazy val allKeys: Set[Key]      = survey.view.map(_.fst).toSet[Key]
    implicit lazy val keyKeyEncoder = KeyEncoder.instance[Key](_.key)
    implicit lazy val keyKeyDecoder = KeyDecoder.instance[Key](key => allKeys.find(_.key == key))
    implicit lazy val keyDotEncoder =
      new DotEncoder[Key] {
        def apply(key: Key) = {
          val res =
            key match {
              case ComparativeLikert(_) =>
                implicitly[Encoder[ComparativeJudgment]]
              case Likert(_) =>
                implicitly[Encoder[Int]]
              case FreeText(_) =>
                implicitly[Encoder[String]]
            }
          res.asInstanceOf[Encoder[key.Out]]
        }
      }
    implicit lazy val keyDotDecoder =
      new DotDecoder[Key] {
        def apply(key: Key) = {
          val res =
            key match {
              case ComparativeLikert(_) =>
                implicitly[Decoder[ComparativeJudgment]]
              case Likert(_) =>
                implicitly[Decoder[Int]]
              case FreeText(_) =>
                implicitly[Decoder[String]]
            }
          res.asInstanceOf[Decoder[key.Out]]
        }
      }
  }

  sealed trait Question[Answer]
  object Question {

    @JsonCodec
    case class ComparativeLikert(
      debaterQuestion: Option[String] = None,
      judgeQuestion: Option[String] = None,
      questionDetails: Option[String] = None,
      numOptions: Int,
      minDescription: String,
      maxDescription: String
    ) extends Question[ComparativeJudgment]

    @JsonCodec
    case class Likert(
      debaterQuestion: Option[String] = None,
      judgeQuestion: Option[String] = None,
      questionDetails: Option[String] = None,
      numOptions: Int,
      minDescription: String,
      maxDescription: String
    ) extends Question[Int]

    @JsonCodec
    case class FreeText(
      debaterQuestion: Option[String] = None,
      judgeQuestion: Option[String] = None,
      questionDetails: Option[String] = None
    ) extends Question[String]
  }

  // TODO: question HTML? for bolding etc.
  val survey = List(
    DotPair[Question](Key.FreeText("reason for outcome"))(
      Question.FreeText(
        debaterQuestion = Some("Why do you think you won/lost this debate?"),
        judgeQuestion = Some(
          "How would you explain why you got the right/wrong answer in this debate?"
        )
      )
    ),
    DotPair[Question](Key.Likert("subjective correctness"))(
      Question.Likert(
        debaterQuestion = Some(
          "Knowing the full story, how disputable do you think the question is?"
        ),
        numOptions = 5,
        minDescription = "\"Correct\" answer is wrong",
        maxDescription = "\"Correct\" answer is right"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("evidence in story"))(
      Question.ComparativeLikert(
        debaterQuestion = Some(
          "How would you rate the quality of the evidence present in the story in favor of each side?"
        ),
        numOptions = 5,
        minDescription = "Hardly anything to latch onto",
        maxDescription = "Clear and convincing evidence"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("evidence use"))(
      Question.ComparativeLikert(
        debaterQuestion = Some(
          "How effectively did each of you use the evidence available to you?"
        ),
        numOptions = 5,
        minDescription = "Poorly / missed very critical evidence",
        maxDescription = "Made the best use of it"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("evidence in debate"))(
      Question.ComparativeLikert(
        judgeQuestion = Some("How compelling was each debater's evidence?"),
        numOptions = 5,
        minDescription = "Poor / irrelevant",
        maxDescription = "Strong & clearly supported their argument"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("factual accuracy"))(
      Question.ComparativeLikert(
        debaterQuestion = Some(
          "How accurately did each of you represent the facts of the story in your arguments?"
        ),
        numOptions = 5,
        minDescription = "Completely inaccurately",
        maxDescription = "Completely accurately"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("factual informativeness (comparative)"))(
      Question.ComparativeLikert(
        judgeQuestion = Some("How much did each debater help you understand the story?"),
        numOptions = 5,
        minDescription = "Hardly at all",
        maxDescription = "Debater was highly informative"
      )
    ),
    DotPair[Question](Key.Likert("factual informativeness (total)"))(
      Question.Likert(
        judgeQuestion = Some(
          "Did you feel like the debate as a whole helped you understand the story better as it progressed?"
        ),
        numOptions = 5,
        minDescription = "Hardly at all",
        maxDescription = "Debate was highly informative"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("facts versus semantics"))(
      Question.ComparativeLikert(
        debaterQuestion = Some(
          "How much did each of you focus on matters of fact versus matters of semantics?"
        ),
        judgeQuestion = Some(
          "How much did each debater focus on matters of fact versus matters of semantics?"
        ),
        numOptions = 5,
        minDescription = "Completely about facts",
        maxDescription = "Completely about semantics"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("clarity"))(
      Question.ComparativeLikert(
        debaterQuestion = Some("How clear & understandable do you feel the argumentation was?"),
        judgeQuestion = Some("How clear & understandable do you feel the argumentation was?"),
        numOptions = 5,
        minDescription = "Completely muddled",
        maxDescription = "Completely clear"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("clash"))(
      Question.ComparativeLikert(
        debaterQuestion = Some("How well did you clash with each other's arguments?"),
        judgeQuestion = Some("How well did the debaters clash with each other's arguments?"),
        numOptions = 5,
        minDescription = "No clash",
        maxDescription = "Perfect/excellent clash"
      )
    ),
    DotPair[Question](Key.ComparativeLikert("judge adaptation"))(
      Question.ComparativeLikert(
        debaterQuestion = Some("How well did each of you respond/adapt to the judge's concerns?"),
        judgeQuestion = Some("How well did each debater respond/adapt to your concerns?"),
        numOptions = 5,
        minDescription = "Not at all",
        maxDescription = "Addressed all concerns"
      )
    ),
    DotPair[Question](Key.FreeText("judge strategies"))(
      Question.FreeText(
        judgeQuestion = Some(
          "Were there any specific questions or expectations that you communicated to the debaters in order to influence their behavior?"
        ),
        questionDetails = Some(
          "For example, indicating that they should respond to a specific argument, back up a particular claim with evidence, and so on. If so, please describe."
        )
      )
    ),
    DotPair[Question](Key.FreeText("other factors"))(
      Question.FreeText(judgeQuestion =
        Some("Were there any other factors worth mentioning that were important to your decision?")
      )
    ),
    DotPair[Question](Key.FreeText("interface"))(
      Question.FreeText(
        debaterQuestion = Some(
          "Is there anything about the interface that made your job more difficult?"
        ),
        judgeQuestion = Some(
          "Is there anything about the interface that made your job more difficult?"
        )
      )
    ),
    DotPair[Question](Key.FreeText("protocol"))(
      Question.FreeText(
        debaterQuestion = Some(
          "Is there anything about the protocol that made your job more difficult?"
        ),
        judgeQuestion = Some(
          "Is there anything about the protocol that made your job more difficult?"
        )
      )
    ),
    DotPair[Question](Key.FreeText("other"))(
      Question.FreeText(
        debaterQuestion = Some("Do you have any other feedback/comments to share?"),
        judgeQuestion = Some("Do you have any other feedback/comments to share?")
      )
    )
  )

  def initAnswers: DotMap[Option, Key] = DotMap(
    survey.map { pair =>
      DotPair[Option](pair.fst: Key)(None)
    }: _*
  )
}
