package debate

import cats.Id

import io.circe.Decoder
import io.circe.Encoder
import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.generic.JsonCodec
import monocle.macros.Lenses

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotMap
import jjm.DotPair

object Feedback {

  // self/opponent for debaters
  // A/B for judges
  @JsonCodec
  @Lenses
  case class ComparativeJudgment(first: Int, second: Int) {
    def isValid = first > -1 && second > -1
  }

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
    case class RoleSelect(key: String) extends Key {
      type Answer = Map[LiveDebateRole, String]
    }

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
              case RoleSelect(_) =>
                implicitly[Encoder[Map[LiveDebateRole, String]]]
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
              case RoleSelect(_) =>
                implicitly[Decoder[Map[LiveDebateRole, String]]]
            }
          res.asInstanceOf[Decoder[key.Out]]
        }
      }
  }

  sealed trait Question[Answer] {
    def questionText(role: Role) = questionTextUnsafe.lift(role)
    def questionTextUnsafe: PartialFunction[Role, String]
    def questionDetails: Option[String]
    def required: Boolean
    def isValid: (DebateSetup, Role) => Boolean
    def isAnswerValid(answer: Answer): Boolean
  }
  object Question {

    case class ComparativeLikert(
      questionTextUnsafe: PartialFunction[Role, String],
      questionDetails: Option[String] = None,
      numOptions: Int,
      minDescription: String,
      maxDescription: String,
      required: Boolean = true,
      isValid: (DebateSetup, Role) => Boolean = (_, _) => true
    ) extends Question[ComparativeJudgment] {
      def isAnswerValid(answer: ComparativeJudgment): Boolean = answer.isValid
    }

    case class Likert(
      questionTextUnsafe: PartialFunction[Role, String],
      questionDetails: Option[String] = None,
      numOptions: Int,
      minDescription: String,
      maxDescription: String,
      required: Boolean = true,
      isValid: (DebateSetup, Role) => Boolean = (_, _) => true
    ) extends Question[Int] {
      def isAnswerValid(answer: Int): Boolean = answer >= 0 && answer <= numOptions
    }

    case class FreeText(
      questionTextUnsafe: PartialFunction[Role, String],
      questionDetails: Option[String] = None,
      required: Boolean = true,
      isValid: (DebateSetup, Role) => Boolean = (_, _) => true
    ) extends Question[String] {
      def isAnswerValid(answer: String): Boolean = true
    }

    case class RoleSelect(
      questionTextUnsafe: PartialFunction[Role, String],
      questionDetails: Option[String] = None,
      required: Boolean = true,
      isValid: (DebateSetup, Role) => Boolean = (_, _) => true
    ) extends Question[Map[LiveDebateRole, String]] {
      def isAnswerValid(answer: Map[LiveDebateRole, String]): Boolean = true
    }
  }

  val survey: List[DotPair[Question, Key]] = List(
    DotPair[Question][Key](Key.FreeText("reason for outcome"))(
      Question.FreeText(
        {
          case Debater(_) =>
            "Why do you think you won/lost this debate?"
          case Judge | OfflineJudge =>
            "How would you explain why you got the right/wrong answer in this debate?"
        },
        isValid = (setup, role) => !(!setup.rules.hasJudge && role.isDebater)
      )
    ),
    DotPair[Question][Key](Key.Likert("subjective correctness"))(
      Question.Likert(
        { case Debater(_) =>
          "Knowing the full story, do you think the original answer to the question is actually correct?"
        },
        questionDetails = Some(
          "An answer of 3 means you think it's disputable, favoring each answer equally."
        ),
        numOptions = 5,
        minDescription = "\"Correct\" answer is wrong",
        maxDescription = "\"Correct\" answer is right"
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("evidence in story"))(
      Question.ComparativeLikert(
        { case Debater(_) =>
          "How would you rate the quality of the evidence present in the story in favor of each side?"
        },
        numOptions = 5,
        minDescription = "Nothing to latch onto",
        maxDescription = "Clear & convincing"
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("evidence use"))(
      Question.ComparativeLikert(
        { case Debater(_) =>
          "Given the evidence available in the story, how effectively did each of you use it?"
        },
        numOptions = 5,
        minDescription = "Not at all",
        maxDescription = "Best possible",
        isValid = (setup, _) => setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.Likert("evidence use (single)"))(
      Question.Likert(
        { case Debater(_) =>
          "Given the evidence available in the story, how effectively did you use it?"
        },
        numOptions = 5,
        minDescription = "Not at all",
        maxDescription = "Best possible",
        isValid = (setup, _) => setup.numAssignedDebaters == 1
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("evidence in debate"))(
      Question.ComparativeLikert(
        { case Judge | OfflineJudge =>
          "How compelling was each debater's evidence?"
        },
        numOptions = 5,
        minDescription = "Poor / irrelevant",
        maxDescription = "Clear & effective",
        isValid = (setup, _) => setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.Likert("evidence in debate (single)"))(
      Question.Likert(
        { case Judge | OfflineJudge =>
          "How compelling was the debater's evidence?"
        },
        numOptions = 5,
        minDescription = "Poor / irrelevant",
        maxDescription = "Clear & effective",
        isValid = (setup, _) => setup.numAssignedDebaters == 1
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("factual accuracy"))(
      Question.ComparativeLikert(
        { case Debater(_) =>
          "How accurately did each of you represent the facts of the story in your arguments?"
        },
        numOptions = 5,
        minDescription = "Completely inaccurately",
        maxDescription = "Completely accurately",
        isValid = (setup, _) => setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.Likert("factual accuracy (single)"))(
      Question.Likert(
        { case Debater(_) =>
          "How accurately did you represent the facts of the story in your arguments?"
        },
        numOptions = 5,
        minDescription = "Completely inaccurately",
        maxDescription = "Completely accurately",
        isValid = (setup, _) => setup.numAssignedDebaters == 1
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("factual informativeness (comparative)"))(
      Question.ComparativeLikert(
        { case Judge | OfflineJudge =>
          "How much did each debater help you understand the story?"
        },
        numOptions = 5,
        minDescription = "Not at all",
        maxDescription = "Highly informative",
        isValid = (setup, _) => setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.Likert("factual informativeness (total)"))(
      Question.Likert(
        {
          case Judge =>
            "Did you feel like the debate as a whole helped you understand the story better as it progressed?"
          case OfflineJudge =>
            "Did you feel like the debate as a whole helped you understand the story?"
        },
        numOptions = 5,
        minDescription = "Not at all",
        maxDescription = "Highly informative"
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("facts versus semantics"))(
      Question.ComparativeLikert(
        {
          case Debater(_) =>
            "How much did each of you focus on matters of fact versus matters of semantics?"
          case Judge | OfflineJudge =>
            "How much did each debater focus on matters of fact versus matters of semantics?"
        },
        numOptions = 5,
        minDescription = "Completely facts",
        maxDescription = "Completely semantics",
        isValid = (setup, _) => setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.Likert("facts versus semantics (single)"))(
      Question.Likert(
        {
          case Debater(_) =>
            "How much did you focus on matters of fact versus matters of semantics?"
          case Judge | OfflineJudge =>
            "How much did the debater focus on matters of fact versus matters of semantics?"
        },
        numOptions = 5,
        minDescription = "Completely facts",
        maxDescription = "Completely semantics",
        isValid = (setup, _) => setup.numAssignedDebaters == 1
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("clarity"))(
      Question.ComparativeLikert(
        { case _ =>
          "How clear & understandable do you feel the argumentation was?"
        },
        numOptions = 5,
        minDescription = "Completely muddled",
        maxDescription = "Completely clear",
        isValid = (setup, _) => setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.Likert("clarity (single)"))(
      Question.Likert(
        { case _ =>
          "How clear & understandable do you feel the argumentation was?"
        },
        numOptions = 5,
        minDescription = "Completely muddled",
        maxDescription = "Completely clear",
        isValid = (setup, _) => setup.numAssignedDebaters == 1
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("clash"))(
      Question.ComparativeLikert(
        {
          case Debater(_) =>
            "How well did you clash with each other's arguments?"
          case Judge | OfflineJudge =>
            "How well did the debaters clash with each other's arguments?"
        },
        numOptions = 5,
        minDescription = "No clash",
        maxDescription = "Addressed all arguments",
        isValid = (setup, _) => setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.ComparativeLikert("judge adaptation"))(
      Question.ComparativeLikert(
        {
          case Debater(_) =>
            "How well did each of you respond/adapt to the judge's concerns?"
          case Judge =>
            "How well did each debater respond/adapt to your concerns?"
          case OfflineJudge =>
            "How well did each debater respond/adapt to the judge's concerns?"
        },
        numOptions = 5,
        minDescription = "Not at all",
        maxDescription = "Addressed all concerns",
        isValid = (setup, _) => setup.rules.hasJudge && setup.numAssignedDebaters == 2
      )
    ),
    DotPair[Question][Key](Key.Likert("judge adaptation (single)"))(
      Question.Likert(
        {
          case Debater(_) =>
            "How well did you respond/adapt to the judge's concerns?"
          case Judge =>
            "How well did the debater respond/adapt to your concerns?"
          case OfflineJudge =>
            "How well did the debater respond/adapt to the judge's concerns?"
        },
        numOptions = 5,
        minDescription = "Not at all",
        maxDescription = "Addressed all concerns",
        isValid = (setup, _) => setup.rules.hasJudge && setup.numAssignedDebaters == 1
      )
    ),
    DotPair[Question][Key](Key.Likert("judge reasoning"))(
      Question.Likert(
        {
          case Debater(_) =>
            "How sound was the judge's reasoning? Did they make their final judgment for the right reasons, given the debate?"
          case Judge | OfflineJudge =>
            "In retrospect, given the information you had available, how sound do you think your reasoning was behind your final judgment?"
        },
        numOptions = 5,
        minDescription = "Made critical mistakes",
        maxDescription = "Totally sound",
        isValid = (setup, role) => role.isInstanceOf[Debater] --> setup.rules.hasJudge
      )
    ),
    DotPair[Question][Key](Key.FreeText("judge strategies"))(
      Question.FreeText(
        { case Judge =>
          "Were there any specific questions or expectations that you communicated to the debaters in order to influence their behavior?"
        },
        questionDetails = Some(
          "For example, indicating that they should respond to a specific argument, back up a particular claim with evidence, and so on. If so, please describe."
        ),
        required = false
      )
    ),
    DotPair[Question][Key](Key.FreeText("other factors"))(
      Question.FreeText(
        { case Judge | OfflineJudge =>
          "Were there any other factors worth mentioning that were important to your decision?"
        },
        required = false
      )
    ),
    DotPair[Question][Key](Key.FreeText("interface"))(
      Question.FreeText(
        { case _ =>
          "Is there anything about the interface that made your job more difficult?"
        },
        required = false
      )
    ),
    DotPair[Question][Key](Key.FreeText("protocol"))(
      Question.FreeText(
        { case _ =>
          "Is there anything about the protocol that made your job more difficult?"
        },
        required = false
      )
    ),
    DotPair[Question][Key](Key.RoleSelect("identity guesses"))(
      Question.RoleSelect(
        {
          case Debater(_) | Judge =>
            "Do you know or can you guess who else was in this debate?"
          case OfflineJudge =>
            "Do you know or can you guess who was in this debate?"
        },
        required = false
      )
    ),
    DotPair[Question][Key](Key.FreeText("other"))(
      Question.FreeText(
        { case _ =>
          "Do you have any other feedback/comments to share?"
        },
        required = false
      )
    )
  )

  def questions: DotMap[Question, Key] = DotMap(survey: _*)

  def initAnswers(setup: DebateSetup, role: Role): DotMap[Option, Key] = DotMap(
    survey
      .filter(pair => pair.snd.questionText(role).nonEmpty && pair.snd.isValid(setup, role))
      .map { pair =>
        DotPair[Option](pair.fst: Key)(None)
      }: _*
  )

  @JsonCodec
  sealed trait SurveyResponse {
    def answers: DotMap[Id, Key]
  }
  object SurveyResponse {
    case class Debater(answers: DotMap[Id, Key])      extends SurveyResponse
    case class Judge(answers: DotMap[Id, Key])        extends SurveyResponse
    case class OfflineJudge(answers: DotMap[Id, Key]) extends SurveyResponse

  }

}
