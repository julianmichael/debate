package debate

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.JsonCodec

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotKleisli
import cats.effect.Effect
import cats.implicits._
import org.http4s.client.Client
import org.http4s.Uri
import io.circe.Json

import scala.language.existentials

trait AIDebateService[F[_]] extends DotKleisli[F, AIDebateService.Request] {
  def takeTurn[Speech](
    debate: Debate,
    role: DebateRole,
    turnType: DebateTurnType {
      type Input = Speech
    }
  ): F[Speech]

  import AIDebateService.Request
  def apply(req: Request): F[req.Out] = {
    val res =
      req match {
        case Request.TakeTurn(debate, role, turnType) =>
          takeTurn[turnType.Input](debate, role, turnType)
      }
    res.asInstanceOf[F[req.Out]]
  }
}
object AIDebateService {

  def apply[F[_]](f: DotKleisli[F, Request]) =
    new AIDebateService[F] {
      def takeTurn[Speech](
        debate: Debate,
        role: DebateRole,
        turn: DebateTurnType {
          type Input = Speech
        }
      ): F[Speech] = f(Request.TakeTurn(debate, role, turn)).asInstanceOf[F[Speech]]
      // TODO: why can't the compiler figure this out?
    }

  @JsonCodec
  case class Response(response: String)

  sealed trait Request {
    type Out
  }
  object Request {
    case class TakeTurn(debate: Debate, role: DebateRole, turn: DebateTurnType) extends Request {
      type Out = turn.Input
    }

    implicit val aiDebateServiceRequestDotEncoder =
      new DotEncoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case TakeTurn(_, _, _) =>
                implicitly[Encoder[Response]]
            }
          res.asInstanceOf[Encoder[req.Out]]
        }
      }
    implicit val aiDebateServiceRequestDotDecoder =
      new DotDecoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case TakeTurn(_, _, _) =>
                implicitly[Decoder[Response]]
            }
          Decoder
            .decodeJson
            .flatMap { response =>
              println(response)
              res.asInstanceOf[Decoder[req.Out]]
            }
        }
      }
  }

  @JsonCodec
  case class DebateTurnPrompt(
    storyId: String,
    storyTitle: String,
    story: String,
    question: String,
    answers: Vector[String],
    turns: List[ReadableTurn],
    debaterIndex: Int,
    charLimitOpt: Option[Int],
    quoteCharLimitOpt: Option[Int],
    isSimultaneous: Boolean // simultaneous or sequential speeches
  )
  object DebateTurnPrompt {
    def fromDebate[Speech](
      debate: Debate,
      role: DebateRole,
      turnType: DebateTurnType {
        type Input = Speech
      }
    ): DebateTurnPrompt = {
      val readableDebate = ReadableDebate.fromDebate(debate)
      DebateTurnPrompt(
        storyId = readableDebate.storyId,
        storyTitle = readableDebate.storyTitle,
        story = readableDebate.story,
        question = readableDebate.question,
        answers = readableDebate.answers,
        turns = readableDebate.turns,
        debaterIndex =
          Option(role)
            .collect { case Debater(i) =>
              i
            }
            .get, // debaters only
        charLimitOpt = turnType.charLimitOpt,
        quoteCharLimitOpt = turnType.quoteLimit,
        isSimultaneous =
          turnType match {
            case DebateTurnType.SimultaneousSpeechesTurn(_, _, _) =>
              true
            case _ =>
              false
          }
      )
    }
  }

  def forLocalServer[F[_]: Effect](client: Client[F], localPort: Int) = {
    import org.http4s.{Request => HttpRequest}
    import org.http4s.Method
    import org.http4s.circe._
    implicit val entityEncoder = jsonEncoderOf[F, DebateTurnPrompt]
    // implicit val entityDecoder = jsonDecoderOf[F, Response]
    new AIDebateService[F] {
      // def takeTurn(debate: DebateTurnPrompt) = {
      def takeTurn[Speech](
        debate: Debate,
        role: DebateRole,
        turn: DebateTurnType {
          type Input = Speech
        }
      ) = {
        val entity = DebateTurnPrompt.fromDebate(debate, role, turn)
        // TODO modify endpoint as necessary (if necessary)
        // TODO compile-time uri handling
        val endpoint      = Uri.unsafeFromString(s"http://localhost:$localPort/debate")
        val speechDecoder = DebateTurnType.debateTurnTypeDotDecoder(turn)
        client
          .fetchAs[Json](HttpRequest[F](method = Method.POST, uri = endpoint).withEntity(entity))
          .map(speechDecoder.decodeJson(_))
          .map(_.leftMap(new RuntimeException(_)))
          .flatMap(Effect[F].fromEither[Speech](_))
      }
    }
  }
}
