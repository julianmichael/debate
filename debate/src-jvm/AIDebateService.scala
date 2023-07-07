package debate

import scala.language.existentials

import cats.effect.Effect
import cats.implicits._

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.JsonCodec
import org.http4s.Uri
import org.http4s.client.Client

import jjm.DotDecoder
import jjm.DotEncoder
import jjm.DotKleisli
import jjm.implicits._
import jjm.ling.ESpan
import jjm.ling.HasSourceText
import jjm.ling.HasToken
import jjm.ling.Text

import debate.SpeechSegment.Quote

trait AIDebateService[F[_]] extends DotKleisli[F, AIDebateService.Request] {
  def takeTurn[Speech](
    debate: Debate,
    profile: Profile.AI,
    role: DebateRole,
    turnType: DebateTurnType {
      type Input = Speech
    }
  ): F[Speech]

  import AIDebateService.Request
  def apply(req: Request): F[req.Out] = {
    val res =
      req match {
        case Request.TakeTurn(debate, profile, role, turnType) =>
          takeTurn[turnType.Input](debate, profile, role, turnType)
      }
    res.asInstanceOf[F[req.Out]]
  }
}
object AIDebateService {

  def apply[F[_]](f: DotKleisli[F, Request]) =
    new AIDebateService[F] {
      def takeTurn[Speech](
        debate: Debate,
        profile: Profile.AI,
        role: DebateRole,
        turn: DebateTurnType {
          type Input = Speech
        }
      ): F[Speech] = f(Request.TakeTurn(debate, profile, role, turn)).asInstanceOf[F[Speech]]
      // TODO: why can't the compiler figure this out?
    }

  @JsonCodec
  case class Response(response: String)

  sealed trait Request {
    type Out
  }
  object Request {
    case class TakeTurn(debate: Debate, profile: Profile.AI, role: DebateRole, turn: DebateTurnType)
        extends Request {
      type Out = turn.Input
    }

    implicit val aiDebateServiceRequestDotEncoder =
      new DotEncoder[Request] {
        def apply(req: Request) = {
          val res =
            req match {
              case TakeTurn(_, _, _, _) =>
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
              case TakeTurn(_, _, _, _) =>
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
    turnType: String // "single debater" | "sequential" | "simultaneous"
  )
  object DebateTurnPrompt {
    def fromDebate[Speech](
      debate: Debate,
      role: DebateRole,
      turnType: DebateTurnType {
        type Input = Speech
      }
    ): DebateTurnPrompt = {
      // TODO DebatePanel.visibleRounds
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
        turnType =
          turnType match {
            case DebateTurnType.SimultaneousSpeechesTurn(_, _, _) =>
              "simultaneous"
            case DebateTurnType.DebaterSpeechTurn(_, _, _) =>
              if (
                debate
                  .setup
                  .roles
                  .keySet
                  .collect { case Debater(i) =>
                    i
                  }
                  .size == 1
              ) {
                // technically we would want to check the round type for assignedDebatersOnly,
                // but in practice this will work as long as everyone is assigned who needs to be.
                "single debater"
              } else {
                "sequential"
              }
            case _ =>
              // shouldn't happen since we're only having the model do debating in the presence
              // of a live judge
              ???

          }
      )
    }
  }

  // adapted from https://stackoverflow.com/questions/44672145/functional-way-to-find-the-longest-common-substring-between-two-strings-in-scala
  def longestCommonVectorSubstring[A](a: Vector[A], b: Vector[A]): ((Int, Int), Vector[A]) = {
    def loop(
      bestLengths: Map[(Int, Int), Int],
      bestIndices: (Int, Int),
      i: Int,
      j: Int
    ): ((Int, Int), Vector[A]) =
      // if we're done, return the best substring
      if (i > a.length) {
        val bestJ      = bestIndices._2
        val bestLength = bestLengths(bestIndices)
        (
          (bestIndices._1 - bestLength, bestIndices._2 - bestLength),
          b.slice(bestJ - bestLength, bestJ)
        )
      } else {
        // compute the best length for a match at (i, j)
        val currentLength: Int =
          if (a(i - 1) == b(j - 1)) {
            bestLengths(i - 1, j - 1) + 1
          } else
            0
        // store this result if it's nonzero
        val newBestLengths =
          if (currentLength != 0)
            bestLengths + ((i, j) -> currentLength)
          else
            bestLengths
        // update the best indices if we've found a longer substring
        val newBestIndices =
          if (currentLength > bestLengths(bestIndices))
            (i, j)
          else
            bestIndices
        // loop through j second
        val (newI, newJ) =
          if (j == b.length)
            (i + 1, 1)
          else
            (i, j + 1)
        loop(newBestLengths, newBestIndices, newI, newJ)
      }

    if (b.isEmpty)
      ((0, 0), Vector[A]())
    else
      loop(Map.empty[(Int, Int), Int].withDefaultValue(0), (0, 0), 1, 1)
  }

  // adapted from https://stackoverflow.com/questions/44672145/functional-way-to-find-the-longest-common-substring-between-two-strings-in-scala
  // allows for extra whitespace in one or the other.
  // max length is computed based on QUOTE tokens.
  def longestCommonVectorSubstring[A](
    a: Vector[A],
    b: Vector[A],
    isCollapsible: A => Boolean
  ): ((Int, Int), (Int, Int), Vector[A]) = {
    def loop(
      bestLengths: Map[(Int, Int), (Int, Int)],
      bestIndices: (Int, Int),
      i: Int,
      j: Int
    ): ((Int, Int), (Int, Int), Vector[A]) =
      // if we're done, return the best substring
      if (i > a.length) {
        val bestI = bestIndices._1
        (bestIndices, bestLengths(bestIndices), a.slice(bestI - bestLengths(bestIndices)._1, bestI))
      } else {
        // compute the best length for a match at (i, j)
        val currentLengths: (Int, Int) = {
          val (prevILen, prevJLen) = bestLengths(i - 1, j - 1)
          if (a(i - 1) == b(j - 1)) {
            (prevILen + 1, prevJLen + 1)
          } else if (isCollapsible(a(i - 1)) && isCollapsible(b(j - 1))) {
            (prevILen + 1, prevJLen + 1)
          } else if (isCollapsible(a(i - 1))) {
            (prevILen + 1, prevJLen)
          } else if (isCollapsible(b(j - 1))) {
            (prevILen, prevJLen + 1)
          } else
            (0, 0)
        }
        // store this result if it's nonzero
        val newBestLengths =
          if (currentLengths != (0, 0))
            bestLengths + ((i, j) -> currentLengths)
          else
            bestLengths
        // update the best indices if we've found a longer substring
        val newBestIndices =
          if (currentLengths._2 > bestLengths(bestIndices)._2)
            (i, j)
          else
            bestIndices
        // loop through j second
        val (newI, newJ) =
          if (j == b.length)
            (i + 1, 1)
          else
            (i, j + 1)
        loop(newBestLengths, newBestIndices, newI, newJ)
      }

    if (b.isEmpty)
      ((0, 0), (0, 0), Vector[A]())
    else
      loop(Map.empty[(Int, Int), (Int, Int)].withDefaultValue((0, 0)), (0, 0), 1, 1)
  }

  val quoteSpanningBufferSize = 10
  def recursivelyReconstructQuotes[A: HasSourceText: HasToken](
    storyTokens: Vector[String],
    quoteTokens: Vector[A],
    storyOffset: Int,
    mustBeAtBeginning: Boolean = false,
    mustBeAtEnd: Boolean = false
  ): Vector[SpeechSegment] =
    // println(s"recursivelyReconstructQuotes: ${storyTokens.size} ${quoteTokens.size}")
    // println(quoteTokens)

    if (storyTokens.isEmpty || quoteTokens.isEmpty)
      Vector.empty[SpeechSegment]
    else {
      val ((storyIndex, quoteIndex), sequence) = longestCommonVectorSubstring(
        storyTokens,
        quoteTokens.map(_.token)
      )
      println(s"longest subsequence: $sequence")
      if (sequence.size == 0) {
        Vector(SpeechSegment.Text(Text.render(quoteTokens)))
      } else if (
        mustBeAtEnd &&
        (storyTokens.length -
          (storyIndex + quoteTokens.length - quoteIndex) > quoteSpanningBufferSize)
      ) {
        Vector(SpeechSegment.Text(Text.render(quoteTokens)))
      } else if (mustBeAtBeginning && (storyIndex - quoteIndex > quoteSpanningBufferSize)) {
        Vector(SpeechSegment.Text(Text.render(quoteTokens)))
      } else {
        val preQuotes =
          if (quoteIndex > 0) {
            val storyBeforeCertifiedQuote = storyTokens.slice(0, storyIndex)
            val quoteBeforeCertifiedQuote = quoteTokens.slice(0, quoteIndex)
            recursivelyReconstructQuotes(
              storyBeforeCertifiedQuote,
              quoteBeforeCertifiedQuote,
              storyOffset = storyOffset,
              mustBeAtEnd = true
            )
          } else
            Vector.empty[SpeechSegment]
        val postQuotes =
          if (quoteIndex + sequence.length < quoteTokens.length) {
            val storyAfterCertifiedQuote = storyTokens
              .slice(storyIndex + sequence.length, storyTokens.length)
            val quoteAfterCertifiedQuote = quoteTokens
              .slice(quoteIndex + sequence.length, quoteTokens.length)
            recursivelyReconstructQuotes(
              storyAfterCertifiedQuote,
              quoteAfterCertifiedQuote,
              storyOffset = storyOffset + storyIndex + sequence.length,
              mustBeAtBeginning = true
            )
          } else
            Vector.empty[SpeechSegment]
        (preQuotes :+
          Quote(ESpan(storyOffset + storyIndex, storyOffset + storyIndex + sequence.length))) ++
          postQuotes

      }
    }

  def reconstructSpeech(story: SourceMaterial, msg: String): Vector[SpeechSegment] = {
    println(s"Original speech:\n$msg")
    val startDelimiter = "\\<quote>"
    val endDelimiter   = "\\</quote>"
    // import jjm.implicits._
    val segments = msg.split(startDelimiter).toVector
    val result =
      SpeechSegment.Text(segments.head) +:
        segments
          .tail
          .flatMap { segmentWithEndDelimiter =>
            val quote :: nonQuoteSegments = segmentWithEndDelimiter.split(endDelimiter).toList
            val quoteTokens               = Server.tokenizeStoryAligned(quote)
            // val ((storyTokenIndex, quoteTokenIndex), _, bestSubsequence) =
            //   longestCommonVectorSubstring(
            //     story.contents,
            //     quoteTokens,
            //     isCollapsible = "\\s+".r.matches
            //   )
            SpeechSegments.collapse(
              recursivelyReconstructQuotes(story.contents, quoteTokens, storyOffset = 0) ++
                nonQuoteSegments.map(SpeechSegment.Text(_)).filter(_.text.nonEmpty)
            )
          // SpeechSegments.collapse(
          //   Vector(
          //     Option(SpeechSegment.Text("\"")),
          //     Option(quoteTokenIndex)
          //       .filter(_ > 0)
          //       .map { quoteTokenIndex =>
          //         SpeechSegment.Text(Text.renderSpan(quoteTokens, ESpan(0, quoteTokenIndex)))
          //       },
          //     Option(bestSubsequence.size)
          //       .filter(_ > 0)
          //       .map(subseqLen =>
          //         SpeechSegment.Quote(ESpan(storyTokenIndex, storyTokenIndex + subseqLen))
          //       ),
          //     Option(quoteTokenIndex + bestSubsequence.size)
          //       .filter(_ < quote.size)
          //       .map { quoteTokenIndex =>
          //         SpeechSegment.Text(
          //           Text.renderSpan(
          //             quoteTokens,
          //             ESpan(quoteTokenIndex + bestSubsequence.size, quote.size)
          //           )
          //         )
          //       },
          //     Option(SpeechSegment.Text("\"" + nonQuoteSegments.mkString(" ")))
          //   ).flatten
          // )
          }
    println(s"\nReconstructed speech:\n$result")
    result
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
        profile: Profile.AI,
        role: DebateRole,
        turn: DebateTurnType {
          type Input = Speech
        }
      ) = {
        val entity = DebateTurnPrompt.fromDebate(debate, role, turn)
        // TODO modify endpoint as necessary (if necessary)
        // TODO compile-time uri handling
        val endpoint = Uri.unsafeFromString(s"http://localhost:$localPort/debate")
        // Speech will always be DebateSpeech for now, but later we can match on DebateTurnType
        // to make sure it matches if we need to.
        def getErrorString(msg: String) =
          s"""Received an error from the AI server:
             |
             |"$msg"
             |
             |Ask an admin to resolve the error and retry this turn of the debate.
             |""".stripMargin
        import io.circe.parser.decode
        client
          .fetchAs[String](HttpRequest[F](method = Method.POST, uri = endpoint).withEntity(entity))
          .map(s =>
            decode[io.circe.Json](s)
              .toOption
              .flatMap(_.asObject)
              .flatMap(_.apply("detail"))
              .flatMap(_.asString)
              .map(getErrorString)
              // .map(_.spaces2)
              .orElse(decode[String](s).toOption).getOrElse(s)
          )
          // .redeem(
          //   error => "Error returned from AI server: " + error.getMessage,
          //   // decode[io.circe.Json](error.getMessage)
          //   //   .toOption
          //   //   .orElse(decode[String](error.getMessage).toOption)
          //   //   .getOrElse(error.getMessage),
          //   body => decode[String](body).toOption.get
          // )
          .map(msg =>
            DebateSpeech(
              profile.name,
              System.currentTimeMillis(),
              reconstructSpeech(debate.setup.sourceMaterial, msg)
            ).asInstanceOf[Speech]
          )
      }
    }
  }
}
