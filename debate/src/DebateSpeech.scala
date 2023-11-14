package debate

import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

import jjm.ling.ESpan

@Lenses
@JsonCodec
case class DebateSpeech(speaker: String, timestamp: Long, content: Vector[SpeechSegment]) {
  def allQuotes = content.collect { case SpeechSegment.Quote(span) =>
    span
  }
}
object DebateSpeech {}

/** Segment of a single contiguous argument by a debater. Can be free text
  * (Text) or evidence (Quote). We could potentially incorporate other types of
  * evidence or content later (e.g., links, images, etc.).
  */
@JsonCodec
sealed trait SpeechSegment {
  def isEmpty: Boolean
}
object SpeechSegment {
  @JsonCodec
  case class Text(text: String) extends SpeechSegment {
    def isEmpty = text.isEmpty
  }
  @JsonCodec
  case class Quote(span: ESpan) extends SpeechSegment {
    def isEmpty = span.begin == span.endExclusive
  }
}

object SpeechSegments {
  val CharRange = "([0-9]+)-([0-9]+)".r

  private[this] def getFromStringAux(content: String): Vector[SpeechSegment] = {
    val indexOfStartTag = content.indexOf("<<")
    if (indexOfStartTag < 0) // no quotes
      Vector(SpeechSegment.Text(content))
    else {
      val postStartTag  = content.substring(indexOfStartTag + 2)
      val indexOfEndTag = postStartTag.indexOf(">>")
      if (indexOfEndTag < 0) // tag wasn't closed
        Vector(SpeechSegment.Text(content))
      else {
        val tagContent = postStartTag.substring(0, indexOfEndTag)
        val postEndTag = postStartTag.substring(indexOfEndTag + 2)
        tagContent match {
          case CharRange(begin, end) =>
            Vector(
              SpeechSegment.Text(content.substring(0, indexOfStartTag)),
              SpeechSegment.Quote(ESpan(begin.toInt, end.toInt))
            ) ++ getFromStringAux(postEndTag)
          case _ =>
            Vector(SpeechSegment.Text(content.substring(0, indexOfStartTag + 2))) ++
              getFromStringAux(postStartTag)
        }
      }

    }
  }
  def collapse(segments: Vector[SpeechSegment]): Vector[SpeechSegment] = {
    import SpeechSegment.Text
    segments
      .filterNot(s => s.isEmpty)
      .foldLeft(List.empty[SpeechSegment]) {
        case (Text(x) :: rest, Text(y)) =>
          Text(x + y) :: rest
        case (acc, next) =>
          next :: acc
      }
      .toVector
      .reverse
  }
  def getFromString(content: String): Vector[SpeechSegment] = collapse(getFromStringAux(content))

  def getSpeechLength(source: Vector[String], speechSegments: Vector[SpeechSegment]) =
    speechSegments.foldMap {
      case SpeechSegment.Text(text) =>
        text.size
      case SpeechSegment.Quote(span) =>
        Utils.renderSpan(source, span).size
    }

  def getSpeechString(
    source: Vector[String],
    speechSegments: Vector[SpeechSegment],
    quoteDelimiters: (String, String), // = ("```", "```")
    renderQuoteIndices: ESpan => String
  ) = speechSegments.foldMap {
    case SpeechSegment.Text(text) =>
      text
    case SpeechSegment.Quote(span) =>
      quoteDelimiters._1 + Utils.renderSpan(source, span) + quoteDelimiters._2 +
        renderQuoteIndices(span)
  }

  // def getQuoteLength(source: Vector[String], speechSegments: Vector[SpeechSegment]) = speechSegments
  //   .foldMap {
  //     case SpeechSegment.Text(_) =>
  //       0
  //     case SpeechSegment.Quote(span) =>
  //       Utils.renderSpan(source, span).size
  //   }

  implicit class RichESpan(span: ESpan) {
    def -(other: ESpan): Vector[ESpan] = {
      val (begin, end)           = (span.begin, span.endExclusive)
      val (otherBegin, otherEnd) = (other.begin, other.endExclusive)
      if (begin >= otherEnd || end <= otherBegin) {
        Vector(span)
      } else if (begin < otherBegin && end > otherEnd) {
        Vector(ESpan(begin, otherBegin), ESpan(otherEnd, end))
      } else if (begin < otherBegin) {
        Vector(ESpan(begin, otherBegin))
      } else if (end > otherEnd) {
        Vector(ESpan(otherEnd, end))
      } else {
        Vector.empty
      }
    }
  }

  def getNewQuotes(
    coveredSpans: Set[ESpan],
    // source: Vector[String],
    speechSegments: Vector[SpeechSegment]
  ) = {

    val allNewSpans = speechSegments
      .collect { case SpeechSegment.Quote(span) =>
        span
      }
      .flatMap(span =>
        coveredSpans.foldLeft(Vector(span))((acc, coveredSpan) => acc.flatMap(_ - coveredSpan))
      )
    val collapsedSpans =
      allNewSpans.foldLeft(Set.empty[ESpan]) { case (acc, span) =>
        acc.find(_.overlaps(span)) match {
          case None =>
            acc + span
          case Some(overlapper) =>
            acc - overlapper + (span |+| overlapper)
        }
      }
    collapsedSpans
    // .unorderedFoldMap(Utils.renderSpan(source, _).size)
  }

  def getQuoteCoverage(source: Vector[String], speechSegments: Vector[SpeechSegment]) = {

    val allSpans = speechSegments.collect { case SpeechSegment.Quote(span) =>
      span
    }
    val collapsedSpans =
      allSpans.foldLeft(Set.empty[ESpan]) { case (acc, span) =>
        acc.find(_.overlaps(span)) match {
          case None =>
            acc + span
          case Some(overlapper) =>
            acc - overlapper + (span |+| overlapper)
        }
      }
    collapsedSpans.unorderedFoldMap(Utils.renderSpan(source, _).size)
  }

  def getString(speechSegments: Vector[SpeechSegment]) = speechSegments.foldMap {
    case SpeechSegment.Text(text) =>
      text
    case SpeechSegment.Quote(span) =>
      span2text(span)
  }

}
