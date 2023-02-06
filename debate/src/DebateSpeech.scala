package debate

import cats.implicits._

import io.circe.generic.JsonCodec

import jjm.ling.ESpan

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
    if (indexOfStartTag < 0)
      Vector(SpeechSegment.Text(content))
    else { // no quotes
      val postStartTag  = content.substring(indexOfStartTag + 2)
      val indexOfEndTag = postStartTag.indexOf(">>")
      if (indexOfEndTag < 0)
        Vector(SpeechSegment.Text(content))
      else { // tag wasn't closed
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

  // def getQuoteLength(source: Vector[String], speechSegments: Vector[SpeechSegment]) = speechSegments
  //   .foldMap {
  //     case SpeechSegment.Text(_) =>
  //       0
  //     case SpeechSegment.Quote(span) =>
  //       Utils.renderSpan(source, span).size
  //   }

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
