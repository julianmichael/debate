package debate

import io.circe.generic.JsonCodec
import jjm.ling.ESpan

/** A single contiguous argument by a debater.
  *
  * @param speaker
  *   the ID of the debater who wrote the speech.
  * @param timestamp
  *   millis from epoch from which the speech was written.
  * @param content
  *   the debater's argument
  */
@JsonCodec case class DebateSpeech(
    speaker: ParticipantId,
    timestamp: Long,
    content: Vector[SpeechSegment]
) {
  def allQuotes = content.collect { case SpeechSegment.Quote(span) => span }
}
object DebateSpeech {}

/** Segment of a single contiguous argument by a debater. Can be free text
  * (Text) or evidence (Quote). We could potentially incorporate other types of
  * evidence or content later (e.g., links, images, etc.).
  */
@JsonCodec sealed trait SpeechSegment {
  def isEmpty: Boolean
}
object SpeechSegment {
  @JsonCodec case class Text(text: String) extends SpeechSegment {
    def isEmpty = text.isEmpty
  }
  @JsonCodec case class Quote(span: ESpan) extends SpeechSegment {
    def isEmpty = span.begin == span.endExclusive
  }

  val CharRange = "([0-9]+)-([0-9]+)".r

  private[this] def getSegmentsFromStringAux(
      content: String
  ): Vector[SpeechSegment] = {
    val indexOfStartTag = content.indexOf("<<")
    if (indexOfStartTag < 0) Vector(Text(content))
    else { // no quotes
      // val initialText = Text(content.substring(0, indexOfStartTag))
      val postStartTag = content.substring(indexOfStartTag + 2)
      val indexOfEndTag = postStartTag.indexOf(">>")
      if (indexOfEndTag < 0) Vector(Text(content))
      else { // tag wasn't closed
        val tagContent = postStartTag.substring(0, indexOfEndTag)
        val postEndTag = postStartTag.substring(indexOfEndTag + 2)
        tagContent match {
          case CharRange(begin, end) =>
            Vector(
              Text(content.substring(0, indexOfStartTag)),
              Quote(ESpan(begin.toInt, end.toInt))
            ) ++ getSegmentsFromStringAux(postEndTag)
          case _ =>
            Vector(
              Text(content.substring(0, indexOfStartTag + 2))
            ) ++ getSegmentsFromStringAux(postStartTag)
        }
      }

    }
  }
  def collapse(segments: Vector[SpeechSegment]): Vector[SpeechSegment] = {
    segments
      .filterNot(s => s.isEmpty)
      .foldLeft(List.empty[SpeechSegment]) {
        case (Text(x) :: rest, Text(y)) => Text(x + y) :: rest
        case (acc, next)                => next :: acc
      }
      .toVector
      .reverse
  }
  def getSegmentsFromString(content: String): Vector[SpeechSegment] = {
    collapse(getSegmentsFromStringAux(content))
  }
}
