package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

import io.circe.{KeyDecoder, KeyEncoder}

import cats.implicits._

@JsonCodec
@Lenses
case class DebateSetting(isHuman: Boolean, isDebate: Boolean)
object DebateSetting {
  implicit def debateSettingShow = cats
    .Show
    .show[DebateSetting] { setting =>
      val human =
        if (setting.isHuman)
          "Human"
        else
          "AI"
      val debate =
        if (setting.isDebate)
          "debate"
        else
          "consultancy"
      s"$human $debate"
    }
  implicit def debateSettingKeyEncoder = KeyEncoder.instance[DebateSetting](_.show)
  implicit def debateSettingKeyDecoder = KeyDecoder.instance[DebateSetting] {
    case "Human debate" =>
      Some(DebateSetting(true, true))
    case "Human consultancy" =>
      Some(DebateSetting(true, false))
    case "AI debate" =>
      Some(DebateSetting(false, true))
    case "AI consultancy" =>
      Some(DebateSetting(false, false))
    case _ =>
      None
  }
  def fromDebate(debate: Debate): DebateSetting = {
    val isAI = debate.setup.roles.values.toList.exists(_ == "GPT-4")
    val isDebate =
      debate
        .setup
        .roles
        .keySet
        .collect { case Debater(i) =>
          i
        }
        .size > 1
    DebateSetting(isHuman = !isAI, isDebate = isDebate)
  }
}
