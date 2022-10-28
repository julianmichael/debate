package livechat.audio

import scalajs.js
import scalajs.js.annotation.JSGlobal
import org.scalajs.dom.raw.AudioContext

import scala.language.implicitConversions

object WebkitAudioContext {
  @js.native
  @JSGlobal("webkitAudioContext")
  class webkitAudioContext() extends js.Any
  implicit def webkitAudioContextToAudioContext(wac: webkitAudioContext): AudioContext =
    wac.asInstanceOf[AudioContext]
}
