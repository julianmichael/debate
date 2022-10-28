import monocle.Lens
import monocle.macros._

import jjm.ling.ESpan

import cats.implicits._

package object livechat extends PackagePlatformExtensions {
  def makePageTitle(x: String) = {
    (if(x.isEmpty) "" else s"$x | ") + "Debate"
  }
  def answerLetter(index: Int) = ('A' + index).toChar.toString

  def vectorEnd[A]: Lens[Vector[A], Option[A]] =
    Lens.apply[Vector[A], Option[A]](_ => None)(
      opt => vec => opt.fold(vec.init)(vec :+ _)
    )

  def span2text(span: ESpan): String = s"<<${span.begin}-${span.endExclusive}>>"

  def simpleTokenize(x: String): Vector[String] = {
    val res = x.split("\n").toVector.map(Vector(_)).intercalate(Vector("\n")).filter(_.nonEmpty).flatMap(
      _.split(" +").toVector
    )
    // println(x.split("\n").toVector.map(Vector(_)).intercalate(Vector("\n")).filter(_.nonEmpty))
    // println(res)
    res
  }

  def bigTokenize(x: String): Vector[String] = {
    val res = x.split("\n").toVector.map(Vector(_)).intercalate(Vector("\n")).filter(_.nonEmpty).flatMap(
      _.split("""(?<=[\\.!?;]) +""").toVector
    )
    res
  }

  def biggTokenize(x: String): Vector[String] = {
    val res = x.split("\n").toVector.map(Vector(_)).intercalate(Vector("\n")).filter(_.nonEmpty)
    // println(x.split("\n").toVector.map(Vector(_)).intercalate(Vector("\n")).filter(_.nonEmpty))
    // println(res)
    res
  }

  def bigggTokenize(x: String): Vector[String] = {
      val res = x.split("\n\n").toVector.map(Vector(_)).intercalate(Vector("\n\n")).filter(_.nonEmpty)
      // println(x.split("\n").toVector.map(Vector(_)).intercalate(Vector("\n")).filter(_.nonEmpty))
      // println(res)
      res
    }

  def minSecTime(millis: Long): String = {
    val secs = millis / 1000
    val mins = secs / 60
    val secsRem = secs % 60
    s"${mins}m ${secsRem}s"
  }
}
