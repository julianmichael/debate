package debate
package view.lobby

import japgolly.scalajs.react.vdom.html_<^._
// import japgolly.scalajs.react.Callback
import debate.util.SetConfig
import scalacss.ScalaCssReact._
import debate.util.Local

import cats.implicits._
import jjm.implicits._

object StoriesAnalyticsPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  def apply(lobby: Lobby) =
    Local[Set[String]].syncedWithLocalStorage("story-reader-excludes", Set.empty) { excludes =>
      val thMod = c"text-left"
      // boolean: whether the person needs to be assigned more judging
      val initialMap: Map[SourceMaterialId, Map[String, Boolean]] = lobby
        .storyRecord
        .toVector
        .foldMap { case (person, storyInfo) =>
          storyInfo
            .toVector
            .foldMap { case (sourceMaterialId, storyStats) =>
              val personIfUnfinished =
                if (
                  storyStats.debating.isEmpty &&
                  (storyStats.needsToJudgeStory || storyStats.canJudgeMore)
                )
                  Map[String, Vector[Boolean]](person -> Vector(storyStats.canJudgeMore))
                else
                  Map.empty[String, Vector[Boolean]]
              Map(sourceMaterialId -> personIfUnfinished)
            }
        }
        .mapVals(_.mapVals(_.exists(identity)))
      val mapWithMissingElements = initialMap.map { case (story, people) =>
        story ->
          ((people ++ lobby.storyRecord.filter(p => !p._2.contains(story)).keySet.map(_ -> true)) --
            excludes.value)
      }

      <.div(
        <.p(
          "Italicized names need to be assigned to judge the story (i.e., they have not yet been assigned to judge it)."
        ),
        <.table(c"table table-striped")(
          <.thead(<.tr(<.th(thMod)("Story"), <.th(thMod)("People remaining not to spoil"))),
          <.tbody(
            mapWithMissingElements
              .toVector
              .sortBy(_._2.size)
              .toVdomArray { case (sourceMaterialId, unfinishedPeople) =>
                <.tr(
                  <.td(sourceMaterialId.title),
                  <.td(
                    Utils
                      .delimitedSpans(unfinishedPeople.toVector.filterNot(_._2).map(_._1).sorted)
                      .toVdomArray,
                    ", ".when(
                      unfinishedPeople.toVector.filter(_._2).nonEmpty &&
                        unfinishedPeople.toVector.filterNot(_._2).nonEmpty
                    ),
                    <.i(
                      Utils
                        .delimitedSpans(unfinishedPeople.toVector.filter(_._2).map(_._1).sorted)
                        .toVdomArray
                    )
                  )
                )
              }
          )
        ),
        <.h4(c"card-title")("Excludes"),
        SetConfig
          .String
          .nice(lobby.profiles.keySet, items = excludes, minItems = 0) {
            case SetConfig.Context(person) =>
              <.div(c"p-2", S.row)(<.span(person))
          }
      )
    }
}
