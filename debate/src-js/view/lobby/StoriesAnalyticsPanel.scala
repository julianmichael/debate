package debate
package view.lobby

import japgolly.scalajs.react.vdom.html_<^._
// import japgolly.scalajs.react.Callback
import debate.util.SetConfig
import scalacss.ScalaCssReact._
import debate.util.Local

import cats.implicits._

object StoriesAnalyticsPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  def apply(lobby: Lobby) =
    Local[Set[String]].syncedWithLocalStorage("story-reader-excludes", Set.empty) { excludes =>
      val thMod = c"text-left"
      val initialMap = lobby
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
                  Set(person)
                else
                  Set.empty[String]
              Map(sourceMaterialId -> personIfUnfinished)
            }
        }
      val mapWithMissingElements = initialMap.map { case (story, people) =>
        story ->
          ((people ++ lobby.storyRecord.filter(p => !p._2.contains(story)).keySet) --
            excludes.value)
      }

      <.div(
        <.table(c"table table-striped")(
          <.thead(<.tr(<.th(thMod)("Story"), <.th(thMod)("People remaining not to spoil"))),
          <.tbody(
            mapWithMissingElements
              .toVector
              .sortBy(_._2.size)
              .toVdomArray { case (sourceMaterialId, unfinishedPeople) =>
                <.tr(
                  <.td(sourceMaterialId.title),
                  <.td(Utils.delimitedSpans(unfinishedPeople.toVector.sorted).toVdomArray)
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
