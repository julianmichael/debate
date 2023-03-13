package debate
package view.lobby

// import japgolly.scalajs.react._
// import japgolly.scalajs.react.MonocleReact._
// import japgolly.scalajs.react.extra.StateSnapshot
// import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.feature.ReactFragment
// import monocle.function.{all => Optics}
// import scalacss.ScalaCssReact._

object DebateSchedulingPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

//   import Utils.ClassSetInterpolator

//   import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

//   val QuALITYIndexFetch    = new CacheCallContent[Unit, Map[String, String]]
//   val QuALITYStoryFetch = new CacheCallContent[String, QuALITYStory]
//   val QuALITYStorySelect =
//     new V.Select[(String, String)](
//       show =
//         choice =>
//           choice match {
//             case (articleId, title) =>
//               s"$title ($articleId)"
//           }
//     )

//   import App.ajaxService

  //   desiredWorkload <- SparseDistribution
  //     .fromMap(people.map(_ -> 1.0).toMap)
  //     .toRight("No people to schedule.")

  def apply(
    // lobby: Lobby,
    // qualityService: QuALITYService[AsyncCallback],
    // registerRuleConfig: RegisterRuleConfig => Callback,
    // joinDebate: Option[(Boolean, String) => Callback],
    // initDebate: CreateRoom => Callback
  ) = ReactFragment(<.h3("Auto-Schedule Debates"))

}
