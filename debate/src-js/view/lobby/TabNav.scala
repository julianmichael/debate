package debate
package view.lobby

import io.circe.Decoder
import io.circe.Encoder
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses

import debate.Utils.ClassSetInterpolator
import debate.util.LocalState2

class TabNav[A: Encoder: Decoder] {

  val LocalTab = new LocalState2[A]

  @Lenses
  case class Props(
    key: String,
    allTabs: Vector[A],
    initialTab: A,
    notifications: Map[A, Int],
    render: StateSnapshot[A] => VdomElement
  )
  val S = Styles
  val V = new jjm.ui.View(S)

  def make(key: String, allTabs: Vector[A], initialTab: A, notifications: Map[A, Int] = Map())(
    render: StateSnapshot[A] => VdomElement
  ) = Component(Props(key, allTabs, initialTab, notifications, render))

  val Component =
    ScalaComponent
      .builder[Props]("Tab Nav")
      .render_P { props =>
        LocalTab.syncedWithSessionStorage(props.key, props.initialTab) { tabState =>
          ReactFragment(
            <.div(c"card-header")(
              <.ul(c"nav nav-fill nav-tabs card-header-tabs")(
                props
                  .allTabs
                  .toVdomArray(tab =>
                    <.li(c"nav-item")(
                      ^.key := tab.toString,
                      <.a(^.classSet1("nav-link", "active" -> (tab == tabState.value)))(
                        ^.href := "#",
                        ^.onClick --> tabState.setState(tab),
                        tab.toString,
                        props
                          .notifications
                          .get(tab)
                          .filter(_ > 0)
                          .map { numNotifs =>
                            <.span(c"badge badge-danger badge-pill")(
                              ^.marginLeft  := "0.5rem",
                              ^.marginRight := "-0.5rem",
                              numNotifs
                            )
                          }
                      )
                    )
                  )
              )
            ),
            props.render(tabState)
          )
        }
      }
      .build

}
