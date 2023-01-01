package debate
package view.lobby

import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses

import debate.Utils.ClassSetInterpolator
import debate.util.LocalState2

object TabNav {

  val LocalInt = new LocalState2[Int]

  case class TabInfo(content: VdomElement, numNotifications: Int = 0)
  object TabInfo

  def tab(content: VdomElement) = TabInfo(content)

  def tabWithNotifications(numNotifications: Int)(content: VdomElement) = TabInfo(
    content,
    numNotifications
  )

  @Lenses
  case class Props(key: String, initialTabIndex: Int, tabs: Vector[(String, TabInfo)])
  val S = Styles
  val V = new jjm.ui.View(S)

  def apply(key: String, initialTabIndex: Int = 0)(tabs: (String, TabInfo)*) = Component(
    Props(key, initialTabIndex, tabs.toVector)
  )

  val Component =
    ScalaComponent
      .builder[Props]("Tab Nav")
      .render_P { props =>
        LocalInt.syncedWithSessionStorage(props.key, props.initialTabIndex) { tabIndex =>
          ReactFragment(
            <.div(c"card-header")(
              <.ul(c"nav nav-fill nav-tabs card-header-tabs")(
                props
                  .tabs
                  .zipWithIndex
                  .toVdomArray { case ((tab, tabInfo), index) =>
                    <.li(c"nav-item")(
                      ^.key := tab,
                      <.a(^.classSet1("nav-link", "active" -> (index == tabIndex.value)))(
                        ^.href := "#",
                        ^.onClick --> tabIndex.setState(index),
                        tab.toString,
                        Option(tabInfo.numNotifications)
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
                  }
              )
            ),
            props.tabs(tabIndex.value)._2.content
          )
        }
      }
      .build

}
