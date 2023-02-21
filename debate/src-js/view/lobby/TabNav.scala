package debate
package view.lobby

import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses

import debate.Utils.ClassSetInterpolator
import debate.util.Local

object TabNav {

  case class TabInfo(content: VdomElement, enabled: Boolean = true, numNotifications: Int = 0)
  object TabInfo

  def tab(content: VdomElement) = TabInfo(content)

  def tabWithNotifications(numNotifications: Int)(content: VdomElement) = TabInfo(
    content,
    numNotifications = numNotifications
  )

  def tabIf(enabled: Boolean)(content: VdomElement) = TabInfo(content, enabled = enabled)

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
        Local
          .named[Int](props.key)
          .syncedWithSessionStorage(props.key, props.initialTabIndex) { tabIndex =>
            ReactFragment(
              <.div(c"card-header")(
                <.ul(c"nav nav-fill nav-tabs card-header-tabs")(
                  props
                    .tabs
                    .zipWithIndex
                    .toVdomArray { case ((tab, tabInfo), index) =>
                      <.li(c"nav-item")(
                        ^.key := tab,
                        <.a(
                          ^.classSet1(
                            "nav-link",
                            "disabled" -> !tabInfo.enabled,
                            "active"   -> (index == tabIndex.value)
                          )
                        )(
                          ^.href := "#",
                          (^.onClick --> tabIndex.setState(index)).when(tabInfo.enabled),
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
              props
                .tabs
                .zipWithIndex
                .find(_._2 == tabIndex.value)
                .map(_._1._2)
                .filter(_.enabled)
                .map(_.content)
                .getOrElse(<.div("Tab nav error. Click a tab above to make things good again."))
            )
          }
      }
      .build

}
