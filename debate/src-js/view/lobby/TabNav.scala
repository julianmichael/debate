package debate
package view.lobby

import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses

import debate.Utils.ClassSetInterpolator
import debate.util.Local

object TabNav {

  case class TabInfo(
    content: VdomElement,
    enabled: Boolean = true,
    badge: Option[VdomElement] = None
  )
  object TabInfo

  def tab(content: VdomElement) = TabInfo(content)

  def tabWithBadge(badge: VdomElement)(content: VdomElement) = TabInfo(content, badge = Some(badge))

  def tabWithNotifications(numNotifications: Int, mod: TagMod = c"badge-danger")(
    content: VdomElement
  ) = TabInfo(
    content,
    badge = Option(numNotifications)
      .filter(_ > 0)
      .map { numNotifs =>
        <.span(c"badge badge-pill", mod)(
          ^.marginLeft  := "0.5rem",
          ^.marginRight := "-0.5rem",
          numNotifs
        )
      }
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
                          tab,
                          tabInfo.badge
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
