package evolution.app.react.component.presentational

import evolution.app.react.component.DrawingListComponent.Props
import japgolly.scalajs.react
import japgolly.scalajs.react.{Children, PropsChildren}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object SidebarComponent {
    case class Props(active: Boolean, width: Int)

    class Backend(bs: BackendScope[Props, Unit]) {
        def render(props: Props, children: PropsChildren): VdomElement = {
            <.div(
                ^.classSet(
                    "is-hidden" -> !props.active,
                    "sidebar" -> true
                ),
                children
            )
        }
    }

    val component = react.ScalaComponent
        .builder[Props]("sidebar")
        .renderBackendWithChildren[Backend]
        .build
}
