package evolution.app.react.component.presentational

import japgolly.scalajs.react
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object SidebarComponent {

  case class Props(
    active: Boolean,
    content: VdomElement
  )

  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props): VdomElement = {
      <.div(
        ^.classSet(
          "is-hidden" -> !props.active,
          "sidebar" -> true
        ),
        props.content
      )
    }
  }

  val component = react.ScalaComponent
    .builder[Props]("sidebar")
    .renderBackend[Backend]
    .build
}
