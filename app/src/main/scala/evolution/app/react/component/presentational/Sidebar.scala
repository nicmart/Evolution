package evolution.app.react.component.presentational

import japgolly.scalajs.react
import japgolly.scalajs.react.PropsChildren
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object Sidebar {

  class Backend(bs: BackendScope[Unit, Unit]) {
    def render(children: PropsChildren): VdomElement = {
      <.div(
        ^.classSet(
          "sidebar" -> true
        ),
        children
      )
    }
  }

  val component = react.ScalaComponent
    .builder[Unit]("sidebar")
    .renderBackendWithChildren[Backend]
    .build
}
