package evolution.app.react.component.presentational

import japgolly.scalajs.react
import japgolly.scalajs.react.{ Callback, PropsChildren }
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object Sidebar {
  type Expanded = Boolean
  class Backend(bs: BackendScope[Expanded, Unit]) {
    def render(expanded: Expanded, children: PropsChildren): VdomElement = {
      <.div(
        ^.classSet(
          "sidebar" -> true,
          "expanded" -> expanded,
          "column" -> true,
          "is-one-third" -> true
        ),
        children
      )
    }
  }

  val component =
    react.ScalaComponent.builder[Expanded]("sidebar").renderBackendWithChildren[Backend].build
}
