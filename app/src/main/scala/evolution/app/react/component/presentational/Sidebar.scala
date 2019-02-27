package evolution.app.react.component.presentational

import japgolly.scalajs.react.{ Callback, PropsChildren }
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react
import org.scalajs.dom
import org.scalajs.dom.raw.MouseEvent

object Sidebar {
  type Expanded = Boolean
  type IsDragging = Boolean
  case class State(isDragging: Boolean, sidebarX: Option[Double])
  object State {
    val empty: State = State(false, None)
  }
  class Backend(bs: BackendScope[Expanded, State]) {
    def windowWidth: Int = dom.document.documentElement.clientWidth
    def render(expanded: Expanded, state: State, children: PropsChildren): VdomElement = {
      <.div(
        ^.classSet(
          "sidebar" -> true,
          "expanded" -> expanded,
          "column" -> false // TODO remove this?
        ),
        ^.width := state.sidebarX.fold(windowWidth.toDouble / 3)(windowWidth - _).toString,
        <.div(
          ^.id := "sidebar-handle",
          ^.onMouseDown --> bs.modState(_.copy(isDragging = true))
        ),
        children
      )
    }

    def onMouseDrag(e: MouseEvent): Callback =
      bs.state.flatMap { state =>
        if (state.isDragging) {
          bs.modState(_.copy(sidebarX = Some(e.clientX)))
        } else Callback.empty
      }
  }

  val component =
    react.ScalaComponent
      .builder[Expanded]("sidebar")
      .initialState(State.empty)
      .renderBackendWithChildren[Backend]
      .componentDidMount { s =>
        Callback(dom.window.onmousemove = (e: MouseEvent) => s.backend.onMouseDrag(e).runNow()) >>
          Callback(dom.window.onmouseup = (e: MouseEvent) => s.modState(_.copy(isDragging = false)).runNow())
      }
      .build
}
