package evolution.app.react.component.presentational

import japgolly.scalajs.react.{ Callback, PropsChildren }
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react
import japgolly.scalajs.react.extra.StateSnapshot
import org.scalajs.dom
import org.scalajs.dom.raw.MouseEvent

object Sidebar {
  case class Props(expanded: Boolean, sidebarX: StateSnapshot[Double])
  case class State(isDragging: Boolean)
  object State {
    val empty: State = State(false)
  }
  class Backend(bs: BackendScope[Props, State]) {
    def windowWidth: Int = dom.document.documentElement.clientWidth
    def render(props: Props, state: State, children: PropsChildren): VdomElement = {
      <.div(
        ^.classSet(
          "sidebar" -> true,
          "expanded" -> props.expanded,
          "column" -> false // TODO remove this?
        ),
        ^.width := (windowWidth - props.sidebarX.value).toString,
        <.div(
          ^.id := "sidebar-handle",
          ^.onMouseDown --> bs.modState(_.copy(isDragging = true))
        ),
        children
      )
    }

    def onMouseDrag(e: MouseEvent): Callback =
      for {
        state <- bs.state
        props <- bs.props
        _ <- if (state.isDragging) props.sidebarX.setState(e.clientX) else Callback.empty
      } yield ()
  }

  val component =
    react.ScalaComponent
      .builder[Props]("sidebar")
      .initialState(State.empty)
      .renderBackendWithChildren[Backend]
      .componentDidMount { s =>
        Callback(dom.window.onmousemove = (e: MouseEvent) => s.backend.onMouseDrag(e).runNow()) >>
          Callback(dom.window.onmouseup = (e: MouseEvent) => s.modState(_.copy(isDragging = false)).runNow())
      }
      .build
}
