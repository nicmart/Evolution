package evolution.app.react.component.presentational

import japgolly.scalajs.react.{ Callback, PropsChildren, ReactMouseEvent }
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react
import japgolly.scalajs.react.extra.StateSnapshot
import org.scalajs.dom
import org.scalajs.dom.raw.MouseEvent

object Sidebar {
  case class Props(expanded: Boolean, width: StateSnapshot[Double])

  case class State(draggingState: Option[DraggingStart]) {
    def isDragging: Boolean = draggingState.isDefined
  }

  case class DraggingStart(startX: Double, oldWidth: Double)

  object State {
    val empty: State = State(None)
  }
  class Backend(bs: BackendScope[Props, State]) {
    def render(props: Props, state: State, children: PropsChildren): VdomElement = {
      <.div(
        ^.classSet(
          "sidebar" -> true,
          "expanded" -> props.expanded,
          "column" -> false // TODO remove this?
        ),
        ^.width := props.width.value.toString,
        <.div(
          ^.id := "sidebar-handle",
          ^.onMouseDown ==> (e => bs.setState(State(Some(DraggingStart(e.clientX, props.width.value))))),
          <.div()
        ),
        children
      )
    }

    def onMouseDrag(e: MouseEvent): Callback =
      for {
        state <- bs.state
        props <- bs.props
        _ <- state.draggingState.fold(Callback.empty)(start =>
          props.width.setState(start.oldWidth + start.startX - e.clientX))
      } yield ()
  }

  val component =
    react.ScalaComponent
      .builder[Props]("sidebar")
      .initialState(State.empty)
      .renderBackendWithChildren[Backend]
      .componentDidMount { s =>
        Callback(dom.window.onmousemove = (e: MouseEvent) => s.backend.onMouseDrag(e).runNow()) >>
          Callback(dom.window.onmouseup = (e: MouseEvent) => s.setState(State.empty).runNow())
      }
      .build
}
