package evolution.app.react.component.control

import evolution.app.react.component.presentational.Button
import japgolly.scalajs.react
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^._

object PlayToggle {
  case class Props(isPlaying: Boolean, onChange: Boolean => Callback)

  val component = react.ScalaComponent
    .builder[Props]("play toggle")
    .stateless
    .render_P { props =>
      val className = if (props.isPlaying) "fa-pause" else "fa-play"
      Button.component(props.onChange(!props.isPlaying)) {
        <.span(
          ^.className := "icon",
          <.i(^.className := s"fa $className")
        )
      }
    }
    .build
}
