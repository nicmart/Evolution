package evolution.app.react.component.control

import evolution.app.react.component.presentational.Button
import japgolly.scalajs.react
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

object PlayToggle {
  type IsPlaying = Boolean

  val component = react.ScalaComponent
    .builder[StateSnapshot[IsPlaying]]("play toggle")
    .stateless
    .render_P { s =>
      val className = if (s.value) "fa-pause" else "fa-play"
      Button.component(s.modState(!_)) {
        <.span(
          ^.className := "icon",
          <.i(^.className := s"fa $className")
        )
      }
    }
    .build
}
