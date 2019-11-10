package evolution.app.react.component.presentational

import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}

object TextArea {
  class Backend {
    def render(props: StateSnapshot[String]): VdomElement = {
      <.div(
        ^.className := "code-wrapper",
        <.textarea(
          ^.value := props.value,
          ^.spellCheck := "false",
          ^.className := "code-area",
          ^.onChange ==> onChange(props),
          props.value
        )
      )
    }

    def onChange(props: StateSnapshot[String])(e: ReactEventFromInput): Callback = {
      props.setState(e.target.value)
    }
  }

  val component =
    ScalaComponent.builder[StateSnapshot[String]]("Single input").stateless.renderBackend[Backend].build
}
