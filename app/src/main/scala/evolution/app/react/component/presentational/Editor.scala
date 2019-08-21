package evolution.app.react.component.presentational

import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.Callback

object Editor {
  case class Props(code: StateSnapshot[String], error: Option[String])

  class Backend {
    def render(props: Props): VdomElement = {

      <.div(
        ^.className := "dsl-config",
        CodeMirror.component(
          CodeMirror.props(
            value = props.code.value,
            onChange = (_, _, _) => Callback.empty,
            onBeforeChange = (_, _, code) => props.code.setState(code)
          )
        )(),
        <.div(
          ^.classSet(
            "dsl-feedback" -> true,
            "dsl-error" -> props.error.isDefined
          ),
          <.span(props.error.getOrElse("").toString)
        )
      )
    }
  }

  val component =
    ScalaComponent.builder[Props]("Editor").stateless.renderBackend[Backend].build
}
