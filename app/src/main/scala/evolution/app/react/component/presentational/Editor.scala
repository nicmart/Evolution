package evolution.app.react.component.presentational

import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.ScalaComponent
import evolution.app.react.component.config.{ ConfigComponent }

object Editor {
  case class Props(code: StateSnapshot[String], error: Option[String])

  class Backend {
    def render(props: Props): VdomElement = {

      val component: ConfigComponent[String] = ConfigComponent.textConfig

      <.div(
        ^.className := "dsl-config",
        component.apply(props.code)(),
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
