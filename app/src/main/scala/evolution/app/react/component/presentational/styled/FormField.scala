package evolution.app.react.component.presentational.styled

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._

object FormField {

  case class Props(label: String, className: String = "")

  val component =
    ScalaComponent
      .builder[Props]("Styled Input")
      .render_PC { (props, children) =>
        <.div(
          ^.className := "field",
          <.label(^.className := "label is-small", props.label),
          <.div(
            ^.className := "control",
            children
          )
        )
      }
      .build
}
