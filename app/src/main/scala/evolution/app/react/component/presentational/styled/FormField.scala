package evolution.app.react.component.presentational.styled

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object FormField {

  case class Props(label: String, className: String, element: VdomElement)

  val component =
    ScalaComponent.builder[Props]("Styled Input")
      .render_P { props =>
        <.div(
          ^.className := "field",
          <.label(^.className := "label is-small", props.label),
          <.div(
            ^.className := "control",
            props.element
          )
        )
      }
      .build
}
