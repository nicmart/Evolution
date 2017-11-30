package evolution.app.react.component.presentational.styled

import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object HorizontalFormField {

  case class Props(label: String, className: String, element: VdomElement)

  val component =
    ScalaComponent.builder[Props]("Horizontal Input")
      .render_P { props =>
        <.div(
          ^.className := "field is-horizontal",
          <.div(
            ^.className := "field-label is-narrow",
            <.label(^.className := "label", props.label)
          ),
          <.div(
            ^.className := "field-body",
            <.div(
              ^.className := "field is-narrow",
              <.div(
                ^.className := "control",
                <.div(
                  ^.className := props.className,
                  props.element
                )
              )
            )
          )
        )
      }
      .build
}
