package evolution.app.react.component.presentational.styled

import japgolly.scalajs.react.{CtorType, ScalaComponent}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

case class HorizontalFormField(
  label: String,
  className: String,
  element: VdomElement
) {
  def render = HorizontalFormField.component(this)
}

object HorizontalFormField {

  val component =
    ScalaComponent.builder[HorizontalFormField]("Horizontal Input")
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
