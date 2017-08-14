package evolution.app.react.component

import evolution.app.react.component.NavbarComponent.Props
import evolution.app.react.component.PageComponent.{Backend, initialState}
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._

object NumericInputComponent {
    case class Props(value: Int, onChange: Int => Callback)
    class Backend(bs: BackendScope[Props, Unit]) {
        def render(props: Props) = {
            <.div(
                ^.className := "field",
                <.div(
                    ^.className := "control",
                    <.input(
                        ^.`type` := "number",
                        ^.className := "input",
                        ^.value := props.value,
                        ^.onChange ==> onChange(props)
                    )
                )
            )
        }

        def onChange(props: Props)(e: ReactEventFromInput): Callback =
            props.onChange(e.target.value.toInt)
    }

    val component = ScalaComponent.builder[Props]("Numeric Input")
        .renderBackend[Backend]
        .build
}

object DoubleInputComponent {
    case class Props(value: Double, onChange: Double => Callback)
    class Backend(bs: BackendScope[Props, Unit]) {
        def render(props: Props) = {
            <.div(
                ^.className := "field",
                <.div(
                    ^.className := "control",
                    <.input(
                        ^.`type` := "number",
                        ^.className := "input",
                        ^.value := props.value,
                        ^.onChange ==> onChange(props)
                    )
                )
            )
        }

        def onChange(props: Props)(e: ReactEventFromInput): Callback =
            props.onChange(e.target.value.toDouble)
    }

    val component = ScalaComponent.builder[Props]("Numeric Input")
      .renderBackend[Backend]
      .build
}