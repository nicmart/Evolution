package evolution.app.react.component.presentational

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}

class SingleInputComponent[T] {
    case class Props(value: T, onChange: T => Callback, parser: String => T)
    class Backend(bs: BackendScope[Props, Unit]) {
        def render(props: Props) = {
            <.div(
                ^.className := "field",
                <.div(
                    ^.className := "control",
                    <.input(
                        ^.`type` := "text",
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

    def apply(value: T, onChange: T => Callback, parser: String => T): VdomElement =
        component(Props(value, onChange, parser))
}

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

    def apply(value: Int, onChange: Int => Callback): VdomElement =
        component(Props(value, onChange))
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
                        ^.`type` := "text",
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

    def apply(value: Double, onChange: Double => Callback): VdomElement =
        component(Props(value, onChange))
}