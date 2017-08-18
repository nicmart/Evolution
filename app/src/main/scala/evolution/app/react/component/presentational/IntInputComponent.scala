package evolution.app.react.component.presentational

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}

object SingleInputComponent {

  case class Props[T](value: T, onChange: T => Callback, parser: String => T)

  class Backend[T](bs: BackendScope[Props[T], Unit]) {
    def render(props: Props[T]) = {
      <.div(
        ^.className := "field",
        <.div(
          ^.className := "control",
          <.input(
            ^.`type` := "text",
            ^.className := "input",
            ^.value := props.value.toString,
            ^.onChange ==> onChange(props)
          )
        )
      )
    }

    def onChange(props: Props[T])(e: ReactEventFromInput): Callback =
      props.onChange(props.parser(e.target.value))
  }

  def component[T] = ScalaComponent.builder[Props[T]]("Single input")
      .renderBackend[Backend[T]]
      .build

  def apply[T](value: T, onChange: T => Callback, parser: String => T): VdomElement =
    component[T](Props(value, onChange, parser))
}

object IntInputComponent {
  def apply(value: Int, onChange: Int => Callback): VdomElement =
    SingleInputComponent[Int](value, onChange, _.toInt)
}

object DoubleInputComponent {
  def apply(value: Double, onChange: Double => Callback): VdomElement =
    SingleInputComponent[Double](value, onChange, _.toDouble)
}