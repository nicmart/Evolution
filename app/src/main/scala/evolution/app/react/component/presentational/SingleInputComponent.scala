package evolution.app.react.component.presentational

import evolution.app.react.component.presentational.SingleInputComponent.Props
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}

object SingleInputComponent {

  case class Props[T](value: T, onChange: T => Callback, parser: String => T)

  class Backend[T](bs: BackendScope[Props[T], String]) {
    def render(props: Props[T], uiValue: String) = {
      <.div(
        ^.className := "field",
        <.div(
          ^.className := "control",
          <.input(
            ^.`type` := "number",
            ^.className := "input is-small",
            ^.value := uiValue,
            ^.onChange ==> onChange(props)
          )
        )
      )
    }

    def onChange(props: Props[T])(e: ReactEventFromInput): Callback = {
      val newVal = e.target.value
      props.onChange(props.parser(newVal)) >> bs.setState(newVal)
    }
  }

  def component[T] = ScalaComponent.builder[Props[T]]("Single input")
      .initialStateFromProps(_.value.toString)
      .renderBackend[Backend[T]]
      .build
}

object IntInputComponent {
  val component = SingleInputComponent.component[Int]
  def apply(value: Int, onChange: Int => Callback): VdomElement =
    component(Props(value, onChange, _.toInt))
}

object DoubleInputComponent {
  val component = SingleInputComponent.component[Double]
  def apply(value: Double, onChange: Double => Callback): VdomElement =
    component(Props(value, onChange, _.toDouble))
}