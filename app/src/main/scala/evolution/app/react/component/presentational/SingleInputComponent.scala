package evolution.app.react.component.presentational

import evolution.app.react.component.presentational.SingleInputComponent.Props
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.{TagOf, VdomElement}
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}
import org.scalajs.dom.html.Input

import scala.util.Try

object SingleInputComponent {

  case class Props[T](value: T, onChange: T => Callback, parser: String => T)

  class Backend[T](bs: BackendScope[Props[T], String]) {
    def render(props: Props[T], uiValue: String): VdomElement = {
      <.input(
        ^.`type` := "text",
        ^.className := "input is-small",
        ^.value := uiValue,
        ^.onChange ==> onChange(props)
      )
    }

    def onChange(props: Props[T])(e: ReactEventFromInput): Callback = {
      val newVal = e.target.value
      val parser = safeParser(props) _
      props.onChange(parser(newVal)) >> bs.setState(newVal)
    }

    private def safeParser(props: Props[T])(value: String): T = {
      Try(props.parser(value)).getOrElse(props.value)
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