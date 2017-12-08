package evolution.app.react.component.presentational

import evolution.app.react.component.presentational.SingleInput.Props
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}

import scala.util.Try

object SingleInput {

  case class Props[T](value: T, onChange: T => Callback)

  class Backend[T](parser: String => T)(bs: BackendScope[Props[T], String]) {
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
      Try(parser(value)).getOrElse(props.value)
    }
  }

  def component[T](parser: String => T) = ScalaComponent.builder[Props[T]]("Single input")
    .initialStateFromProps(_.value.toString)
    .backend(s => new Backend(parser)(s))
    .render(s => s.backend.render(s.props, s.state))
    .build
}

object IntInputComponent {
  val component = SingleInput.component[Int](_.toInt)

  def apply(value: Int, onChange: Int => Callback): VdomElement =
    component(Props(value, onChange))
}

object DoubleInputComponent {
  val component = SingleInput.component[Double](_.toDouble)

  def apply(value: Double, onChange: Double => Callback): VdomElement =
    component(Props(value, onChange))
}