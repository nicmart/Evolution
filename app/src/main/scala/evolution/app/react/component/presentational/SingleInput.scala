package evolution.app.react.component.presentational

import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ReactEventFromInput, ScalaComponent}

import scala.util.Try

object SingleInput:

  class Backend[T](parser: String => T)(bs: BackendScope[StateSnapshot[T], Option[String]]):
    def render(props: StateSnapshot[T], uiValue: Option[String]): VdomElement =
      <.input(
        ^.`type` := "text",
        ^.className := "input is-small",
        ^.value := uiValue.getOrElse(props.value.toString),
        ^.onChange ==> onChange(props)
      )

    def onChange(props: StateSnapshot[T])(e: ReactEventFromInput): Callback =
      val newVal = e.target.value
      val parsedResult = Try(parser(newVal))
      parsedResult.fold(
        _ => bs.setState(Some(newVal)),
        t => props.setState(t) >> bs.setState(None)
      )

  def component[T](parser: String => T) =
    ScalaComponent
      .builder[StateSnapshot[T]]("Single input")
      .initialStateFromProps[Option[String]](_ => None)
      .backend(s => Backend(parser)(s))
      .render(s => s.backend.render(s.props, s.state))
      .build

object IntInputComponent:
  val component = SingleInput.component[Int](_.toInt)

object DoubleInputComponent:
  val component = SingleInput.component[Double](_.toDouble)

object StringInputComponent:
  val component = SingleInput.component[String](identity)
