package evolution.app.react.component.presentational

import evolution.app.data.PointedSeq
import japgolly.scalajs.react
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CtorType, ReactEventFromInput}
import japgolly.scalajs.react.Callback

object Select {

  type Props[T] = StateSnapshot[PointedSeq[Item[T]]]
  type ReactComponent[T] = Component[Props[T], Unit, Backend[T], CtorType.Props]

  final case class Item[T](label: String, key: String, value: T)

  class Backend[T] {
    def render(props: Props[T]): VdomElement = {
      val options = props.value.elements.map { item =>
        <.option(^.value := item.key, item.label)
      }
      <.select(
        options.toTagMod,
        ^.className := "select",
        ^.onChange ==> onChange(props),
        ^.value := props.value.selected.key
      )
    }

    private def onChange[T](props: Props[T])(e: ReactEventFromInput): Callback =
      props.setState(props.value.selectByPredicate(_.key == e.target.value))
  }

  /**
    * WARNING:
    * Don't use this method directly inside render methods, but first fix its value
    * outside, and then use it.
    */
  def component[T]: ReactComponent[T] =
    react.ScalaComponent.builder[StateSnapshot[PointedSeq[Item[T]]]]("dropdown").renderBackend[Backend[T]].build
}
