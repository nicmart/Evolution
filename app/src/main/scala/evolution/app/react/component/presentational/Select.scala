package evolution.app.react.component.presentational

import evolution.app.data.PointedSeq
import japgolly.scalajs.react
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ CtorType, ReactEventFromInput }

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
        ^.onChange ==> ((ts: PointedSeq[Item[T]]) => props.setState(ts)).compose[ReactEventFromInput](e =>
          props.value.selectByPredicate(_.key == e.target.value)),
        ^.value := props.value.selected.key
      )
    }
  }

  /**
   * WARNING:
   * Don't use this method directly inside render methods, but first fix its value
   * outside, and then use it.
   */
  def component[T]: ReactComponent[T] =
    react.ScalaComponent.builder[StateSnapshot[PointedSeq[Item[T]]]]("dropdown").renderBackend[Backend[T]].build
}
