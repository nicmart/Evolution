package evolution.app.react.component.presentational

import japgolly.scalajs.react
import japgolly.scalajs.react.{Callback, ReactEventFromInput}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object Select {

  final case class Item[T](label: String, key: String, value: T)

  case class Props[T](
    items: List[Item[T]],
    current: Item[T],
    onChange: Item[T] => Callback
  )

  class Backend[T](bs: BackendScope[Props[T], Unit]) {
    def render(props: Props[T]): VdomElement = {
      val options = props.items.map { item =>
        <.option(^.value := item.key, item.label)
      }
      <.select(
        options.toTagMod,
        ^.className := "select",
        ^.onChange ==> onChange(props),
        ^.value := props.current.key
      )
    }

    def onChange(props: Props[T])(e: ReactEventFromInput): Callback = {
      val value = e.target.value
      val optItem = props.items.find(_.key == value)
      optItem.fold(Callback.empty)(props.onChange)
    }
  }

  /**
    * WARNING:
    * Don't use this method directly inside render methods, but first fix its value
    * outside, and then use it.
    */
  def component[T] =
    react.ScalaComponent.builder[Props[T]]("dropdown")
      .renderBackend[Backend[T]]
      .build
}
