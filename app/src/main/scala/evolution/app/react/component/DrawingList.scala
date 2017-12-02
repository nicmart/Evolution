package evolution.app.react.component

import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.presentational.Select
import evolution.app.react.component.presentational.Select.Item
import evolution.geometry.Point
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object DrawingList {

  case class Props[T](
    drawingList: List[DrawingDefinition[T]],
    current: DrawingDefinition[T],
    onSelect: DrawingDefinition[T] => Callback
  )

  def selectProps[T](props: Props[T]): Select.Props[DrawingDefinition[T]] = {
    val items = props.drawingList.map { definition =>
      Item(definition.name, definition.name, definition)
    }
    val current = Item(props.current.name, props.current.name, props.current)
    def onChange(item: Item[DrawingDefinition[T]]): Callback = {
      props.onSelect(item.value)
    }

    Select.Props(items, current, onChange)
  }

  class Backend[T](selectComponent: Select.ReactComponent[DrawingDefinition[T]])(bs: BackendScope[Props[T], Unit]) {
    def render(props: Props[T]): VdomElement = {
      selectComponent(
        selectProps(props)
      )
    }
  }

  def component[T] = {
    ScalaComponent.builder[Props[T]]("Example")
      .backend[Backend[T]](scope => new Backend[T](Select.component[DrawingDefinition[T]])(scope))
      .render(scope => scope.backend.render(scope.props))
      .build
  }
}
