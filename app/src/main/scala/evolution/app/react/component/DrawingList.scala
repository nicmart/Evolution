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

  private val selectComponent = Select.component[DrawingDefinition[Point]]

  case class Props(
    drawingList: List[DrawingDefinition[Point]],
    current: DrawingDefinition[Point],
    onSelect: DrawingDefinition[Point] => Callback
  )

  def selectProps(props: Props): Select.Props[DrawingDefinition[Point]] = {
    val items = props.drawingList.map { definition =>
      Item(definition.name, definition.name, definition)
    }
    val current = Item(props.current.name, props.current.name, props.current)
    def onChange(item: Item[DrawingDefinition[Point]]): Callback = {
      props.onSelect(item.value)
    }

    Select.Props(items, current, onChange)
  }

  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props): VdomElement = {
      selectComponent(
        selectProps(props)
      )
    }
  }

  val component =
    ScalaComponent.builder[Props]("Example")
      .renderBackend[Backend]
      .build
}
