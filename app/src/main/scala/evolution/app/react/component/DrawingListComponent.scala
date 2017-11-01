package evolution.app.react.component

import evolution.app.model.definition.{AbstractDrawingDefinition, DrawingDefinition}
import evolution.app.react.component.presentational.SelectComponent
import evolution.app.react.component.presentational.SelectComponent.Item
import evolution.geometry.Point
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object DrawingListComponent {

  private val selectComponent = SelectComponent.component[DrawingDefinition[Point]]

  case class Props(
    drawingList: List[DrawingDefinition[Point]],
    current: DrawingDefinition[Point],
    onSelect: DrawingDefinition[Point] => Callback
  )

  def selectProps(props: Props): SelectComponent.Props[DrawingDefinition[Point]] = {
    val items = props.drawingList.map { definition =>
      Item(definition.name, definition.name, definition)
    }
    val current = Item(props.current.name, props.current.name, props.current)
    def onChange(item: Item[DrawingDefinition[Point]]): Callback = {
      props.onSelect(item.value)
    }

    SelectComponent.Props(items, current, onChange)
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
