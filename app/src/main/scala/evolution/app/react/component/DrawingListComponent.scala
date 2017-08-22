package evolution.app.react.component

import evolution.app.model.{ConfiguredDrawing, DrawingDefinitionList, DrawingListWithSelection}
import evolution.app.portfolio.DrawingPortfolio.DrawingDefinition
import evolution.app.react.component.presentational.SelectComponent
import evolution.app.react.component.presentational.SelectComponent.Item
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import paint.geometry.Geometry.Point
import japgolly.scalajs.react._

object DrawingListComponent {

  case class Props(
    drawingList: List[DrawingDefinition],
    current: DrawingDefinition,
    onSelect: DrawingDefinition => Callback
  )

  def selectProps(props: Props): SelectComponent.Props[DrawingDefinition] = {
    val items = props.drawingList.map { definition =>
      Item(definition.name, definition.name, definition)
    }
    val current = Item(props.current.name, props.current.name, props.current)
    def onChange(item: Item[DrawingDefinition]): Callback = {
      props.onSelect(item.value)
    }

    SelectComponent.Props(items, current, onChange)
  }


  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props): VdomElement = {
      SelectComponent(
        selectProps(props)
      )
    }
  }

  val component = ScalaComponent.builder[Props]("Example")
      .renderBackend[Backend]
      .build
}
