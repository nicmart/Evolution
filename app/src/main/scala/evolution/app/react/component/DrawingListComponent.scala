package evolution.app.react.component

import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.presentational.SelectComponent
import evolution.app.react.component.presentational.SelectComponent.Item
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object DrawingListComponent {

  private val selectComponent = SelectComponent.component[DrawingDefinition]

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
