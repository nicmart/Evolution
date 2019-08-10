package evolution.app.react.component

import evolution.app.data.PointedSeq
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.presentational.Select
import evolution.app.react.component.presentational.Select.Item
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

// TODO this is not used
object DrawingList {

  type Props = StateSnapshot[PointedSeq[DrawingDefinition]]

  def selectProps[T](props: Props): Select.Props[DrawingDefinition] = {
    props.xmapState(definitions => definitions.map(definitionToItem))(items => items.map(_.value))
  }

  private def definitionToItem[T](definition: DrawingDefinition): Item[DrawingDefinition] =
    Item(definition.name, definition.name, definition)

  class Backend[T](selectComponent: Select.ReactComponent[DrawingDefinition]) {
    def render(props: Props): VdomElement = {
      selectComponent(
        selectProps(props)
      )
    }
  }

  def component[T] = {
    ScalaComponent
      .builder[Props]("Example")
      .backend[Backend[T]](_ => new Backend[T](Select.component[DrawingDefinition]))
      .render(scope => scope.backend.render(scope.props))
      .build
  }
}
