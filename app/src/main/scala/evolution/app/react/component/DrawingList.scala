package evolution.app.react.component

import evolution.app.data.PointedSeq
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.presentational.Select
import evolution.app.react.component.presentational.Select.Item
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._

object DrawingList {

  type Props[T] = StateSnapshot[PointedSeq[DrawingDefinition[T]]]

  def selectProps[T](props: Props[T]): Select.Props[DrawingDefinition[T]] = {
    props.xmapState(definitions => definitions.map(definitionToItem))(items => items.map(_.value))
  }

  private def definitionToItem[T](definition: DrawingDefinition[T]): Item[DrawingDefinition[T]] =
    Item(definition.name, definition.name, definition)

  class Backend[T](selectComponent: Select.ReactComponent[DrawingDefinition[T]]) {
    def render(props: Props[T]): VdomElement = {
      selectComponent(
        selectProps(props)
      )
    }
  }

  def component[T] = {
    ScalaComponent
      .builder[Props[T]]("Example")
      .backend[Backend[T]](_ => new Backend[T](Select.component[DrawingDefinition[T]]))
      .render(scope => scope.backend.render(scope.props))
      .build
  }
}
