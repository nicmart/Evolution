package evolution.app.react.component

import evolution.app.model.{ConfiguredDrawing, DrawingDefinitionList, DrawingListWithSelection}
import evolution.app.portfolio.DrawingPortfolio.DrawingDefinition
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import paint.geometry.Geometry.Point
import japgolly.scalajs.react._

object DrawingListComponent {

  case class Props(
    drawingList: DrawingListWithSelection,
    onSelect: DrawingDefinition => Callback
  ) {
    def currentDrawing: DrawingDefinition = drawingList.current

    def list: DrawingDefinitionList = drawingList.list
  }

  class Backend(bs: BackendScope[Props, Unit]) {
    def render(props: Props): VdomElement = {
      import japgolly.scalajs.react.vdom.html_<^._
      val drawingList = props.list
      val options = drawingList.drawings.map { drawing =>
        <.option(drawing.name)
      }
      <.select(
        options.toTagMod,
        ^.onChange ==> onNewSelection(props),
        ^.value := props.currentDrawing.name
      )
    }

    def onNewSelection(props: Props)(e: ReactEventFromInput): Callback =
      props.onSelect(props.list.drawing(e.target.value).get)
  }

  val component = ScalaComponent.builder[Props]("Example")
      .renderBackend[Backend]
      .build
}
