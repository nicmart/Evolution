package evolution.app.react.component

import evolution.app.model.{Drawing, DrawingList}
import evolution.app.portfolio.EvolutionPortfolio
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import paint.geometry.Geometry.Point
import japgolly.scalajs.react._

object DrawingListComponent {
    type State = DrawingList[Point]
    class Backend(bs: BackendScope[Unit, State]) {
        def render(drawingList: State): VdomElement = {
            import japgolly.scalajs.react.vdom.all._
            val options = drawingList.drawings.values.map { drawing =>
                option(drawing.name)
            }
            val dropdown =
                select(
                    options.toSeq: _*
                ).apply(onChange ==> onNewSelection(drawingList))

            drawingList.selected match {
                case None => dropdown
                case Some(drawing) => dropdown(value := drawing.name)
            }
        }

        def onNewSelection(drawingList: State)(e: ReactEventFromInput): Callback = {
            val selected = e.target.value
            bs.setState(drawingList.withSelected(selected))
        }
    }

    val component = ScalaComponent.builder[Unit]("Example")
        .initialState(EvolutionPortfolio.drawingList)
        .renderBackend[Backend]
        .build
}
