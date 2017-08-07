package evolution.app.react.component

import evolution.app.model.{Drawing, DrawingList}
import evolution.app.portfolio.EvolutionPortfolio
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import paint.geometry.Geometry.Point
import japgolly.scalajs.react._

object DrawingListComponent {
    type State = DrawingList[Point]
    case class Props(onSelect: Drawing[Point] => Unit)

    class Backend(bs: BackendScope[Props, State]) {
        def render(drawingList: State): VdomElement = {
            import japgolly.scalajs.react.vdom.html_<^._
            val options = drawingList.drawings.values.map { drawing =>
                <.option(drawing.name)
            }
            val dropdown =
                <.select(
                    options.toSeq: _*
                ).apply(^.onChange ==> onNewSelection(drawingList))

            drawingList.selected match {
                case None => dropdown
                case Some(drawing) => dropdown(^.value := drawing.name)
            }
        }

        def onNewSelection(drawingList: State)(e: ReactEventFromInput): Callback = {
            val selected = e.target.value
            val updateState = bs.modState(_.withSelected(selected))
            for {
                _ <- updateState
                state <- bs.state
                props <- bs.props
            } yield {
                props.onSelect(state.drawing(selected).get)
            }
        }
    }

    val component = ScalaComponent.builder[Props]("Example")
        .initialState(EvolutionPortfolio.drawingList)
        .renderBackend[Backend]
        .build
}
