package evolution.app.react.component

import evolution.app.model.{Drawing, DrawingList}
import evolution.app.portfolio.EvolutionPortfolio
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import paint.geometry.Geometry.Point

object PageComponent {
    case class State(drawingList: DrawingList[Point])

    class Backend(bs: BackendScope[Unit, State]) {
        def render(state: State): VdomElement = {
            println(state.drawingList.selected)
            <.div(
                NavbarComponent.component(NavbarComponent.Props(
                    DrawingListComponent.component(
                        DrawingListComponent.Props(
                            state.drawingList,
                            onDrawingSelected
                        )
                    )
                )),
                <.canvas(^.id := "canvas")
            )
        }

        def onDrawingSelected(drawing: Drawing[Point]): Callback = {
            bs.modState { state =>
                state.copy(drawingList = state.drawingList.withSelected(drawing.name))
            }
        }
    }

    val component = ScalaComponent.builder[Unit]("Page")
        .initialState(State(EvolutionPortfolio.drawingList))
        .renderBackend[Backend]
        .build
}
