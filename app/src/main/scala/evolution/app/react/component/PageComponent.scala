package evolution.app.react.component

import evolution.app.canvas.EvolutionDrawer
import evolution.app.conf.Conf
import evolution.app.model.{Drawing, DrawingList}
import evolution.app.portfolio.EvolutionPortfolio
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Document, Window}
import paint.geometry.Geometry.Point
import org.scalajs.dom
import paint.evolution.Evolution
import paint.random.SimpleRNG

import scala.util.Random

object PageComponent {
    case class State(
        canvasInitializer: dom.html.Canvas => Unit,
        drawer: EvolutionDrawer,
        currentDrawing: Drawing[Point],
        drawingList: DrawingList[Point],
        size: Point,
        canvasVersion: Int = 0
    ) {
        def evolution: Evolution[Point] = currentDrawing.evolution(size)
    }

    class Backend(bs: BackendScope[Unit, State]) {
        // Create a mutable reference
        private val refToCanvas =
            ScalaComponent.mutableRefTo(CanvasComponent.component)

        def render(state: State): VdomElement = {
            <.div(
                NavbarComponent.component(NavbarComponent.Props(
                    DrawingListComponent.component(
                        DrawingListComponent.Props(
                            state.currentDrawing,
                            state.drawingList,
                            onDrawingSelected
                        )
                    )
                )),
                CanvasComponent.component.withKey(state.canvasVersion)(CanvasComponent.Props(
                    state.canvasInitializer,
                    state.currentDrawing,
                    state.drawer,
                    state.size
                ))
            )
        }

        def onDrawingSelected(drawing: Drawing[Point]): Callback = {
            bs.modState { state => state.copy(currentDrawing = drawing, canvasVersion = state.canvasVersion + 1)}
        }
    }

    val initialState = State(
        Conf.canvasInitializer,
        EvolutionDrawer(
            SimpleRNG(Random.nextLong()),
            1000
        ),
        EvolutionPortfolio.drawingList.drawing("brownian").get,
        EvolutionPortfolio.drawingList,
        windowSize(dom.window)
    )

    val component = ScalaComponent.builder[Unit]("Page")
        .initialState(initialState)
        .renderBackend[Backend]
        .build

    private def windowSize(window: Window) = {
        val document = window.document
        Point(
            Math.max(document.documentElement.clientWidth, window.innerWidth).toInt,
            Math.max(document.documentElement.clientHeight, window.innerHeight).toInt
        )
    }
}
