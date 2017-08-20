package evolution.app.react.component

import evolution.app.canvas.EvolutionDrawer
import evolution.app.conf.Conf
import evolution.app.model.{Drawing, DrawingList, DrawingListWithSelection}
import evolution.app.portfolio.DrawingPortfolio
import evolution.app.react.component.presentational.{IntInputComponent, SidebarComponent}
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
    selection: DrawingListWithSelection[Point],
    size: Point,
    canvasVersion: Int = 0
  ) {
    def evolution: Evolution[Point] = currentDrawing.evolution

    def increaseVersion: State = copy(canvasVersion = canvasVersion + 1)
    def currentDrawing: Drawing[Point] = selection.current
    def drawingList: DrawingList[Point] = selection.list
    def updateSeed: State =
      copy(drawer = drawer.copy(rng = SimpleRNG(Random.nextLong())))
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
              state.selection,
              onDrawingChange
            )
          ),
          List()
        )),
        <.div(^.id := "page-content",
          CanvasComponent.component.withKey(state.canvasVersion)(CanvasComponent.Props(
            state.canvasInitializer,
            state.currentDrawing,
            state.drawer,
            state.size
          )),
          SidebarComponent.component(
            SidebarComponent.Props(
              active = true,
              state.selection.current.configElement(onDrawingChange)
            )
          )
        )
      )
    }

    def onIterationsChanged(value: Int): Callback = {
      bs.modState { state =>
        state
            .copy(drawer = state.drawer.copy(iterations = value))
            .increaseVersion
      }
    }

    def onSizeChanged(value: Int): Callback = {
      bs.modState { state =>
        state
            .copy(drawer = state.drawer.copy(strokeSize = value))
            .increaseVersion
      }
    }

    def onDrawingChange(drawing: Drawing[Point]): Callback = {
      bs.modState { state =>
        state
            .copy(selection = state.selection.copy(current = drawing))
            .increaseVersion
            .updateSeed
      }
    }
  }

  val initialState = State(
    Conf.canvasInitializer,
    EvolutionDrawer(
      SimpleRNG(Random.nextLong()),
      1000,
      1
    ),
    DrawingPortfolio.listWithSelection,
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
