package evolution.app.react.component

import evolution.app.canvas.EvolutionDrawer
import evolution.app.conf.Conf
import evolution.app.model.{ConfiguredDrawing, DrawingContext, DrawingDefinitionList, DrawingListWithSelection}
import evolution.app.portfolio.DrawingPortfolio
import evolution.app.portfolio.DrawingPortfolio.DrawingDefinition
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
    currentDrawing: ConfiguredDrawing[Point],
    drawingListWithSelection: DrawingListWithSelection,
    drawingContext: DrawingContext,
    canvasVersion: Int = 0
  ) {
    def evolution: Evolution[Point] = currentDrawing.evolution

    def increaseVersion: State = copy(canvasVersion = canvasVersion + 1)

    def drawingList: DrawingDefinitionList = drawingListWithSelection.list

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
              // @TODO remove drawing definition list
              state.drawingListWithSelection.list.drawings,
              state.drawingListWithSelection.current,
              onDrawingDefinitionChange
            )
          ),
          List(),
          bs.modState(_.increaseVersion.updateSeed)
        )),
        <.div(^.id := "page-content",
          CanvasComponent.component.withKey(state.canvasVersion)(CanvasComponent.Props(
            state.canvasInitializer,
            state.currentDrawing,
            state.drawer,
            state.drawingContext
          )),
          SidebarComponent.component.withKey(state.currentDrawing.name)(
            SidebarComponent.Props(
              active = true,
              state.currentDrawing.configElement(onConfiguredDrawingChange)
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

    def onConfiguredDrawingChange(configuredDrawing: ConfiguredDrawing[Point]): Callback = {
      bs.modState { state =>
        state
          .copy(currentDrawing = configuredDrawing)
          .increaseVersion
          .updateSeed
      }
    }

    def onDrawingDefinitionChange(definition: DrawingDefinition): Callback = {
      bs.modState { state =>
        state
          .copy(drawingListWithSelection = state.drawingListWithSelection.copy(current = definition))
            .copy(currentDrawing = definition.drawing(state.drawingContext))
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
    DrawingPortfolio.listWithSelection.current.drawing(drawingContext(dom.window)),
    DrawingPortfolio.listWithSelection,
    drawingContext(dom.window)
  )

  val component = ScalaComponent.builder[Unit]("Page")
    .initialState(initialState)
    .renderBackend[Backend]
    .build

  private def drawingContext(window: Window): DrawingContext = {
    val document = window.document
    DrawingContext(
      DrawingContext.CanvasSize(
        2 * Math.max(document.documentElement.clientWidth, window.innerWidth).toInt,
        2 * Math.max(document.documentElement.clientHeight, window.innerHeight).toInt
      )
    )
  }
}
