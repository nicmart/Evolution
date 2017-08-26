package evolution.app.react.component

import evolution.app.canvas.EvolutionDrawer
import evolution.app.conf.Conf
import evolution.app.model.configured.ConfiguredDrawing
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.react.component.presentational.styled.HorizontalFormFieldComponent
import evolution.app.react.component.presentational._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.Window
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
    pointRateCounter: RateCounter,
    canvasVersion: Int = 0
  ) {
    def evolution: Evolution[Point] = currentDrawing.evolution

    def increaseVersion: State = copy(canvasVersion = canvasVersion + 1)

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
          ButtonComponent.component(ButtonComponent.Props(
            "Refresh",
            bs.modState(_.increaseVersion.updateSeed)
          )),
//          HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
//            "Rate",
//            "",
//            DoubleInputComponent(state.pointRateCounter.rate, _ => Callback.empty)
//          )),
          HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
            "Iterations",
            "",
            IntInputComponent(state.drawer.iterations, onIterationsChanged)
          )),
          HorizontalFormFieldComponent.component.withKey(1011)(HorizontalFormFieldComponent.Props(
            "Drawing",
            "",
            DrawingListComponent.component.withKey(1)(
              DrawingListComponent.Props(
                state.drawingListWithSelection.list,
                state.drawingListWithSelection.current,
                onDrawingDefinitionChange
              )
            )
          ))
        )),
        <.div(^.id := "page-content",
          CanvasComponent.component.withKey(state.canvasVersion)(CanvasComponent.Props(
            state.canvasInitializer,
            state.currentDrawing,
            state.drawer,
            state.drawingContext,
            bs.modState { state =>
              state.copy(pointRateCounter = state.pointRateCounter.count(state.drawer.iterations))
            }
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
    Conf.drawingList.current.drawing(drawingContext(dom.window)),
    Conf.drawingList,
    drawingContext(dom.window),
    RateCounter.empty(10000)
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
