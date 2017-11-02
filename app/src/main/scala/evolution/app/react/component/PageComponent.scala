package evolution.app.react.component

import evolution.app.canvas.Drawer
import evolution.app.conf.Conf
import evolution.app.model.configured.{DefinitionToComponent, DrawingComponent}
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.definition.{AbstractDrawingDefinition, DrawingDefinition, DrawingListWithSelection}
import evolution.app.react.component.presentational._
import evolution.app.react.component.presentational.styled.HorizontalFormFieldComponent
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent}
import org.scalajs.dom
import org.scalajs.dom.Window
import evolution.geometry.Point
import evolution.algebra.materializer.Materializer

import scala.util.Random

object PageComponent {

  def definitionToComponent: DefinitionToComponent[Long, Point] =
    Conf.definitionToComponent

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class State(
    drawer: Drawer,
    currentDrawing: DrawingComponent[Long, Point],
    drawingListWithSelection: DrawingListWithSelection[Point],
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter,
    seed: Long
  ) {
    def points: Stream[Point] =
      currentDrawing.materialize(seed)

    /**
      * Create a new seed
      */
    def updateRng: State =
      copy(seed = Random.nextLong())

    /**
      * Use to determine if the canvas has to be re-rendered
      */
    def canvasKey: String = seed.toString

    /**
      * Used to determine if the page needs an update
      *
      * @return
      */
    def key: Int = (
      seed,
      currentDrawing,
      pointRateCounter.rate,
      drawingListWithSelection.current.name
    ).hashCode()
  }

  class Backend(bs: BackendScope[Unit, State]) {
    def render(state: State): VdomElement = {
      <.div(
        NavbarComponent.component(NavbarComponent.Props(
          <.span(s"${state.pointRateCounter.rate.toInt} p/s"),
          ButtonComponent.component(ButtonComponent.Props(
            "Refresh",
            bs.modState(_.updateRng)
          )
          ),
          HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
            "Iterations",
            "",
            IntInputComponent(state.drawer.iterations, onIterationsChanged)
          )
          ),
          HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
            "Drawing",
            "",
            DrawingListComponent.component(
              DrawingListComponent.Props(
                state.drawingListWithSelection.list,
                state.drawingListWithSelection.current,
                onDrawingDefinitionChange
              )
            )
          )
          )
        )
        ),
        <.div(
          ^.id := "page-content",
          CanvasComponent.component.withKey(state.canvasKey)(CanvasComponent.Props(
            state.drawingContext,
            canvasInitializer,
            state.drawer,
            state.points,
            bs.modState { state =>
              state.copy(pointRateCounter = state.pointRateCounter.count(state.drawer.iterations))
            }
          )
          ),
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
          .updateRng
      }
    }

    def onSizeChanged(value: Int): Callback = {
      bs.modState { state =>
        state
          .copy(drawer = state.drawer.copy(strokeSize = value))
          .updateRng
      }
    }

    def onConfiguredDrawingChange(configuredDrawing: DrawingComponent[Long, Point]): Callback = {
      bs.modState { state =>
        state
          .copy(currentDrawing = configuredDrawing)
          .updateRng
      }
    }

    def onDrawingDefinitionChange(definition: DrawingDefinition[Point]): Callback = {
      bs.modState { state =>
        state
          .copy(drawingListWithSelection = state.drawingListWithSelection.copy(current = definition))
          .copy(currentDrawing = definitionToComponent.toComponent(definition, state.drawingContext))
          .updateRng
      }
    }
  }

  val initialState = State(
    Drawer(
      1000,
      1
    ),
    definitionToComponent.toComponent(
      Conf.drawingList.current,
      drawingContext(dom.window)
    ),
    Conf.drawingList,
    drawingContext(dom.window),
    RateCounter.empty(1000),
    Random.nextLong()
  )

  val component = ScalaComponent.builder[Unit]("Page")
    .initialState(initialState)
    .renderBackend[Backend]
    .shouldComponentUpdate(s => CallbackTo.pure(s.currentState.key != s.nextState.key))
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
