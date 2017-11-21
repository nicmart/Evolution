package evolution.app.react.component

import evolution.app.canvas.Drawer
import evolution.app.conf.Conf
import evolution.app.model.configured.{DefinitionToComponent, DrawingComponent}
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.definition.{DrawingDefinition, DrawingListWithSelection}
import evolution.app.react.component.presentational._
import evolution.app.react.component.presentational.styled.HorizontalFormFieldComponent
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent}
import org.scalajs.dom
import evolution.geometry.Point
import evolution.app.model.state.LoadableDrawing
import evolution.app.react.pages.{LoadDrawingPage, MyPages}
import japgolly.scalajs.react.extra.router.RouterCtl
import io.circe._
import io.circe.parser._

import scala.util.{Random, Try}

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
      * Use to determine if the canvas has to be re-rendered
      */
    def canvasKey: String = seed.toString

    /**
      * Used to determine if the page needs an update
      */
    def key: Int = (
      seed,
      currentDrawing,
      pointRateCounter.rate,
      drawingListWithSelection.current.name
    ).hashCode()
  }

  case class Props(
    router: RouterCtl[MyPages],
    loadableDrawing: LoadableDrawing
  )

  class Backend(bs: BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = {
      <.div(
        NavbarComponent.component(NavbarComponent.Props(
          <.div(^.className := "navbar-item is-hidden-touch",<.span(s"${ state.pointRateCounter.rate.toInt } p/s")),
          <.div(^.className := "navbar-item is-hidden-touch", ButtonComponent.component(ButtonComponent.Props(
            "Refresh",
            refresh
          ))),
          <.div(^.className := "navbar-item is-hidden-touch", HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
            "Iterations",
            "",
            IntInputComponent(state.drawer.iterations, onIterationsChanged)
          ))),
          <.div(^.className := "navbar-item", HorizontalFormFieldComponent.component(HorizontalFormFieldComponent.Props(
            "Drawing",
            "",
            DrawingListComponent.component(
              DrawingListComponent.Props(
                state.drawingListWithSelection.list,
                state.drawingListWithSelection.current,
                onDrawingDefinitionChange
              )
            )
          )))
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

    private def onIterationsChanged(value: Int): Callback = {
      bs.modState { state =>
        state
          .copy(drawer = state.drawer.copy(iterations = value))
      } >> refresh
    }

    private def onConfiguredDrawingChange(drawingComponent: DrawingComponent[Long, Point]): Callback = {
      bs.modState { state =>
        state
          .copy(currentDrawing = drawingComponent)
      } >> refresh
    }

    private def onDrawingDefinitionChange(definition: DrawingDefinition[Point]): Callback = {
      bs.modState { state =>
        state
          .copy(drawingListWithSelection = state.drawingListWithSelection.copy(current = definition))
          .copy(currentDrawing = definitionToComponent.toComponentWithInitialConfig(definition, state.drawingContext))
      } >> refresh
    }

    private def refresh: Callback = {
      bs.modState { state =>
        state.copy(seed = Random.nextLong())
      }
    }
  }

  def stateFromProps(props: Props): State = {
    State(
      Drawer(
        1000,
        1
      ),
      props.loadableDrawing.drawingComponent,
      Conf.drawingList.copy(current = Conf.drawingList.byName(props.loadableDrawing.drawingComponent.name)),
      Conf.drawingContext,
      RateCounter.empty(1000),
      props.loadableDrawing.seed
    )
  }

  val component = ScalaComponent.builder[Props]("Page")
    .initialStateFromProps(stateFromProps)
    .renderBackend[Backend]
    .shouldComponentUpdate { s =>
      CallbackTo.pure {
        s.currentState.key != s.nextState.key
      }
    }
    .componentDidUpdate { x =>
      x.currentProps.router.set(
        LoadDrawingPage(
          LoadableDrawing(
            x.currentState.seed,
            x.currentState.currentDrawing
          )
        )
      )
    }
    .componentWillReceiveProps { x =>
      if (Conf.areLoadableDrawingEquivalent(x.nextProps.loadableDrawing, x.currentProps.loadableDrawing)) {
        x.setState(stateFromProps(x.nextProps))
      } else Callback.empty
    }
    .build
}
