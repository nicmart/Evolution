package evolution.app.react.component

import evolution.app.canvas.Drawer
import evolution.app.conf.Conf
import evolution.app.model.configured.DrawingConfigComponent
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.presentational._
import evolution.app.react.component.presentational.styled.HorizontalFormFieldComponent
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent}
import org.scalajs.dom
import evolution.geometry.Point
import evolution.app.model.state.DrawingState
import evolution.app.react.pages.MyPages
import japgolly.scalajs.react.extra.router.RouterCtl
import io.circe._
import io.circe.parser._

import scala.util.{Random, Try}

object PageComponent {

  val definition: DrawingDefinition.Aux[Point, Conf.drawingDefinition.Config] =
    Conf.drawingDefinition

  def canvasInitializer: dom.html.Canvas => Unit =
    Conf.canvasInitializer

  case class State(
    drawer: Drawer,
    points: Long => Stream[Point],
    drawingState: DrawingState[definition.Config],
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter
  ) {
    def stream: Stream[Point] = points(drawingState.seed)
    /**
      * Use to determine if the canvas has to be re-rendered
      */
    def canvasKey: String = drawingState.seed.toString

    /**
      * Used to determine if the page needs an update
      */
    def key: Int = (
      drawingState,
      pointRateCounter.rate
    ).hashCode()
  }

  case class Props(
    router: RouterCtl[MyPages],
    drawingState: DrawingState[definition.Config]
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
          )))
        )),
        <.div(
          ^.id := "page-content",
          CanvasComponent.component.withKey(state.canvasKey)(CanvasComponent.Props(
            state.drawingContext,
            canvasInitializer,
            state.drawer,
            state.stream,
            bs.modState { state =>
              state.copy(pointRateCounter = state.pointRateCounter.count(state.drawer.iterations))
            }
          )
          ),
          SidebarComponent.component.withKey("sidebar")(
            SidebarComponent.Props(
              active = true
            )
          )(
            Conf.drawingConfComponent(DrawingConfigComponent.Props[Long, Point, definition.Config](
              props.drawingState.config,
              onConfigChange,
              onStreamChange
            ))
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

    private def onConfigChange(drawingConfig: definition.Config): Callback = {
      bs.modState { state =>
        state
          .copy(drawingState = state.drawingState.copy(config = drawingConfig))
      } >> refresh
    }

    private def onStreamChange(points: Long => Stream[Point]): Callback = {
      bs.modState { state =>
        state
          .copy(points = points)
      } >> refresh
    }

    private def refresh: Callback = {
      bs.modState { state =>
        state
          .copy(drawingState = state.drawingState.copy(seed = Random.nextLong()))
      }
    }
  }

  def stateFromProps(props: Props): State = {
    State(
      Drawer(
        1000,
        1
      ),
      seed => Conf.materializer.materialize(seed, definition.evolution(props.drawingState.config, Conf.drawingContext)),
      props.drawingState,
      Conf.drawingContext,
      RateCounter.empty(1000)
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
//    .componentDidUpdate { x =>
//      if (
//        x.currentState.currentDrawing.serialize != x.prevState.currentDrawing.serialize &&
//        !Conf.areLoadableDrawingDifferent(x.prevProps.loadableDrawing, x.currentProps.loadableDrawing)
//      ) {
//        x.currentProps.router.set(
//          LoadDrawingPage(
//            DrawingState(
//              x.currentState.seed,
//              x.currentState.currentDrawing.
//            )
//          )
//        ) >> Callback.log("DIDUPDATE")
//      } else Callback.empty
//    }
    .componentWillReceiveProps { x =>
      if (Conf.areLoadableDrawingDifferent(x.nextProps.drawingState, x.currentProps.drawingState)) {
        val newState = stateFromProps(x.nextProps)
        if (newState.drawingState != x.state.drawingState) {
          x.setState(newState) >> Callback.log("WILL RECEIVE PROPS")
        } else Callback.empty
      } else Callback.empty
    }
    .build
}
