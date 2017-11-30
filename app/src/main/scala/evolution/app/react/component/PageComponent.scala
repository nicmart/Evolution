package evolution.app.react.component

import evolution.app.canvas.Drawer
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.presentational._
import evolution.app.react.component.presentational.styled.HorizontalFormField
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, ScalaComponent}
import org.scalajs.dom
import evolution.geometry.Point
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.DrawingConfig
import evolution.app.react.pages.{LoadDrawingPage, MyPages}
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
      Conf.pageComponent(Page.Props(
        state.drawingContext,
        state.drawer,
        state.stream,
        props.drawingState,
        state.pointRateCounter.rate.toInt,
        state.drawer.iterations,
        onConfigChange,
        onStreamChange,
        refresh,
        onIterationsChanged,
        bs.modState { state =>
          state.copy(pointRateCounter = state.pointRateCounter.count(state.drawer.iterations))
        }
      ))
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
        5000,
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
    .componentDidUpdate { x =>
      if (x.currentState.drawingState != x.prevState.drawingState) {
        x.currentProps.router.set(
          LoadDrawingPage(
            x.currentState.drawingState
          )
        ) >> Callback.log("DIDUPDATE")
      } else Callback.empty
    }
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
