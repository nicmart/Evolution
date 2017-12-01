package evolution.app.react.component

import evolution.app.canvas.Drawer
import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.presentational._
import japgolly.scalajs.react.component.Scala.{BackendScope, Component}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CallbackTo, CtorType, ScalaComponent}
import org.scalajs.dom
import evolution.geometry.Point
import evolution.app.model.state.DrawingState
import evolution.app.react.pages.{LoadDrawingPage, MyPages}
import japgolly.scalajs.react.extra.router.RouterCtl
import io.circe._
import io.circe.parser._

import scala.util.{Random, Try}

object App {

  type ReactComponent[C] = Component[Props[C], State[C], Backend[C], CtorType.Props]

  case class State[C](
    drawer: Drawer,
    points: Long => Stream[Point],
    drawingState: DrawingState[C],
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter
  ) {
    def stream: Stream[Point] = points(drawingState.seed)

    /**
      * Used to determine if the page needs an update
      */
    def key: Int = (
      drawingState,
      pointRateCounter.rate
    ).hashCode()
  }

  case class Props[C](
    router: RouterCtl[MyPages[C]],
    drawingState: DrawingState[C]
  )

  class Backend[C](
    definition: DrawingDefinition.Aux[Point, C],
    canvasInitializer: dom.html.Canvas => Unit,
    pageComponent: Page.ReactComponent[C]
  )(bs: BackendScope[Props[C], State[C]]) {
    def render(props: Props[C], state: State[C]): VdomElement = {
      pageComponent(Page.Props(
        state.drawingContext,
        state.drawer,
        state.stream,
        props.drawingState,
        state.pointRateCounter.rate.toInt,
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

    private def onConfigChange(drawingConfig: C): Callback = {
      bs.modState { state =>
        state
          .copy(drawingState = state.drawingState.copy(config = drawingConfig))
      } >> Callback.log("updating confs: " + drawingConfig.toString) >> refresh
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

  private def stateFromProps[C](
    definition: DrawingDefinition.Aux[Point, C]
  )(props: Props[C]): State[C] = {
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

  def component[C](
    definition: DrawingDefinition.Aux[Point, C],
    canvasInitializer: dom.html.Canvas => Unit,
    pageComponent: Page.ReactComponent[C]
  ) =
    ScalaComponent.builder[Props[C]]("App")
    .initialStateFromProps(stateFromProps(definition))
    .backend[Backend[C]](scope => new Backend[C](definition, canvasInitializer, pageComponent)(scope))
    .render(scope => scope.backend.render(scope.props, scope.state))
    .shouldComponentUpdate { s =>
      CallbackTo.pure {
        s.currentState.key != s.nextState.key
      }
    }
    .componentDidUpdate { event =>
      if (event.currentState.drawingState != event.prevState.drawingState) {
//        event.currentProps.router.set(
//          LoadDrawingPage(
//            event.currentState.drawingState
//          )
//        ) >>
          Callback.log("DIDUPDATE")
      } else Callback.empty
    }
    .componentWillReceiveProps { event =>
      // @todo
      if (event.nextProps.drawingState != event.currentProps.drawingState) {
        println(event.nextProps.drawingState)
        println(event.currentProps.drawingState)
        val newState = stateFromProps(definition)(event.nextProps)
        if (newState.drawingState != event.state.drawingState) {
          event.setState(newState) >> Callback.log("WILL RECEIVE PROPS")
        } else Callback.empty
      } else Callback.empty
    }
    .build
}
