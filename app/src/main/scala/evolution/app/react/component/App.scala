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

  // @todo: move points and context to the props
  // This should allow us to remove stateFromProps methods
  case class State[C](
    drawer: Drawer,
    points: Long => Stream[Point],
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter
  )

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
        points(props, state),
        props.drawingState,
        state.pointRateCounter.rate.toInt,
        onConfigChange(props),
        refresh(props),
        onIterationsChanged,
        onRateCountUpdate
      ))
    }

    private def points(p: Props[C], s: State[C]): Stream[Point] =
      s.points(p.drawingState.seed)

    private[App] def key(p: Props[C], s: State[C]): Int =
      ( p.drawingState,
        s.pointRateCounter.rate,
        s.drawer.iterations
      ).hashCode()

    private[App] def onIterationsChanged(value: Int): Callback = {
      bs.modState { state =>
        state.copy(drawer = state.drawer.copy(iterations = value))
      }
    }

    private def onConfigChange(props: Props[C])(drawingConfig: C): Callback = {
      props.router.set(LoadDrawingPage(
        DrawingState(
          Random.nextLong(),
          drawingConfig
        )
      ))
    }

    private def refresh(props: Props[C]): Callback = {
      props.router.set(LoadDrawingPage(
        DrawingState(
          Random.nextLong(),
          props.drawingState.config
        )
      ))
    }

    private def onRateCountUpdate: Callback =
      bs.modState { state =>
        state.copy(pointRateCounter = state.pointRateCounter.count(state.drawer.iterations))
      }
  }

  private val drawingContext: DrawingContext = {
    val document = dom.window.document
    DrawingContext(
      DrawingContext.CanvasSize(
        2 * Math.max(document.documentElement.clientWidth, dom.window.innerWidth).toInt,
        2 * Math.max(document.documentElement.clientHeight, dom.window.innerHeight).toInt
      )
    )
  }

  private def stateFromProps[C](
    definition: DrawingDefinition.Aux[Point, C]
  )(props: Props[C]): State[C] = {
    State(
      Drawer(
        1000,
        1
      ),
      seed => Conf.materializer.materialize(seed, definition.evolution(props.drawingState.config, drawingContext)),
      drawingContext,
      RateCounter.empty(5000)
    )
  }

  private def updateStateFromProps[C](
    definition: DrawingDefinition.Aux[Point, C]
  )(props: Props[C], state: State[C]): State[C] =
    state
      .copy(points = seed => Conf.materializer.materialize(seed, definition.evolution(props.drawingState.config, drawingContext)))

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
        s.backend.key(s.currentProps, s.currentState) != s.backend.key(s.nextProps, s.nextState)
      }
    }
    .componentWillReceiveProps { event =>
      val newState = updateStateFromProps(definition)(event.nextProps, event.state)
      event.setState(newState)
    }
    .build
}
