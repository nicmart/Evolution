package evolution.app.react.component

import evolution.app.canvas.drawer.{BaseFrameDrawer, FrameDrawer}
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
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
import org.scalajs.dom.raw.UIEvent

import scala.util.Random

object App {

  type ReactComponent[C] = Component[Props[C], State[C], Backend[C], CtorType.Props]

  case class State[C](
    drawer: BaseFrameDrawer,
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter,
    running: Boolean
  ) {
    def play: State[C] = copy(running = true)
    def stop: State[C] = copy(running = false)
    def toggle: State[C] = copy(running = !running)
  }

  case class Props[C](
    router: RouterCtl[MyPages[C]],
    drawingState: DrawingState[C]
  )

  class Backend[C](
    points: (DrawingContext, DrawingState[C]) => Stream[Point],
    canvasInitializer: dom.html.Canvas => Unit,
    pageComponent: Page.ReactComponent[C]
  )(bs: BackendScope[Props[C], State[C]]) {
    def render(props: Props[C], state: State[C]): VdomElement = {
      pageComponent(Page.Props(
        state.running,
        state.drawingContext,
        state.drawer,
        points(state.drawingContext, props.drawingState),
        props.drawingState,
        state.pointRateCounter.rate.toInt,
        onRunningToggleChange,
        onConfigChange(props),
        refresh(props),
        onIterationsChanged,
        onRateCountUpdate
      ))
    }

    private[App] def key(p: Props[C], s: State[C]): Int =
      ( p.drawingState,
        s.pointRateCounter.rate,
        s.drawer.iterations,
        s.running
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
      )) >> bs.modState(state => state.play)
    }

    private def refresh(props: Props[C]): Callback = {
      props.router.set(LoadDrawingPage(
        DrawingState(
          Random.nextLong(),
          props.drawingState.config
        )
      )) >> bs.modState(state => state.play)
    }

    private def onRateCountUpdate: Callback =
      bs.modState { state =>
        state.copy(pointRateCounter = state.pointRateCounter.count(state.drawer.iterations))
      }

    private def onRunningToggleChange(isRunning: Boolean): Callback =
      bs.modState { state => state.toggle }

    def onResize: Callback =
      bs.modState(s => s.copy(drawingContext = drawingContext))
  }

  private def drawingContext: DrawingContext = {
    val document = dom.window.document
    DrawingContext(
      DrawingContext.CanvasSize(
        2 * Math.max(document.documentElement.clientWidth, dom.window.innerWidth).toInt,
        2 * Math.max(document.documentElement.clientHeight, dom.window.innerHeight).toInt
      )
    )
  }

  def component[C](
    points: (DrawingContext, DrawingState[C]) => Stream[Point],
    canvasInitializer: dom.html.Canvas => Unit,
    frameDrawerFromContext: DrawingContext => BaseFrameDrawer,
    rateCounter: RateCounter,
    pageComponent: Page.ReactComponent[C]
  ) =
    ScalaComponent.builder[Props[C]]("App")
      .initialState(State[C](frameDrawerFromContext(drawingContext), drawingContext, rateCounter, true))
      .backend[Backend[C]](scope => new Backend[C](points, canvasInitializer, pageComponent)(scope))
      .render(scope => scope.backend.render(scope.props, scope.state))
      .shouldComponentUpdate { s =>
        CallbackTo.pure {
          s.backend.key(s.currentProps, s.currentState) != s.backend.key(s.nextProps, s.nextState)
        }
      }
      .componentDidMount(s => Callback { dom.window.addEventListener("resize", (_: Any) => s.backend.onResize.runNow()) })
      .build
}
