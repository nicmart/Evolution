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
import evolution.app.model.state.{DrawingState, RendererState}
import evolution.app.react.pages.{LoadDrawingPage, MyPages, PageState}
import japgolly.scalajs.react.extra.router.RouterCtl
import org.scalajs.dom.raw.UIEvent

import scala.util.Random

object App {

  type ReactComponent[C] = Component[Props[C], State[C], Backend[C], CtorType.Props]

  case class State[C](
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter,
    running: Boolean
  ) {
    def play: State[C] = copy(running = true)
    def stop: State[C] = copy(running = false)
    def toggle: State[C] = copy(running = !running)
  }

  case class Props[C](
    pageState: PageState[C],
    onPageStateChange: PageState[C] => Callback
  ) {
    def hasChanged: Callback = onPageStateChange(pageState)
    def withDrawingState(drawingState: DrawingState[C]): Props[C] =
      copy(pageState.copy(drawingState = drawingState))
    def withRenderingState(rendererState: RendererState): Props[C] =
      copy(pageState.copy(rendererState = rendererState))
  }

  class Backend[C](
    points: (DrawingContext, DrawingState[C]) => Stream[Point],
    canvasInitializer: dom.html.Canvas => Unit,
    pageComponent: Page.ReactComponent[C]
  )(bs: BackendScope[Props[C], State[C]]) {
    def render(props: Props[C], state: State[C]): VdomElement = {
      pageComponent(Page.Props(
        state.running,
        state.drawingContext,
        props.pageState.rendererState,
        points(state.drawingContext, props.pageState.drawingState),
        props.pageState.drawingState,
        state.pointRateCounter.rate.toInt,
        onRunningToggleChange,
        onConfigChange(props),
        props.withRenderingState(_).hasChanged,
        refresh(props),
        onRateCountUpdate(props)
      ))
    }

    private[App] def key(p: Props[C], s: State[C]): Int =
      (s.pointRateCounter.rate, p.pageState, s.running).hashCode()


    private def onConfigChange(props: Props[C])(drawingConfig: C): Callback = {
      props.withDrawingState(
        DrawingState(
          Random.nextLong(),
          drawingConfig
        )
      ).hasChanged >> bs.modState(state => state.play)
    }

    private def refresh(props: Props[C]): Callback = {
      props.withDrawingState(
        DrawingState(
          Random.nextLong(),
          props.pageState.drawingState.config
        )
      ).hasChanged >> bs.modState(state => state.play)
    }

    private def onRateCountUpdate(p: Props[C]): Callback =
      bs.modState { state =>
        state.copy(pointRateCounter = state.pointRateCounter.count(p.pageState.rendererState.iterations))
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
    rateCounter: RateCounter,
    pageComponent: Page.ReactComponent[C]
  ) =
    ScalaComponent.builder[Props[C]]("App")
      .initialState(State[C](drawingContext, rateCounter, true))
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
