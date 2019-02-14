package evolution.app.react.component

import cats.Eval
import evolution.app.model.context.DrawingContext
import evolution.app.model.counter.RateCounter
import evolution.app.react.component.presentational._
import japgolly.scalajs.react.component.Scala.{ BackendScope, Component }
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ Callback, CallbackTo, CtorType, ScalaComponent }
import org.scalajs.dom
import evolution.geometry.Point
import evolution.app.model.state.{ DrawingState, RendererState }
import evolution.app.react.pages._
import japgolly.scalajs.react.extra.StateSnapshot

object App {

  type ReactComponent[C] = Component[StateSnapshot[PageState[C]], State[C], Backend[C], CtorType.Props]

  case class State[C](
    drawingContext: DrawingContext,
    pointRateCounter: RateCounter,
    running: Boolean
  )

  class Backend[C](
    points: (DrawingContext, DrawingState[C]) => Iterator[Point],
    canvasInitializer: dom.html.Canvas => Unit,
    pageComponent: Page.ReactComponent[C]
  )(bs: BackendScope[StateSnapshot[PageState[C]], State[C]]) {
    def render(pageStateSnapshot: StateSnapshot[PageState[C]], state: State[C]): VdomElement = {
      val stateSnapshot = StateSnapshot(state)(s => bs.setState(s))
      val drawingStateSnapshot = pageStateSnapshot.zoomState(_.drawingState)(drawingState =>
        pageState => pageState.copy(drawingState = drawingState))
      val renderingStateSnapshot = pageStateSnapshot.zoomState(_.rendererState)(renderingState =>
        pageState => pageState.copy(rendererState = renderingState))

      pageComponent(
        Page.Props(
          stateSnapshot.zoomState(_.running)(isPlaying => state => state.copy(running = isPlaying)),
          state.drawingContext,
          renderingStateSnapshot,
          Eval.later(points(state.drawingContext, pageStateSnapshot.value.drawingState)),
          drawingStateSnapshot,
          state.pointRateCounter.rate.toInt,
          drawingStateSnapshot.modState(_.withNewSeed),
          onRateCountUpdate(pageStateSnapshot.value.rendererState)
        ))
    }

    private[App] def key(p: PageState[C], s: State[C]): Int =
      (s.pointRateCounter.rate, p, s.running).hashCode()

    private def onRateCountUpdate(rendererState: RendererState): Callback =
      bs.modState { state =>
        state.copy(pointRateCounter = state.pointRateCounter.count(rendererState.iterations))
      }

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
    points: (DrawingContext, DrawingState[C]) => Iterator[Point],
    canvasInitializer: dom.html.Canvas => Unit,
    rateCounter: RateCounter,
    pageComponent: Page.ReactComponent[C]
  ) =
    ScalaComponent
      .builder[StateSnapshot[PageState[C]]]("App")
      .initialState(State[C](drawingContext, rateCounter, true))
      .backend[Backend[C]](scope => new Backend[C](points, canvasInitializer, pageComponent)(scope))
      .render(scope => scope.backend.render(scope.props, scope.state))
      .shouldComponentUpdate { s =>
        CallbackTo.pure {
          s.backend.key(s.currentProps.value, s.currentState) != s.backend.key(s.nextProps.value, s.nextState)
        }
      }
      .componentDidMount(s =>
        Callback { dom.window.addEventListener("resize", (_: Any) => s.backend.onResize.runNow()) })
      .build
}
