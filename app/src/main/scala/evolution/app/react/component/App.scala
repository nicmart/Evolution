package evolution.app.react.component

import cats.Eval
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.model.counter.RateCounter
import evolution.app.react.component.presentational._
import japgolly.scalajs.react.component.Scala.{ BackendScope, Component }
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.{ Callback, CallbackTo, CtorType, ScalaComponent }
import org.scalajs.dom
import evolution.geometry.Point
import evolution.app.model.state.{ DrawingState, RendererState }
import evolution.app.react.pages._
import japgolly.scalajs.react.extra.StateSnapshot

object App {

  type ReactComponent[C] = Component[StateSnapshot[PageState[C]], State[C], Backend[C], CtorType.Props]

  case class State[C](
    pointRateCounter: RateCounter,
    running: Boolean,
    layout: LayoutState
  ) {
    def drawingContext: DrawingContext = layout.drawingContext
    def withWindowSize(size: Point): State[C] = copy(layout = layout.copy(windowSize = size))
  }

  case class LayoutState(sidebarWidth: Double, windowSize: Point, sidebarExpanded: Boolean) {
    private val canvasWidth: Double = if (sidebarExpanded) windowSize.x - sidebarWidth else windowSize.x
    val drawingContext: DrawingContext =
      DrawingContext(CanvasSize(canvasWidth.toInt, windowSize.y.toInt))
  }

  class Backend[C](
    points: (DrawingContext, DrawingState[C]) => Iterator[Point],
    canvasInitializer: dom.html.Canvas => Unit,
    pageComponent: Page.ReactComponent[C]
  )(bs: BackendScope[StateSnapshot[PageState[C]], State[C]]) {
    def render(pageStateSnapshot: StateSnapshot[PageState[C]], state: State[C]): VdomElement = {
      val stateSnapshot = StateSnapshot(state)(s => bs.setState(s))
      val drawingStateSnapshot = pageStateSnapshot.zoomState(_.drawingState)(drawingState =>
        pageState => pageState.copy(drawingState = drawingState))
      val layoutSnapshot = StateSnapshot(state.layout)(layout => bs.modState(_.copy(layout = layout)))
      val renderingStateSnapshot = pageStateSnapshot.zoomState(_.rendererState)(renderingState =>
        pageState => pageState.copy(rendererState = renderingState))

      pageComponent(
        Page.Props[C](
          running = stateSnapshot.zoomState(_.running)(isPlaying => state => state.copy(running = isPlaying)),
          layout = layoutSnapshot,
          rendererState = renderingStateSnapshot,
          points = Eval.later(
            points(
              state.drawingContext * pageStateSnapshot.value.rendererState.resolutionFactor,
              pageStateSnapshot.value.drawingState)),
          drawingState = drawingStateSnapshot,
          pointRate = state.pointRateCounter.rate.toInt,
          onRefresh = drawingStateSnapshot.modState(_.withNewSeed),
          onFrameDraw = onRateCountUpdate(pageStateSnapshot.value.rendererState)
        )
      ).vdomElement
    }

    private[App] def key(p: PageState[C], s: State[C]): Int =
      (s.pointRateCounter.rate, p, s.running, s.layout).hashCode()

    private def onRateCountUpdate(rendererState: RendererState): Callback =
      bs.modState { state =>
        state.copy(pointRateCounter = state.pointRateCounter.count(rendererState.iterations))
      }

    def updateLayout: Callback =
      windowSize.flatMap { windowSize =>
        bs.modState(s => s.withWindowSize(windowSize))
      }
  }

  private def windowSize: CallbackTo[Point] = CallbackTo {
    val element = dom.window.document.documentElement
    Point(
      element.clientWidth,
      element.clientHeight
    )
  }

  private def initialLayout: CallbackTo[LayoutState] = windowSize.map { size =>
    LayoutState(size.x / 3, size, true)
  }

  def component[C](
    points: (DrawingContext, DrawingState[C]) => Iterator[Point],
    canvasInitializer: dom.html.Canvas => Unit,
    rateCounter: RateCounter,
    pageComponent: Page.ReactComponent[C]
  ) =
    ScalaComponent
      .builder[StateSnapshot[PageState[C]]]("App")
      .initialStateCallback(initialLayout.map(layout => State[C](rateCounter, running = true, layout)))
      .backend[Backend[C]](scope => new Backend[C](points, canvasInitializer, pageComponent)(scope))
      .render(scope => scope.backend.render(scope.props, scope.state))
      .shouldComponentUpdate { s =>
        CallbackTo.pure {
          s.backend.key(s.currentProps.value, s.currentState) != s.backend.key(s.nextProps.value, s.nextState)
        }
      }
      // The drawing context can be updated only after the sidebar has been resized
      // That's why we have to update the drawing context after the the dom has been updated and rendered
      // TODO can we remove this?
      .componentDidUpdate(s =>
        if (s.prevState.layout != s.currentState.layout) s.backend.updateLayout
        else Callback.empty)
      .componentDidMount(s =>
        Callback {
          dom.window.addEventListener("resize", (_: Any) => s.backend.updateLayout.runNow())
      })
      .build
}
