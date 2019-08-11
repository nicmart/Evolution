package evolution.app.react.component

import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.model.counter.RateCounter
import evolution.app.react.component.presentational._
import japgolly.scalajs.react.component.Scala.{ BackendScope, Component }
import japgolly.scalajs.react.vdom.VdomElement
import org.scalajs.dom
import japgolly.scalajs.react.{ Callback, CallbackTo, CtorType, ScalaComponent }
import evolution.geometry.Point
import evolution.app.model.state.RendererState
import evolution.app.react.underware.SnapshotUnderware
import evolution.app.react.pages._
import japgolly.scalajs.react.extra.StateSnapshot

object App {

  type ReactComponent = Component[StateSnapshot[PageState], State, Backend, CtorType.Props]

  case class State(
    pointRateCounter: RateCounter,
    running: Boolean,
    layout: LayoutState,
    points: Iterator[Point]
  ) {
    def drawingContext: DrawingContext = layout.drawingContext
    def withWindowSize(size: Point): State = copy(layout = layout.copy(windowSize = size))
  }

  case class LayoutState(sidebarWidth: Double, windowSize: Point, sidebarExpanded: Boolean) {
    private val canvasWidth: Double = if (sidebarExpanded) windowSize.x - sidebarWidth else windowSize.x
    val drawingContext: DrawingContext =
      DrawingContext(CanvasSize(canvasWidth.toInt, windowSize.y.toInt))
  }

  class Backend(pageComponent: Page.ReactComponent)(bs: BackendScope[StateSnapshot[PageState], State]) {
    def render(pageStateSnapshot: StateSnapshot[PageState], state: State): VdomElement = {
      val stateSnapshot = SnapshotUnderware.simpleSnapshot(state)(s => bs.setState(s))
      val pointsSnapshot = stateSnapshot.zoomState(_.points)(points => state => state.copy(points = points))
      val drawingStateSnapshot = pageStateSnapshot.zoomState(_.drawingState)(
        drawingState => pageState => pageState.copy(drawingState = drawingState)
      )
      val layoutSnapshot =
        SnapshotUnderware.simpleSnapshot(state.layout)(layout => bs.modState(_.copy(layout = layout)))
      val renderingStateSnapshot = pageStateSnapshot.zoomState(_.rendererState)(
        renderingState => pageState => pageState.copy(rendererState = renderingState)
      )

      pageComponent(
        Page.Props(
          running = stateSnapshot.zoomState(_.running)(isPlaying => state => state.copy(running = isPlaying)),
          layout = layoutSnapshot,
          rendererState = renderingStateSnapshot,
          points = pointsSnapshot,
          drawingState = drawingStateSnapshot,
          pointRate = state.pointRateCounter.rate.toInt,
          onRefresh = drawingStateSnapshot.modState(_.withNewSeed),
          onFrameDraw = onRateCountUpdate(pageStateSnapshot.value.rendererState)
        )
      ).vdomElement
    }

    private[App] def key(p: PageState, s: State): Int =
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

  def component(
    rateCounter: RateCounter,
    pageComponent: Page.ReactComponent
  ) =
    ScalaComponent
      .builder[StateSnapshot[PageState]]("App")
      .initialStateCallback(initialLayout.map(layout => State(rateCounter, running = true, layout, Iterator.empty)))
      .backend[Backend](scope => new Backend(pageComponent)(scope))
      .render(scope => scope.backend.render(scope.props, scope.state))
      .shouldComponentUpdate { s =>
        CallbackTo.pure {
          s.backend.key(s.currentProps.value, s.currentState) != s.backend.key(s.nextProps.value, s.nextState)
        }
      }
      // The drawing context can be updated only after the sidebar has been resized
      // That's why we have to update the drawing context after the the dom has been updated and rendered
      // TODO can we remove this?
      .componentDidUpdate(
        s =>
          if (s.prevState.layout != s.currentState.layout) s.backend.updateLayout
          else Callback.empty
      )
      .componentDidMount(
        s =>
          Callback {
            dom.window.addEventListener("resize", (_: Any) => s.backend.updateLayout.runNow())
          }
      )
      .build
}
