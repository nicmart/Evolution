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
    sidebarExpanded: Boolean,
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
          running = stateSnapshot.zoomState(_.running)(isPlaying => state => state.copy(running = isPlaying)),
          drawingContext = stateSnapshot.zoomState(_.drawingContext)(newContext =>
            state => state.copy(drawingContext = newContext)),
          rendererState = renderingStateSnapshot,
          points = Eval.later(points(state.drawingContext, pageStateSnapshot.value.drawingState)),
          drawingState = drawingStateSnapshot,
          pointRate = state.pointRateCounter.rate.toInt,
          onRefresh = drawingStateSnapshot.modState(_.withNewSeed),
          onFrameDraw = onRateCountUpdate(pageStateSnapshot.value.rendererState),
          sidebarExpanded = stateSnapshot.zoomState(_.sidebarExpanded)(isExpanded =>
            pageState => pageState.copy(sidebarExpanded = isExpanded))
        ))
    }

    private[App] def key(p: PageState[C], s: State[C]): Int =
      (s.pointRateCounter.rate, p, s.running, s.drawingContext, s.sidebarExpanded).hashCode()

    private def onRateCountUpdate(rendererState: RendererState): Callback =
      bs.modState { state =>
        state.copy(pointRateCounter = state.pointRateCounter.count(rendererState.iterations))
      }

    def updateDrawingContext: Callback =
      drawingContextCallback.flatMap { ctx =>
        bs.modState(s => s.copy(drawingContext = ctx))
      }
  }

  private def drawingContextCallback: CallbackTo[DrawingContext] = CallbackTo {
    val element = dom.window.document.documentElement
    DrawingContext(
      DrawingContext.CanvasSize(
        2 * element.clientWidth * 2 / 3,
        2 * element.clientHeight
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
      .initialStateCallback(drawingContextCallback.map(ctx =>
        State[C](ctx, rateCounter, sidebarExpanded = true, running = true)))
      .backend[Backend[C]](scope => new Backend[C](points, canvasInitializer, pageComponent)(scope))
      .render(scope => scope.backend.render(scope.props, scope.state))
      .shouldComponentUpdate { s =>
        CallbackTo.pure {
          s.backend.key(s.currentProps.value, s.currentState) != s.backend.key(s.nextProps.value, s.nextState)
        }
      }
      // The drawing context can be updated only after the sidebar has been resized
      // That's why we have to update the drawing context after the the dom has been updated and rendered
      .componentDidUpdate(s =>
        if (s.prevState.sidebarExpanded != s.currentState.sidebarExpanded) s.backend.updateDrawingContext
        else Callback.empty)
      .componentDidMount(s =>
        Callback {
          dom.window.addEventListener("resize", (_: Any) => s.backend.updateDrawingContext.runNow())
      })
      .build
}
