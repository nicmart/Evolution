package evolution.app.react.component

import evolution.app.conf.Conf
import evolution.app.model.context.DrawingContext
import evolution.app.model.context.DrawingContext.CanvasSize
import evolution.app.model.counter.RateCounter
import evolution.app.model.state.RendererState
import evolution.app.model.{CodeCompiler, Drawing, TermBasedCodeCompiler}
import evolution.app.react.component.presentational._
import evolution.app.react.pages._
import evolution.app.react.underware.SnapshotUnderware
import evolution.compiler.phases.FullCompiler
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.predicates.UnifyPredicates
import evolution.compiler.phases.typer.{PredicatesSolverTyper, RecursiveTyper}
import evolution.compiler.term.{RegisterBasedInterpreter, TreeToTermCompiler}
import evolution.geometry.Point
import evolution.logging.NoOpLogger
import japgolly.scalajs.react.component.Scala.{BackendScope, Component}
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.{Callback, CallbackTo, CtorType, ScalaComponent}
import org.scalajs.dom

object App {

  type ReactComponent = Component[StateSnapshot[PageState], State, Backend, CtorType.Props]

  case class State(
      pointRateCounter: RateCounter,
      running: Boolean,
      layout: LayoutState,
      compilationResult: Option[CompilationResult],
      selectedDrawing: Option[Drawing],
      id: Int
  ) {
    def drawingContext: DrawingContext = layout.drawingContext
    def withWindowSize(size: Point): State = copy(layout = layout.copy(windowSize = size))
    def next: State = copy(id = id + 1)
  }

  case class CompilationResult(key: Int, result: Either[String, Iterator[Point]])

  case class LayoutState(sidebarWidth: Double, windowSize: Point, sidebarExpanded: Boolean) {
    private val canvasWidth: Double = if (sidebarExpanded) windowSize.x - sidebarWidth else windowSize.x
    val drawingContext: DrawingContext =
      DrawingContext(CanvasSize(canvasWidth.toInt, windowSize.y.toInt))
  }

  class Backend(pageComponent: Page.ReactComponent)(bs: BackendScope[StateSnapshot[PageState], State]) {
    def render(pageStateSnapshot: StateSnapshot[PageState], state: State): VdomElement = {
      val stateSnapshot = SnapshotUnderware.simpleSnapshot(state)(s => bs.setState(s))
      val drawingStateSnapshot = pageStateSnapshot.zoomState(_.drawingState)(
        drawingState => pageState => pageState.copy(drawingState = drawingState)
      )
      val layoutSnapshot =
        SnapshotUnderware.simpleSnapshot(state.layout)(layout => bs.modState(_.copy(layout = layout)))
      val renderingStateSnapshot = pageStateSnapshot.zoomState(_.rendererState)(
        renderingState => pageState => pageState.copy(rendererState = renderingState)
      )
      val selectedDrawingSnapshot =
        SnapshotUnderware.simpleSnapshot(state.selectedDrawing) { s =>
          bs.setState(
            state.copy(selectedDrawing = s),
            pageStateSnapshot.setStateOption(s.map(PageState.fromDrawing))
          )
        }

      pageComponent(
        Page.Props(
          running = stateSnapshot.zoomState(_.running)(isPlaying => state => state.copy(running = isPlaying)),
          layout = layoutSnapshot,
          rendererState = renderingStateSnapshot,
          compilationResult = state.compilationResult.map(_.result).getOrElse(Left("Not compiled yet")),
          drawingState = drawingStateSnapshot,
          selectedDrawing = selectedDrawingSnapshot,
          pointRate = state.pointRateCounter.rate.toInt,
          onShuffle = drawingStateSnapshot.modState(_.withNewSeed),
          onReload = bs.modState(_.next),
          onFrameDraw = onRateCountUpdate(pageStateSnapshot.value.rendererState),
          id = state.id
        )
      ).vdomElement
    }

    private[App] def key(p: PageState, s: State): Int =
      (s.pointRateCounter.rate, p, s.running, s.layout, s.selectedDrawing, evolutionKey(p, s)).hashCode()

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

  private def evolutionKey(props: PageState, state: State) =
    (props.drawingState.code, props.drawingState.seed, state.layout.drawingContext, state.id, props.rendererState)
      .hashCode()

  private def codeCompiler(pageState: PageState): CodeCompiler = {
    println(s"materializer is ${pageState.materializer}")
    new TermBasedCodeCompiler(
      new FullCompiler(
        FastParseParser,
        new PredicatesSolverTyper(
          new RecursiveTyper,
          new UnifyPredicates(NoOpLogger)
        ),
        new TreeToTermCompiler,
        new RegisterBasedInterpreter,
        Conf.logger
      )
    )
  }

  private def compile(pageState: PageState, state: State): CompilationResult =
    CompilationResult(
      evolutionKey(pageState, state),
      codeCompiler(pageState).compile(
        pageState.drawingState.code,
        pageState.drawingState.seed,
        state.layout.drawingContext * pageState.rendererState.resolutionFactor
      )
    )

  private def recompile(props: StateSnapshot[PageState], state: State): Option[State] =
    if (needsCompilation(props, state))
      Some(state.copy(compilationResult = Some(compile(props.value, state))))
    else
      None

  private def needsCompilation(props: StateSnapshot[PageState], state: State): Boolean =
    !state.compilationResult.exists(cr => cr.key == evolutionKey(props.value, state))

  def component(
      rateCounter: RateCounter,
      pageComponent: Page.ReactComponent
  ) =
    ScalaComponent
      .builder[StateSnapshot[PageState]]("App")
      .initialStateCallback(
        initialLayout.map(layout => State(rateCounter, running = true, layout, None, None, 1))
      )
      .backend[Backend](scope => new Backend(pageComponent) (scope))
      .render(scope => scope.backend.render(scope.props, scope.state))
      .shouldComponentUpdate { s =>
        CallbackTo.pure {
          s.backend.key(s.currentProps.value, s.currentState) != s.backend.key(s.nextProps.value, s.nextState)
        }
      }
      // TODO we try to compile only when it is necessary. I did not find a cleaner way to do this
      .getDerivedStateFromPropsOption(recompile _)
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
