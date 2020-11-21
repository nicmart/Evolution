package evolution.app.react.component

import cats.effect.{ContextShift, IO, Timer}
import evolution.app.canvas.drawer.FrameDrawer
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.RendererState
import evolution.geometry.Point
import fs2.Stream
import japgolly.scalajs.react.component.Scala.{BackendScope, Component}
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, CtorType, ScalaComponent}
import org.scalajs.dom

object Canvas {

  type ReactComponent = Component[Props, Unit, Backend, CtorType.Props]
  val canvasId = "drawing-canvas"

  import scala.concurrent.ExecutionContext
  import scala.concurrent.duration._

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  case class Props(
      context: DrawingContext,
      canvasInitializer: dom.html.Canvas => Unit,
      rendererState: RendererState,
      points: Option[Stream[IO, Point]],
      onFrameDidDraw: Callback,
      running: Boolean
  )

  class Backend(drawerFromState: (RendererState, DrawingContext) => FrameDrawer)(scope: BackendScope[Props, Unit]) {
    println("Creating a CANVAS Backend instance")

    private var stopPending = false

    private def toMeteredStream[T](t: IO[T]): Stream[IO, T] =
      Stream.eval(t) ++ Stream.repeatEval(t).metered(10.milliseconds)

    private val runningStream: Stream[IO, Boolean] =
      toMeteredStream(IO(scope.props.runNow().running))

    private val stopPendingStream: Stream[IO, Boolean] =
      toMeteredStream(IO(stopPending))

    def scheduleStop(): Unit = {
      stopPending = true
    }

    def render(props: Props): VdomElement = {
      val size = props.context.canvasSize.point
      val retinaSize = props.context.canvasSize.point * props.rendererState.resolutionFactor
      <.canvas(
        ^.id := canvasId,
        ^.width := size.x.toString + "px",
        ^.height := size.y.toString + "px",
        VdomAttr("width") := retinaSize.x.toString,
        VdomAttr("height") := retinaSize.y.toString
      )
    }

    // TODO chunk drawings and parametrise iterations
    private def drawStream(props: Props, ctx: dom.CanvasRenderingContext2D, drawer: FrameDrawer): Stream[IO, Unit] =
      props.points
        .getOrElse(Stream.empty)
        .chunkN(1000)
        .unchunk
        .pauseWhen(runningStream.map(!_))
        .interruptWhen(stopPendingStream)
        .evalMap(chunk => draw(props, drawer.drawFrame(ctx, chunk.iterator)))

    private def draw(props: Props, drawing: => Unit): IO[Unit] =
      IO.async(
        cb =>
          dom.window.requestAnimationFrame { _ =>
            props.onFrameDidDraw.runNow()
            cb(Right(drawing))
          }
      )

    def onMount(node: dom.Element, props: Props): Callback = Callback {
      stopPending = false
      props.canvasInitializer(canvas(node))
      startDrawingStream(node, props)
    }

    def startDrawingStream(node: dom.Element, props: Props): Unit = {
      val drawer = drawerFromState(props.rendererState, props.context * props.rendererState.resolutionFactor)
      drawStream(props, canvasContext(node), drawer).compile.drain.unsafeRunAsyncAndForget()
    }

    def canvas(node: dom.Element): dom.html.Canvas =
      node.asInstanceOf[dom.html.Canvas]

    def canvasContext(node: dom.Element): dom.CanvasRenderingContext2D =
      canvas(node).getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  }

  def component(drawerFromState: (RendererState, DrawingContext) => FrameDrawer) =
    ScalaComponent
      .builder[Props]("Canvas")
      .backend[Backend](new Backend(drawerFromState) (_))
      .render(s => s.backend.render(s.props))
      .componentDidMount(s => s.backend.onMount(s.getDOMNode.asElement, s.props))
      .componentWillUnmount(s => Callback(s.backend.scheduleStop()))
      .build
}
