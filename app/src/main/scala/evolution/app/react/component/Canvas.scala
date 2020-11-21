package evolution.app.react.component

import cats.effect.{ContextShift, IO, Timer}
import evolution.app.canvas.drawer.FrameDrawer
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.RendererState
import evolution.geometry.Point
import fs2.Stream
import japgolly.scalajs.react.component.Scala.Component
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

  class Backend(drawerFromState: (RendererState, DrawingContext) => FrameDrawer) {
    println("Creating a CANVAS Backend instance")
    private var running = false
    private var stopPending = false

    private val refresh = 10.milliseconds

    private def toMeteredStream[T](t: IO[T]): Stream[IO, T] =
      Stream.eval(t) ++ Stream.repeatEval(t).metered(refresh)

    private val runningStream: Stream[IO, Boolean] =
      toMeteredStream(IO(running))

    private val stopPendingStream: Stream[IO, Boolean] =
      toMeteredStream(IO(stopPending))

    def scheduleStop(): Unit = {
      stopPending = true
    }

    // TODO this should not exist and points should be injected from the props
    var points: Stream[IO, Point] = Stream.empty

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
      points
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

    def toggleRunning(props: Props): Callback = Callback { running = props.running }

    def onMount(node: dom.Element, props: Props): Callback = Callback {
      stopPending = false
      running = props.running
      props.canvasInitializer(canvas(node))
      props.points.foreach { ps =>
        points = ps
        startDrawingStream(node, props)
      }
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
      .backend[Backend](s => new Backend(drawerFromState))
      .render(s => s.backend.render(s.props))
      .componentDidMount(s => s.backend.onMount(s.getDOMNode.asElement, s.props))
      .componentWillUnmount(s => Callback(s.backend.scheduleStop()))
      .componentDidUpdate(s => s.backend.toggleRunning(s.currentProps))
      .build
}
