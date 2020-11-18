package evolution.app.react.component

import cats.effect.IO
import evolution.app.canvas.drawer.FrameDrawer
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.RendererState
import evolution.geometry.Point
import fs2.Stream
import japgolly.scalajs.react.component.Scala.Component
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.{Callback, CtorType, ScalaComponent}
import org.scalajs.dom

object Canvas {

  type ReactComponent = Component[Props, Unit, Backend, CtorType.Props]
  val canvasId = "drawing-canvas"

  case class Props(
      context: DrawingContext,
      canvasInitializer: dom.html.Canvas => Unit,
      rendererState: RendererState,
      points: Option[Stream[IO, Point]],
      onFrameDidDraw: Callback,
      running: Boolean
  )

  class Backend(drawerFromState: (RendererState, DrawingContext) => FrameDrawer) {
    var running = false
    var stopPending = false
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

//    def tick(props: Props, ctx: dom.CanvasRenderingContext2D, drawer: FrameDrawer): Unit = {
//      if (runNext()) {
//        points = drawer.drawFrame(ctx, points)
//        props.onFrameDidDraw.runNow()
//        dom.window.requestAnimationFrame(_ => tick(props, ctx, drawer))
//      } else {
//        running = false
//        stopPending = false
//      }
//    }

    // TODO chunk drawings and parametrise iterations
    private def drawStream(props: Props, ctx: dom.CanvasRenderingContext2D, drawer: FrameDrawer): Stream[IO, Unit] =
      points.chunkN(1000).evalMap(chunk => draw(props, drawer.drawFrame(ctx, chunk.iterator)))

    private def draw(props: Props, drawing: => Unit): IO[Unit] =
      IO.async(
        cb =>
          dom.window.requestAnimationFrame { _ =>
            props.onFrameDidDraw.runNow()
            cb(Right(drawing))
          }
      )

    def toggleRunning(node: dom.Element, props: Props): Callback = Callback {
      if (props.running) start(node, props)
      else scheduleStop()
    }

    def onMount(node: dom.Element, props: Props): Callback = Callback {
      props.canvasInitializer(canvas(node))
      props.points.foreach { ps =>
        points = ps
        start(node, props)
      }
    }

    def start(node: dom.Element, props: Props): Unit = {
      val drawer = drawerFromState(props.rendererState, props.context * props.rendererState.resolutionFactor)
      if (!running && props.running) {
        running = true
        stopPending = false
        //dom.window.requestAnimationFrame(_ => tick(props, canvasContext(node), drawer))
        drawStream(props, canvasContext(node), drawer).compile.drain.unsafeRunAsyncAndForget()
      }
    }

    def scheduleStop(): Unit = {
      stopPending = true
    }

    def runNext(): Boolean =
      running && !stopPending

    def canvas(node: dom.Element): dom.html.Canvas =
      node.asInstanceOf[dom.html.Canvas]

    def canvasContext(node: dom.Element): dom.CanvasRenderingContext2D =
      canvas(node).getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  }

  def component(drawerFromState: (RendererState, DrawingContext) => FrameDrawer) =
    ScalaComponent
      .builder[Props]("Canvas")
      .backend[Backend](_ => new Backend(drawerFromState))
      .render(s => s.backend.render(s.props))
      .componentDidMount { s =>
        s.backend.onMount(s.getDOMNode.asElement, s.props)
      }
      .componentWillUnmount(
        s =>
          Callback {
            s.backend.scheduleStop()
          }
      )
      .componentWillReceiveProps(s => s.backend.toggleRunning(s.getDOMNode.asElement, s.nextProps))
      .build
}
