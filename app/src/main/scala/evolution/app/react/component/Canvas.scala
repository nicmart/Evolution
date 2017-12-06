package evolution.app.react.component

import evolution.app.canvas.drawer.BaseFrameDrawer
import evolution.app.model.context.DrawingContext
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import org.scalajs.dom
import evolution.geometry.Point

object Canvas {

  case class Props(
    context: DrawingContext,
    canvasInitializer: dom.html.Canvas => Unit,
    drawer: BaseFrameDrawer,
    points: Stream[Point],
    onFrameDidDraw: Callback,
    running: Boolean
  )

  class Backend(bs: BackendScope[Props, Unit]) {
    var running = false
    var stopPending = false
    var points: Stream[Point] = Stream.empty

    def render(props: Props): VdomElement = {
      val size = props.context.canvasSize.point
      <.canvas(
        ^.width := (size.x / 2).toString,
        ^.height := (size.y / 2).toString,
        VdomAttr("width") := size.x.toString,
        VdomAttr("height") := size.y.toString
      )
    }

    def tick(props: Props, ctx: dom.CanvasRenderingContext2D): Unit = {
      if (runNext()) {
          points = props.drawer.drawFrame(ctx, points)
          props.onFrameDidDraw.runNow()
          dom.window.requestAnimationFrame(_ => tick(props, ctx))
      } else {
        running = false
        stopPending = false
      }
    }

    def toggleRunning(node: dom.Element, props: Props): Callback = Callback {
      if (props.running) start(node, props)
      else scheduleStop()
    }

    def onMount(node: dom.Element, props: Props): Callback = Callback {
      props.canvasInitializer(canvas(node))
      points = props.points
      start(node, props)
    }

    def start(node: dom.Element, props: Props): Unit = {
      println("starting")
      if (!running && props.running) {
        println("really starting")
        running = true
        stopPending = false
        dom.window.requestAnimationFrame(_ => tick(props, canvasContext(node)))
      }
    }

    def scheduleStop(): Unit = {
      println("stopping")
      stopPending = true
    }

    def runNext(): Boolean =
      running && !stopPending

    def canvas(node: dom.Element): dom.html.Canvas =
      node.asInstanceOf[dom.html.Canvas]

    def canvasContext(node: dom.Element): dom.CanvasRenderingContext2D =
      canvas(node).getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  }

  val component =
    ScalaComponent.builder[Props]("Canvas")
      .renderBackend[Backend]
      .componentDidMount { s =>
        s.backend.onMount(s.getDOMNode, s.props)
      }
      .componentWillUnmount(s => Callback {
        s.backend.scheduleStop()
      })
      .componentWillReceiveProps(s => s.backend.toggleRunning(s.getDOMNode, s.nextProps))
      .build
}
