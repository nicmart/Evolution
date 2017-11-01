package evolution.app.react.component

import evolution.app.canvas.Drawer
import evolution.app.model.context.DrawingContext
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import org.scalajs.dom
import evolution.geometry.Point

object CanvasComponent {

  case class Props(
    context: DrawingContext,
    canvasInitializer: dom.html.Canvas => Unit,
    drawer: Drawer,
    points: Stream[Point],
    onFrameDidDraw: Callback
  )

  class Backend(bs: BackendScope[Props, Unit]) {
    var running = true
    def render(props: Props): VdomElement = {
      val size = props.context.canvasSize.point
      <.canvas(
        ^.width := (size.x / 2).toString,
        ^.height := (size.y / 2).toString,
        VdomAttr("width") := size.x.toString,
        VdomAttr("height") := size.y.toString
      )
    }

    def tick(props: Props, ctx: dom.CanvasRenderingContext2D)(points: Stream[Point]): Unit = {
      if (running) {
        val nextPoints: Stream[Point] = props.drawer.draw(ctx, points)
        props.onFrameDidDraw.runNow()
        dom.window.requestAnimationFrame(_ => tick(props, ctx)(nextPoints))
      }
    }

    def onMount(canvas: dom.html.Canvas, props: Props): Callback = Callback {
      val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      props.canvasInitializer(canvas)
      dom.window.requestAnimationFrame(_ => tick(props, ctx)(props.points))
    }
  }

  val component =
    ScalaComponent.builder[Props]("Canvas")
      .renderBackend[Backend]
      .componentDidMount(s =>
        s.backend.onMount(s.getDOMNode.asInstanceOf[dom.html.Canvas], s.props)
      )
      .componentWillUnmount(s => Callback {
        s.backend.running = false
      }
      )
      .build
}
