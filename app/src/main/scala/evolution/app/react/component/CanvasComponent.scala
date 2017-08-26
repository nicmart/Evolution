package evolution.app.react.component

import evolution.app.canvas.EvolutionDrawer
import evolution.app.model.configured.ConfiguredDrawing
import evolution.app.model.context.DrawingContext
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import paint.geometry.Geometry.Point

object CanvasComponent {

  case class Props(
    canvasInitializer: dom.html.Canvas => Unit,
    currentDrawing: ConfiguredDrawing[Point],
    drawer: EvolutionDrawer,
    drawingContext: DrawingContext,
    onFrameDidDraw: Callback
  )

  case class MutableState(var running: Boolean)

  class Backend(bs: BackendScope[Props, MutableState]) {
    var stopAnimationCallback: Callback = Callback.empty

    def render(props: Props): VdomElement = {
      val size = props.drawingContext.canvasSize.point
      <.canvas(
        ^.width := (size.x / 2).toString,
        ^.height := (size.y / 2).toString,
        VdomAttr("width") := size.x.toString,
        VdomAttr("height") := size.y.toString
      )
    }

    def animationCallback(state: MutableState, props: Props)(action: Unit => Unit): Unit = {
      if (state.running) {
        dom.window.requestAnimationFrame(_ => action())
        props.onFrameDidDraw.runNow()
      }
    }

    def onMount(canvas: dom.html.Canvas, props: Props, state: MutableState): Callback = Callback {
      props.canvasInitializer(canvas)
      val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      val initialStream = props.drawer.pointStream(props.currentDrawing.evolution)
      props.drawer.loop[Unit](
        ctx,
        initialStream,
        animationCallback(state, props)
      )
    }
  }

  val component =
    ScalaComponent.builder[Props]("Canvas")
      .initialState(MutableState(true))
      .renderBackend[Backend]
      .componentDidMount(s =>
        s.backend.onMount(s.getDOMNode.asInstanceOf[dom.html.Canvas], s.props, s.state)
      )
      .componentWillUnmount(s => Callback { s.state.running = false })
      .build
}
