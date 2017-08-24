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
    drawingContext: DrawingContext
  )

  class Backend(bs: BackendScope[Props, Unit]) {
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

    def onMount(canvas: dom.html.Canvas, props: Props): Callback = Callback {
      props.canvasInitializer(canvas)
      val cancelAnimationCallback =
        props.drawer.animationCallback(
          canvas,
          props.currentDrawing.evolution
        )
      stopAnimationCallback = Callback {
        cancelAnimationCallback()
      }
    }
  }

  val component = ScalaComponent.builder[Props]("Canvas")
      .renderBackend[Backend]
      .componentDidMount(s =>
        s.backend.onMount(s.getDOMNode.asInstanceOf[dom.html.Canvas], s.props)
      )
      .componentWillUnmount(_.backend.stopAnimationCallback)
      .build
}
