package evolution.app.react.component

import evolution.app.canvas.EvolutionDrawer
import evolution.app.model.legacy.Drawing
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.component.Scala.BackendScope
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import paint.geometry.Geometry.Point

object CanvasComponent {
    case class Props(
        canvasInitializer: dom.html.Canvas => Unit,
        currentDrawing: Drawing[Point],
        drawer: EvolutionDrawer,
        windowSize: Point
    )

    class Backend(bs: BackendScope[Props, Unit]) {
        var stopAnimationCallback: Callback = Callback.empty

        def render(props: Props): VdomElement = {
            val size = props.windowSize
            <.canvas(
                ^.width := size.x.toString,
                ^.height := size.y.toString,
                VdomAttr("width") := (2 * size.x).toString,
                VdomAttr("height") := (2 * size.y).toString
            )
        }

        def onMount(canvas: dom.html.Canvas, props: Props): Callback = Callback {
            props.canvasInitializer(canvas)
            val cancelAnimationCallback = props.drawer.animationCallback(canvas, props.currentDrawing.evolution(props.windowSize * 2))
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
