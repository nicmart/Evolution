package evolution.app.react.component.config

import evolution.app.react.component.config.ConfigComponent.instance
import evolution.app.react.component.config.instances.empty
import evolution.drawing.algebra.Drawing
import evolution.drawing.algebra.interpreter.Serializer
import evolution.drawing.algebra.parser.DrawingParser.PointDrawingParser
import evolution.geometry.Point
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

object DrawingDSLComponent {
  def apply(component: ConfigComponent[String]): ConfigComponent[Drawing[Unit, Point]] =
    instance("drawing config") { (props, children) =>
      val stringSnapshot = props.zoomState[String](
        drawing => drawing.run(Serializer)(Nil))(
        serialized => currDrawing => PointDrawingParser.parse(serialized).fold(
          error => { println(error); currDrawing },
          identity
        )
      )
      component(stringSnapshot)()
    }
}
