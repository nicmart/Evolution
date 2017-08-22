package evolution.app.model.context

import paint.geometry.Geometry.Point

final case class DrawingContext(
  canvasSize: DrawingContext.CanvasSize
)

object DrawingContext {
  final case class CanvasSize(width: Int, height: Int) {
    def point: Point = Point(width.toDouble, height.toDouble)
  }
}