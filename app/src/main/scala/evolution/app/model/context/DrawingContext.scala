package evolution.app.model.context

import evolution.geometry.Point

final case class DrawingContext(
  canvasSize: DrawingContext.CanvasSize
)

object DrawingContext {

  final case class CanvasSize(width: Int, height: Int) {
    def point: Point = Point(width.toDouble, height.toDouble)
  }

}