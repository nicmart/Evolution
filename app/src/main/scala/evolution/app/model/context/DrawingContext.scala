package evolution.app.model.context

import evolution.geometry.Point

final case class DrawingContext(
  canvasSize: DrawingContext.CanvasSize
) {
  def right: Double = canvasSize.width / 2
  def top: Double = canvasSize.height / 2
  def left: Double = -right
  def bottom: Double = -top
}

object DrawingContext {

  final case class CanvasSize(width: Int, height: Int) {
    def point: Point = Point(width.toDouble, height.toDouble)
  }

}