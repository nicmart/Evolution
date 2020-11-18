package evolution.app.canvas.drawer

import evolution.app.model.context.DrawingContext
import evolution.geometry.Point
import org.scalajs.dom

trait FrameDrawer {
  def drawFrame(context: dom.CanvasRenderingContext2D, points: Iterator[Point]): Unit
}

final case class BaseFrameDrawer(
    drawingContext: DrawingContext,
    iterations: Int,
    pointDrawer: PointDrawer
) extends FrameDrawer {
  private val offsetX = drawingContext.canvasSize.width / 2
  private val offsetY = drawingContext.canvasSize.height / 2

  @inline override def drawFrame(context: dom.CanvasRenderingContext2D, pointStream: Iterator[Point]): Unit =
    pointStream
      .take(iterations)
      .foreach(
        point => pointDrawer.drawPoint(offsetX + point.x, offsetY - point.y, context)
      )
}
