package evolution.app.canvas.drawer

import evolution.app.model.context.DrawingContext
import evolution.geometry.Point
import org.scalajs.dom
import org.scalajs.dom.raw.CanvasRenderingContext2D

trait FrameDrawer {
  def drawFrame(context: dom.CanvasRenderingContext2D, pointStream: Iterator[Point]): Iterator[Point]
}

final case class BaseFrameDrawer(
  drawingContext: DrawingContext,
  iterations: Int,
  pointDrawer: PointDrawer
) extends FrameDrawer {
  private val offsetX = drawingContext.canvasSize.width / 2
  private val offsetY = drawingContext.canvasSize.height / 2

  @inline def drawFrame(context: dom.CanvasRenderingContext2D, pointStream: Iterator[Point]): Iterator[Point] = {
    var currentStream = pointStream
    if (currentStream.hasNext) {
      (1 to iterations).foreach { _ =>
        currentStream = drawAndNext(currentStream, context)
      }
    }
    currentStream
  }

  @inline private def drawAndNext(points: Iterator[Point], context: CanvasRenderingContext2D): Iterator[Point] = {
    if (points.hasNext) drawPoint(points.next(), context)
    points
  }

  @inline private def drawPoint(point: Point, context: CanvasRenderingContext2D): Unit = {
    pointDrawer.drawPoint(offsetX + point.x, offsetY - point.y, context)
  }
}
