package evolution.app.canvas.drawer

import evolution.geometry.Point
import org.scalajs.dom
import org.scalajs.dom.raw.CanvasRenderingContext2D

trait FrameDrawer {
  def drawFrame(context: dom.CanvasRenderingContext2D, pointStream: Stream[Point]): Stream[Point]
}

final case class BaseFrameDrawer(
  iterations: Int,
  pointDrawer: PointDrawer
) extends FrameDrawer {
  @inline def drawFrame(context: dom.CanvasRenderingContext2D, pointStream: Stream[Point]): Stream[Point] = {
    var currentStream = pointStream
    (1 to iterations).foreach { _ =>
      currentStream = drawAndNext(currentStream, context)
    }
    currentStream
  }

  @inline private def drawAndNext(points: Stream[Point], context: CanvasRenderingContext2D): Stream[Point] = {
    drawPoint(points.head, context)
    points.tail
  }

  @inline private def drawPoint(point: Point, context: CanvasRenderingContext2D): Unit = {
    pointDrawer.drawPoint(point.x, point.y, context)
  }
}
