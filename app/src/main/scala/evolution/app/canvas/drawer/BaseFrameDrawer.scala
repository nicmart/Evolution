package evolution.app.canvas.drawer

import evolution.app.model.context.DrawingContext
import evolution.geometry.Point
import org.scalajs.dom
import org.scalajs.dom.raw.CanvasRenderingContext2D

trait FrameDrawer {
  def drawFrame(context: dom.CanvasRenderingContext2D, pointStream: Stream[Point]): Stream[Point]
}

final case class BaseFrameDrawer(
  iterations: Int,
  pointDrawer: PointDrawer,
  drawingContext: DrawingContext
) extends FrameDrawer {
  @inline def drawFrame(context: dom.CanvasRenderingContext2D, pointStream: Stream[Point]): Stream[Point] = {
    //clearCanvas(context)
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

  @inline private def clearCanvas(context: dom.CanvasRenderingContext2D): Unit = {
    context.beginPath
    context.rect(0, 0, drawingContext.canvasSize.width, drawingContext.canvasSize.height)
    val prefFillStyle = context.fillStyle
    context.fillStyle = "rgba(0,0,0,0.12)"
    context.fill()
    context.fillStyle = prefFillStyle
  }
}
