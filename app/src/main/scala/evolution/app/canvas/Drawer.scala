package evolution.app.canvas

import org.scalajs.dom
import org.scalajs.dom.raw.CanvasRenderingContext2D
import evolution.geometry.Point

case class Drawer(iterations: Int, strokeSize: Int) {

  def draw(context: dom.CanvasRenderingContext2D, pointStream: Stream[Point]): Stream[Point] = {
    var currentStream = pointStream
    (1 to iterations).foreach { _ =>
      currentStream = drawAndNext(currentStream, context)
    }
    currentStream
  }

  private def drawAndNext(points: Stream[Point], context: CanvasRenderingContext2D): Stream[Point] = {
    drawPoint(points.head, strokeSize, context)
    points.tail
  }

  private def drawPoint(point: Point, size: Double, context: CanvasRenderingContext2D): Unit = {
    context.lineWidth = size
    context.beginPath
    context.moveTo(point.x, point.y)
    context.lineTo(point.x + 1, point.y + 1)
    context.stroke()
  }
}
