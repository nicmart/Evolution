package evolution.app.canvas

import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.CanvasRenderingContext2D
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import paint.random.RNG

import scala.scalajs.js

case class EvolutionDrawer(rng: RNG, iterations: Int, strokeSize: Int) {

  def draw(context: dom.CanvasRenderingContext2D, pointStream: Stream[Point]): Stream[Point] = {
    var currentStream = pointStream
    for (i <- 1 to iterations) {
      currentStream = drawAndNext(currentStream, context)
    }
    currentStream
  }

  def loop[T](context: dom.CanvasRenderingContext2D, pointStream: Stream[Point], f: (Unit => T) => T): Unit = {
    def innerDraw(currStream: Stream[Point]): T = {
      val next = draw(context, currStream)
      f(_ => innerDraw(next))
    }
    f(_ => innerDraw(pointStream))
  }

  def drawAndNext(points: Stream[Point], context: CanvasRenderingContext2D): Stream[Point] = {
    drawPoint(points.head, strokeSize, context)
    points.tail
  }

  def pointStream(evolution: Evolution[Point]): Stream[Point] =
    evolution.unfold(rng)

  private def drawPoint(point: Point, size: Double, context: CanvasRenderingContext2D): Unit = {
    context.lineWidth = size
    context.beginPath()
    context.lineTo(point.x.toInt, point.y.toInt)
    context.stroke()
  }
}
