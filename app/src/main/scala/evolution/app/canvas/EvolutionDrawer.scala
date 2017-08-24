package evolution.app.canvas

import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.CanvasRenderingContext2D
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import paint.random.RNG

case class EvolutionDrawer(rng: RNG, iterations: Int, strokeSize: Int) {

  def animationCallback(canvas: Canvas, evolution: Evolution[Point]): Unit => Unit = {
    val initialStream = pointStream(evolution)
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    var currentAnimationFrameId: Option[Int] = None

    def draw(stream: Stream[Point])(t: Double): Unit = {
      var currentStream = stream
      for (i <- 1 to iterations) {
        currentStream = drawAndNext(currentStream, ctx)
      }
      val id = dom.window.requestAnimationFrame(draw(currentStream))
      currentAnimationFrameId = Some(id)
    }

    draw(initialStream)(0)
    (_: Unit) => currentAnimationFrameId.foreach(dom.window.cancelAnimationFrame)
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
