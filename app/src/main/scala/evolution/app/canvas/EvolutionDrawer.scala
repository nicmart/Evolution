package evolution.app.canvas

import evolution.app.canvas.CanvasEvolution.{drawPoint, drawPointEvolution, drawedPoints, lastDebugTime}
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.CanvasRenderingContext2D
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import paint.random.RNG

case class EvolutionDrawer(rng: RNG, iterations: Int, strokeSize: Int) {

  def animationCallback(canvas: Canvas, evolution: Evolution[Point]): Unit => Unit = {
    val initialStream = drawingStream(evolution)
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    var currentAnimationFrameId: Option[Int] = None

    def draw(stream: Stream[CanvasRenderingContext2D => Unit])(t: Double): Unit = {
      var currentStream = stream
      for (i <- 1 to iterations) {
        currentStream.head.apply(ctx)
        currentStream = currentStream.tail
      }
      val id = dom.window.requestAnimationFrame(draw(currentStream))
      currentAnimationFrameId = Some(id)
    }

    draw(initialStream)(0)
    (_: Unit) => currentAnimationFrameId.foreach(dom.window.cancelAnimationFrame)
  }

  def drawingStream(evolution: Evolution[Point]): Stream[CanvasRenderingContext2D => Unit] =
    drawPointEvolution(strokeSize, evolution).unfold(rng)

  private def drawPointEvolution(size: Double, pointEv: Evolution[Point]): Evolution[CanvasRenderingContext2D => Unit] =
    pointEv.map { point => { context =>
      drawPoint(point, size, context)
    }
    }

  private def drawPoint(point: Point, size: Double, context: CanvasRenderingContext2D): Unit = {
    context.lineWidth = size
    context.beginPath()
    context.lineTo(point.x.toInt, point.y.toInt)
    context.stroke()
  }
}
