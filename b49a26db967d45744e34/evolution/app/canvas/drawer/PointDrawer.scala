package evolution.app.canvas.drawer

import evolution.app.model.context.DrawingContext
import org.scalajs.dom.raw.CanvasRenderingContext2D

trait PointDrawer {
  def drawPoint(x: Double, y: Double, canvasContext: CanvasRenderingContext2D): Unit
}

final case class FillRectPointDrawer(size: Int) extends PointDrawer {
  @inline override def drawPoint(x: Double, y: Double, context: CanvasRenderingContext2D): Unit =
    context.fillRect(x, y, size, size)
}

case class TorusPlaneDrawer(drawer: PointDrawer, drawingContext: DrawingContext) extends PointDrawer {
  private val width = drawingContext.canvasSize.width
  private val height = drawingContext.canvasSize.height

  @inline override def drawPoint(x: Double, y: Double, canvasContext: CanvasRenderingContext2D): Unit =
    drawer.drawPoint(
      positiveMod(x, width),
      positiveMod(y, height),
      canvasContext
    )

  @inline private def positiveMod(d: Double, m: Double): Double =
    if (d >= 0) d % m else (d % m) + m
}

case class RealProjectivePlaneDrawer(drawer: PointDrawer, drawingContext: DrawingContext) extends PointDrawer {
  private val width = drawingContext.canvasSize.width
  private val height = drawingContext.canvasSize.height

  @inline override def drawPoint(x: Double, y: Double, canvasContext: CanvasRenderingContext2D): Unit = {
    drawer.drawPoint(
      positiveMod(parity(y, height) * positiveMod(x, width), width),
      positiveMod(parity(x, width) * positiveMod(y, height), height),
      canvasContext
    )
  }

  @inline private def positiveMod(d: Double, m: Double): Double =
    if (d >= 0) d % m else (d % m) + m

  @inline private def parity(d: Double, m: Double): Double =
    if (positiveMod(d, 2 * m) < m) 1 else -1
}
