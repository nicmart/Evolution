package evolution.app.canvas.drawer

import evolution.app.model.context.DrawingContext
import evolution.geometry.Point
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D

case class RGBAColor(red: Short, blue: Short, green: Short, opacity: Double) {
  def css: String = s"rgba($red, $blue, $green, $opacity)"
}

final class ClearCanvasFrameDrawer(
    drawingContext: DrawingContext,
    drawer: FrameDrawer,
    color: RGBAColor
) extends FrameDrawer {

  @inline override def drawFrame(context: CanvasRenderingContext2D, points: Iterator[Point]): Iterator[Point] = {
    clearCanvas(context)
    drawer.drawFrame(context, points)
  }

  @inline private def clearCanvas(context: dom.CanvasRenderingContext2D): Unit = {
    context.beginPath
    context.rect(0, 0, drawingContext.canvasSize.width, drawingContext.canvasSize.height)
    val prefFillStyle = context.fillStyle
    context.fillStyle = color.css
    context.fill()
    context.fillStyle = prefFillStyle
  }
}
