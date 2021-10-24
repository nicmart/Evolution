package evolution.app

import org.scalajs.dom

trait CanvasInitializer extends (dom.html.Canvas => Unit):
  self =>
  def apply(canvas: dom.html.Canvas): Unit

  def andThen(next: CanvasInitializer): CanvasInitializer = new CanvasInitializer {
    override def apply(canvas: dom.html.Canvas): Unit =
      self.apply(canvas)
      next.apply(canvas)
  }

case class ColorCanvasInitializer(backgroundColor: String, color: String) extends CanvasInitializer:
  override def apply(canvas: dom.html.Canvas): Unit =
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.fillStyle = backgroundColor
    ctx.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    ctx.fillStyle = color
    ctx.strokeStyle = color
    ctx.lineCap = "square"
    ctx.lineJoin = "miter"
    ctx.miterLimit = 20
