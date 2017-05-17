package evolution.app

import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.html.Canvas

trait CanvasInitializer { self =>
    def initialise(canvas: dom.html.Canvas): Unit
    def andThen(next: CanvasInitializer): CanvasInitializer = new CanvasInitializer {
        override def initialise(canvas: dom.html.Canvas): Unit = {
            self.initialise(canvas)
            next.initialise(canvas)
        }
    }
}

case class FullWindowCanvasInitializer(document: Document, window: Window) extends CanvasInitializer {
    def initialise(canvas: dom.html.Canvas): Unit = {
        val (width, height) = windowSize(document, window)
        canvas.width = 2 * width
        canvas.height = 2 * height
        canvas.style.width = s"${width}px"
        canvas.style.height = s"${height}px"
    }

    private def windowSize(document: Document, window: Window) = (
        Math.max(document.documentElement.clientWidth, window.innerWidth).toInt,
        Math.max(document.documentElement.clientHeight, window.innerHeight).toInt
    )
}

case class ColorCanvasInitializer(color: String) extends CanvasInitializer {
    override def initialise(canvas: dom.html.Canvas): Unit = {
        val ctx = canvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]
        val oldFillStyle = ctx.fillStyle.toString
        ctx.fillStyle = color
        ctx.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
        ctx.fillStyle = oldFillStyle
    }
}
