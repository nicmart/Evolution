package evolution.app

import evolution.app.conf.Conf

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import paint.random.{RNG, SequenceRNG, SimpleRNG}

import scala.util.Random
import paint.evolution.PointEvolutions._
import paint.evolution.NumericEvolutions._
import paint.evolution.Evolution._
import evolution.app.canvas.CanvasEvolution._
import evolution.app.model.Drawing
import evolution.app.portfolio.EvolutionPortfolio
import org.scalajs.dom.html.Select

@JSExport
object App {

    @JSExport
    def start(document: dom.html.Document): Unit = {
        val canvas = document.getElementById("canvas").asInstanceOf[dom.html.Canvas]
        initializeCanvas(canvas)

        val drawingDropdown = document.getElementById("drawing").asInstanceOf[dom.html.Select]
        inititalizeDrawingDropdown(drawingDropdown)
    }

    private def initializeCanvas(htmlCanvas: dom.html.Canvas): Unit = {
        Conf.canvasInitializer.initialise(htmlCanvas)
        val ctx = htmlCanvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]

        ctx.fillStyle = "white"
        ctx.strokeStyle = "white"
        ctx.lineCap = "square"
        ctx.lineJoin = "miter"
        ctx.miterLimit = 20

        val canvasSize = Point(htmlCanvas.width, htmlCanvas.height)

        val state: RNG = SimpleRNG(Random.nextLong())
        //var state: RNG = SequenceRNG(0)
        var drawingStream: Stream[CanvasRenderingContext2D => Unit] =
            drawPointEvolution(1, EvolutionPortfolio.current(canvasSize)).unfold(state)

        val iterations = 1000

        def draw(t: Double): Unit = {

            for (i <- 1 to iterations) {
                drawingStream.head.apply(ctx)
                drawingStream = drawingStream.tail
            }

            window.requestAnimationFrame(draw)
        }

        draw(0)
    }

    private def inititalizeDrawingDropdown(drawingDropdown: Select): Unit = {

        def drawingToOption[T](drawing: Drawing[T]): dom.html.Option = {
            val option = document.createElement("option").asInstanceOf[html.Option]
            option.textContent = drawing.name
            option
        }

        val drawings = EvolutionPortfolio.drawingList.drawings
        val options = for (drawing <- drawings.values) yield drawingToOption(drawing)

        options.foreach { option =>
          drawingDropdown.appendChild(option)
        }
    }


}
