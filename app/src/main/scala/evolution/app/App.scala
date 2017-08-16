package evolution.app

import evolution.app.conf.Conf

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom._
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import paint.random.{RNG, SequenceRNG, SimpleRNG}

import scala.util.Random
import evolution.app.canvas.CanvasEvolution._
import evolution.app.canvas.CanvasSize
import evolution.app.model.legacy.Drawing
import evolution.app.portfolio.EvolutionPortfolio
import org.scalajs.dom.html.{Button, Canvas, Select}

@JSExport
object App {
  private var currentAnimationId: Option[Int] = None

  @JSExport
  def start(document: dom.html.Document): Unit = {
    val canvas = document.getElementById("canvas").asInstanceOf[dom.html.Canvas]
    val drawingDropdown = document.getElementById("drawing").asInstanceOf[dom.html.Select]
    val restartButton = document.getElementById("restart").asInstanceOf[dom.html.Button]

    inititalizeDrawingDropdown(canvas, drawingDropdown)
    initializeCanvas(
      canvas,
      currentEvolutionFactory(drawingDropdown)
    )

    initializeRestartButton(
      restartButton,
      drawingDropdown,
      canvas
    )
  }

  private def currentEvolutionFactory(drawingDropdown: dom.html.Select): CanvasSize => Evolution[Point] = {
    val drawingList = EvolutionPortfolio.drawingList
    drawingList.drawing(drawingDropdown.value).get.evolution
  }

  private def initializeCanvas(
    htmlCanvas: dom.html.Canvas,
    evolutionFactory: CanvasSize => Evolution[Point]
  ): Unit = {
    Conf.canvasInitializer.apply(htmlCanvas)
    val ctx = htmlCanvas.getContext("2d")
        .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.fillStyle = "white"
    ctx.strokeStyle = "white"
    ctx.lineCap = "square"
    ctx.lineJoin = "miter"
    ctx.miterLimit = 20

    val canvasSize = Point(htmlCanvas.width, htmlCanvas.height)

    val state: RNG = SimpleRNG(Random.nextLong())
    var drawingStream: Stream[CanvasRenderingContext2D => Unit] =
      drawPointEvolution(1, evolutionFactory(canvasSize)).unfold(state)

    val iterations = 1000

    def draw(t: Double): Unit = {

      for (i <- 1 to iterations) {
        drawingStream.head.apply(ctx)
        drawingStream = drawingStream.tail
      }

      currentAnimationId = Some(window.requestAnimationFrame(draw))
    }

    draw(0)
  }

  private def inititalizeDrawingDropdown(canvas: dom.html.Canvas, drawingDropdown: Select): Unit = {

    def drawingToOption[T](drawing: Drawing[T]): dom.html.Option = {
      val option = document.createElement("option").asInstanceOf[html.Option]
      option.textContent = drawing.name
      option
    }

    val drawingList = EvolutionPortfolio.drawingList
    val drawings = drawingList.drawings
    val options = for (drawing <- drawings.values) yield drawingToOption(drawing)

    options.foreach { option =>
      drawingDropdown.appendChild(option)
    }

    drawingList.selected.foreach(drawing => drawingDropdown.value = drawing.name)

    drawingDropdown.addEventListener("change", (e: dom.Event) => {
      stopCurrentAnimation()
      initializeCanvas(canvas, currentEvolutionFactory(drawingDropdown))
    })
  }

  def initializeRestartButton(restartButton: Button, drawingDropdown: Select, canvas: Canvas): Unit = {
    restartButton.addEventListener("click", (e: dom.Event) => {
      stopCurrentAnimation()
      initializeCanvas(canvas, currentEvolutionFactory(drawingDropdown))
    })
  }

  private def stopCurrentAnimation(): Unit = {
    currentAnimationId.foreach(id => window.cancelAnimationFrame(id))
  }
}
