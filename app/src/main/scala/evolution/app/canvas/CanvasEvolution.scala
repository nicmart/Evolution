package evolution.app.canvas

import org.scalajs.dom.raw.CanvasRenderingContext2D
import paint.evolution.Evolution
import paint.geometry.Geometry.Point

/**
  * Created by Nicolò Martini on 17/05/2017.
  */
object CanvasEvolution {
    var drawedPoints: Long = 0
    var lastDebugTime: Double = scala.scalajs.js.Date.now()
    var interval = 1000

    def drawPointEvolution(size: Double, pointEv: Evolution[Point]): Evolution[CanvasRenderingContext2D => Unit] =
        pointEv.map { point =>
            { context =>
                drawPoint(point, size, context)
            }
        }

    def drawPointsEvolution(
        size: Double,
        pointsEv: Evolution[List[Point]]
    ): Evolution[CanvasRenderingContext2D => Unit] = {
        pointsEv.map { points =>
            context =>
                points.foreach(drawPoint(_, size, context))
        }
    }

    private def drawPoint(point: Point, size: Double, context: CanvasRenderingContext2D): Unit = {
        drawedPoints += 1
        val now = scala.scalajs.js.Date.now()
        if (now - lastDebugTime > interval) {
            lastDebugTime = now
            println(s"Points: ${drawedPoints}")
            drawedPoints = 0
        }
        //context.lineWidth = size
        //context.strokeStyle = "white"
        context.beginPath()
        context.lineTo(point.x.toInt, point.y.toInt)
        context.stroke()
    }
}
