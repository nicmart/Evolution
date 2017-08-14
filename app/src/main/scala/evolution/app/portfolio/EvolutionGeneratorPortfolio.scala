package evolution.app.portfolio

import evolution.app.canvas.CanvasSize
import evolution.app.model.{Drawing, DrawingContext, DrawingList}
import paint.evolution.Evolution
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.generator.EvolutionGenerator
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import paint.evolution.implicits._

object EvolutionGeneratorPortfolio {

    object brownian {
        case class Context(
            start: Point,
            radius: Double
        )

        val generator: EvolutionGenerator.Aux[Point, Context] =
            EvolutionGenerator { ctx: Context =>
                solveIndependent(ctx.start)(
                    rectangle2D(ctx.radius)
                ).positional
            }

        Drawing(
            "brownian",
            generator,
            Context(Point(200, 200), 2)
        )

        val defaultContext: generator.Context = Context(Point(200, 200), 2)
    }

    def drawingContext: DrawingContext[Point] =
        DrawingContext(
            DrawingList(List(
                Drawing("brownian", brownian.generator, brownian.defaultContext)
            )),
            Drawing("brownian", brownian.generator, brownian.defaultContext)
        )
}
