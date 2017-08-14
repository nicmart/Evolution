package evolution.app.portfolio

import evolution.app.canvas.CanvasSize
import evolution.app.model.{Drawing, DrawingListWithSelection, DrawingList}
import evolution.app.react.component.instances.BrownianComponent
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

        val drawing = Drawing(
            "brownian",
            generator,
            BrownianComponent,
            Context(Point(900, 600), 2)
        )
    }

    def listWithSelection: DrawingListWithSelection[Point] =
        DrawingListWithSelection(
            DrawingList(List(
                brownian.drawing
            )),
            brownian.drawing
        )
}
