package evolution.app.portfolio

import evolution.app.canvas.CanvasSize
import evolution.app.model.{Drawing, DrawingList, DrawingListWithSelection}
import evolution.app.react.component.EvolutionContextComponent
import evolution.app.react.component.instances.BrownianComponent
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.generator.EvolutionGenerator
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import evolution.app.react.component.instances._
import paint.evolution.Evolution
import paint.evolution.NumericEvolutions.double
import paint.evolution.motion.MotionEvolutions

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
      EvolutionContextComponent[Context],
      Context(Point(900, 600), 2)
    )
  }

  object brownianWithRandomJumps {

    case class Context(
      start: Point,
      radius: Double,
      jumpProbability: Double,
      jumpSize: Int
    )

    val generator: EvolutionGenerator.Aux[Point, Context] =
      EvolutionGenerator { ctx: Context =>
        val jumpProbability = ctx.jumpProbability
        MotionEvolutions.solveIndependent(ctx.start)(
          rectangle2D(ctx.radius).slowDown(double.map[Int](d => if (d < jumpProbability) ctx.jumpSize else 1))
        ).positional
      }

    val drawing = Drawing(
      "brownian with random jumps",
      generator,
      EvolutionContextComponent[Context],
      Context(
        Point(900, 600),
        1,
        0.0001,
        200
      )
    )
  }

  def listWithSelection: DrawingListWithSelection[Point] =
    DrawingListWithSelection(
      DrawingList(List(
        brownian.drawing,
        brownianWithRandomJumps.drawing
      )),
      brownian.drawing
    )
}
