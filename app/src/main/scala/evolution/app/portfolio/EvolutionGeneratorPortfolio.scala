package evolution.app.portfolio

import evolution.app.model.{Drawing, DrawingList, DrawingListWithSelection}
import evolution.app.react.component.config.instances._
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.generator.EvolutionGenerator
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import paint.evolution.NumericEvolutions.double
import paint.evolution.motion.MotionEvolutions

object EvolutionGeneratorPortfolio {

  object brownian {

    case class Config(
      start: Point,
      radius: Double
    )

    val generator: EvolutionGenerator[Point, Config] =
      EvolutionGenerator { config: Config =>
        solveIndependent(config.start)(
          rectangle2D(config.radius)
        ).positional
      }

    val drawing = Drawing(
      "brownian",
      generator,
      ConfigComponent[Config],
      Config(Point(900, 600), 2)
    )
  }

  object brownianWithRandomJumps {

    case class Config(
      start: Point,
      radius: Double,
      jumpProbability: Double,
      jumpSize: Int
    )

    val generator: EvolutionGenerator[Point, Config] =
      EvolutionGenerator { config: Config =>
        val slowDownEvo = double.map[Int] { d =>
          if (d < config.jumpProbability) config.jumpSize else 1
        }
        MotionEvolutions.solveIndependent(config.start)(
          rectangle2D(config.radius).slowDown(slowDownEvo)
        ).positional
      }

    val drawing = Drawing(
      "brownian with random jumps",
      generator,
      ConfigComponent[Config],
      Config(
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
