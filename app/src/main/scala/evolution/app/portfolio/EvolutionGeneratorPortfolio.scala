package evolution.app.portfolio

import evolution.app.model.{Drawing, DrawingList, DrawingListWithSelection}
import evolution.app.react.component.settings.instances._
import evolution.app.react.component.settings.SettingsComponent
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.generator.EvolutionGenerator
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import paint.evolution.NumericEvolutions.double
import paint.evolution.motion.MotionEvolutions

object EvolutionGeneratorPortfolio {

  object brownian {

    case class Settings(
      start: Point,
      radius: Double
    )

    val generator: EvolutionGenerator[Point, Settings] =
      EvolutionGenerator { settings: Settings =>
        solveIndependent(settings.start)(
          rectangle2D(settings.radius)
        ).positional
      }

    val drawing = Drawing(
      "brownian",
      generator,
      SettingsComponent[Settings],
      Settings(Point(900, 600), 2)
    )
  }

  object brownianWithRandomJumps {

    case class Settings(
      start: Point,
      radius: Double,
      jumpProbability: Double,
      jumpSize: Int
    )

    val generator: EvolutionGenerator[Point, Settings] =
      EvolutionGenerator { settings: Settings =>
        val slowDownEvo = double.map[Int] { d =>
          if (d < settings.jumpProbability) settings.jumpSize else 1
        }
        MotionEvolutions.solveIndependent(settings.start)(
          rectangle2D(settings.radius).slowDown(slowDownEvo)
        ).positional
      }

    val drawing = Drawing(
      "brownian with random jumps",
      generator,
      SettingsComponent[Settings],
      Settings(
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
