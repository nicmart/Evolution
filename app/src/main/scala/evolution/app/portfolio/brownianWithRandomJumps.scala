package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.portfolio.brownian.Config
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.EvolutionLegacy
import paint.evolution.motion.MotionEvolutions
import paint.geometry.Geometry.Point
import evolution.app.react.component.config.instances._
import paint.evolution.algebra.{Evolution, FullAlgebra}
import paint.evolution.algebra.syntax.all._

object brownianWithRandomJumps extends DrawingDefinition("brownian with random jumps") {

  case class Config(
    radius: Double,
    jumpProbability: Double,
    jumpSize: Int
  )

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): EvolutionLegacy[Point] = {
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        val slowDownEvo = double.map[Int] { d =>
          if (d < config.jumpProbability) config.jumpSize else 1
        }
        solveIndependent(context.canvasSize.point / 2)(
          rectangle2D(config.radius).slowDownBy(slowDownEvo)
        ).positional
      }
    }.run
  }

  val currentConfig =
    Config(
      1,
      0.0001,
      200
    )
}
