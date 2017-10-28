package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.instances._
import cats.implicits._
import evolution.app.portfolio.primes.Config
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra.Evolution
import evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import evolution.algebra
import evolution.geometry.Geometry
import evolution.algebra.syntax.all._
import evolution.geometry.Geometry.Point

object bouncing extends DrawingDefinition("bouncing") {
  case class Config(
    groundLevel: Int,
    gravity: Double,
    elasticity: Double,
    friction: Double,
    horizontalSpeed: Double
  )

  protected def currentConfig =
    Config(
      groundLevel = 100,
      gravity = 0.000001,
      elasticity = 0.000001,
      friction = 0.0001,
      horizontalSpeed = 0.003
    )

  class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      val canvasSize = context.canvasSize.point
      val ground = canvasSize.y - config.groundLevel

      val gravityField: Evo[AccelerationLaw[Double]] = constant {
        (_, _) => config.gravity
      }

      val elasticGround: Evo[AccelerationLaw[Double]] = constant {
        (y, vel) =>
          if (y > ground) {
            (ground - y) * config.elasticity + config.gravity - config.friction * vel
          }
          else 0
      }

      val law = gravityField + elasticGround

      val xEv = solveIndependentStatic(0.0)(config.horizontalSpeed).positional
      val yEv = solve2(0.0, 0.0) {
        law
      }.positional

      cartesian(xEv, yEv)
    }
  }

  protected def generateEvolution(config: Config, context: DrawingContext): Evolution[Geometry.Point] =
    new ThisEvolution(config, context)

  protected def component = ConfigComponent[Config]
}
