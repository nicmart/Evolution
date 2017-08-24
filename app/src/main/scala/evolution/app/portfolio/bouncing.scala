package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import paint.evolution.implicits._
import paint.evolution.Evolution.constant
import paint.evolution.PointEvolutions.cartesian
import paint.evolution.motion.AccelerationEvolution
import paint.evolution.motion.MotionEvolutions._
import evolution.app.react.component.config.instances._
import cats.implicits._
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.Evolution
import paint.geometry.Geometry

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

  protected def evolution(config: Config, context: DrawingContext): Evolution[Geometry.Point] = {
    val canvasSize = context.canvasSize.point
    val ground = canvasSize.y - config.groundLevel

    val gravityField: AccelerationEvolution[Double] = constant {
      (_, _) => config.gravity
    }

    val elasticGround: AccelerationEvolution[Double] = constant {
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

  protected def component = ConfigComponent[Config]
}
