package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.instances._
import cats.implicits._
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra.LegacyEvolution
import evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import evolution.algebra
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import evolution.app.portfolio.brownian.Config
import evolution.geometry.Point
import io.circe.generic.auto._

object bouncing extends DrawingDefinition[Point] {
  override val name = "bouncing"
  case class Config(groundLevel: Int, gravity: Double, elasticity: Double, friction: Double, horizontalSpeed: Double)

  def initialConfig =
    Config(groundLevel = 100, gravity = -0.000001, elasticity = 0.000001, friction = 0.0001, horizontalSpeed = 0.003)

  class ThisEvolution(config: Config, context: DrawingContext) extends LegacyEvolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      val canvasSize = context.canvasSize.point
      val ground = config.groundLevel + context.bottom

      val gravityField: Evo[AccelerationLaw[Double]] = constant { (_, _) =>
        config.gravity
      }

      val elasticGround: Evo[AccelerationLaw[Double]] = constant { (y, vel) =>
        if (y < ground) {
          (ground - y) * config.elasticity + config.gravity - config.friction * vel
        } else 0
      }

      val law = gravityField + elasticGround

      val xEv = solveIndependentStatic(context.left)(config.horizontalSpeed).positional
      val yEv = solve2(context.top, 0.0) {
        law
      }.positional

      cartesian(xEv, yEv)
    }
  }

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] =
    new ThisEvolution(config, context)

  val configComponent: ConfigComponent[Config] = ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
