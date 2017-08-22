package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.Evolution
import paint.evolution.NumericEvolutions.double
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.motion.MotionEvolutions
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import evolution.app.react.component.config.instances._

object brownianWithRandomJumps extends DrawingDefinition("brownian with random jumps") {

  case class Config(
    radius: Double,
    jumpProbability: Double,
    jumpSize: Int
  )

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    val slowDownEvo = double.map[Int] { d =>
      if (d < config.jumpProbability) config.jumpSize else 1
    }
    MotionEvolutions.solveIndependent(context.canvasSize.point / 2)(
      rectangle2D(config.radius).slowDown(slowDownEvo)
    ).positional
  }

  val currentConfig =
    Config(
      1,
      0.0001,
      200
    )
}
