package evolution.app.portfolio

import evolution.app.model.DrawingContext
import evolution.app.portfolio.DrawingPortfolio.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.Evolution
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import evolution.app.react.component.config.instances._

object brownian extends DrawingDefinition("brownian") {

  case class Config(
    radius: Double
  )

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    solveIndependent(context.canvasSize.point / 2)(
      rectangle2D(config.radius)
    ).positional
  }

  val currentConfig = Config(2)
}
