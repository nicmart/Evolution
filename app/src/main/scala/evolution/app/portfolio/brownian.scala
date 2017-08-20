package evolution.app.portfolio

import evolution.app.portfolio.DrawingPortfolio.{DrawingDefinition, WithCanvasSize}
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.Evolution
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import evolution.app.react.component.config.instances._

object brownian extends DrawingDefinition("brownian") {

  case class Config(
    canvasSize: Point,
    radius: Double
  ) extends WithCanvasSize

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  override def evolution(config: Config): Evolution[Point] = {
    solveIndependent(config.canvasSize)(
      rectangle2D(config.radius)
    ).positional
  }

  val defaultConfig = Config(Point(900, 600), 2)
}
