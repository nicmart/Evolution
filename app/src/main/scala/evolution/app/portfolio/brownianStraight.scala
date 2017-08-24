package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.{Evolution, NumericEvolutions}
import paint.evolution.NumericEvolutions.choose
import paint.evolution.PointEvolutions.rotate
import paint.evolution.motion.MotionEvolutions
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import paint.evolution.motion.MotionEvolutions._

object brownianStraight extends DrawingDefinition("brownian straight") {
  case class Config(
    maxLength: Int,
    minLength: Int,
    rotation: Double,
    n: Int
  )

  protected def currentConfig = Config(
    maxLength = 10,
    minLength = 5,
    rotation = 0,
    n = 4
  )

  protected def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    rotate(
      context.canvasSize.point / 2,
      2 * Math.PI * (config.rotation / 360),
      solveIndependent(context.canvasSize.point / 2) {
        choose(Point.regularPolygon(config.n))
          .slowDown(NumericEvolutions.intRange(config.minLength, config.maxLength))
      }.positional
    )
  }

  protected def component: ConfigComponent[Config] = ConfigComponent[Config]
}
