package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.{Evolution, PointEvolutions}
import paint.evolution.NumericEvolutions.ball
import paint.evolution.PointEvolutions._
import paint.evolution.SemigroupEvolutions.translate
import paint.evolution.implicits._
import paint.evolution.motion.MotionEvolutions.solveIndependent
import paint.geometry.Geometry.Point
import evolution.app.react.component.config.instances._

object curlyRing extends DrawingDefinition("curly ring") {
  case class Config(
    bigRadius: Double,
    bigRadialSpeed: Double,
    mediumRadius: Double,
    mediumRadialSpeed: Double,
    smallRadius: Double,
    smallRadialSpeed: Double,
    noiseStrength: Double,
    noiseFrames: Int
  )


  val currentConfig = Config(
    bigRadius = 300,
    bigRadialSpeed = 0.0001,
    mediumRadius = 30,
    mediumRadialSpeed = 0.003,
    smallRadius = 5,
    smallRadialSpeed = 0.01,
    noiseFrames = 50,
    noiseStrength = 5
  )

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    centeredIn(context.canvasSize.point / 2) {
      translate(
        translate(
          uniformRadial(Point(0, config.bigRadius), config.bigRadialSpeed),
          translate(
            uniformRadial(Point(0, config.mediumRadius), config.mediumRadialSpeed),
            uniformRadial(Point(0, config.smallRadius), config.smallRadialSpeed)
          )
        ),
        ball2D(config.noiseStrength).slowDown(config.noiseFrames)
      )
    }
  }
}
