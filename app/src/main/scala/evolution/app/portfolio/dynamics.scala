package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.Evolution
import paint.evolution.PointEvolutions.rectangle2D
import paint.evolution.implicits._
import paint.evolution.motion.{AccelerationLaw, MotionEvolutions}
import paint.geometry.Geometry.Point

object dynamics extends DrawingDefinition("dynamics") {

  case class Config(
    acceleration: Double,
    friction: Double,
    initialSpeed: Point,
    numberOfPoints: Int
  )

  protected def currentConfig =
    Config(
      acceleration = 0.001,
      friction = 0.0008,
      initialSpeed = Point(0, 0),
      numberOfPoints = 1
    )

  protected def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    def accelerationEvolution: Evolution[AccelerationLaw[Point]] =
      rectangle2D(config.acceleration) map { randomAcc =>
        (position, velocity) =>
          randomAcc - velocity * config.friction
      }

    val singleEvo = MotionEvolutions.solve2(context.canvasSize.point / 2, config.initialSpeed)(
      accelerationEvolution
    ).positional

    Evolution.sequenceParallel(List.fill(config.numberOfPoints)(singleEvo)).flattenList
  }

  protected def component: ConfigComponent[Config] = ConfigComponent[Config]
}
