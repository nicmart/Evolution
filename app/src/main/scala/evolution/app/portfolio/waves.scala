package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.Evolution
import paint.evolution.Evolution.{constant, sequenceParallel}
import paint.evolution.NumericEvolutions.ball
import paint.evolution.PointEvolutions.{cartesian, uniformLinear}
import paint.evolution.SemigroupEvolutions.translate
import paint.evolution.implicits._
import paint.evolution.motion.{AccelerationEvolution, MotionEvolutions}
import paint.geometry.Geometry.Point
import evolution.app.react.component.config.instances._

object waves extends DrawingDefinition("waves") {

  case class Config(
    springConstant: Double,
    friction: Double,
    speed: Double,
    acceleration: Double,
    numberOfWaves: Int
  )

  def currentConfig = Config(
    springConstant = 0.0004,
    friction = 0.0004,
    speed = 0.1,
    acceleration = 0.0025,
    numberOfWaves = 40
  )

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    val accelerationEq: (Point, Point) => Point =
      (x, v) => Point(0, -config.springConstant * x.y) - v * config.friction
    val accelerationEvo: AccelerationEvolution[Point] = constant(accelerationEq)
    val accelerationEvo2: AccelerationEvolution[Point] =
      accelerationEvo.zipWith(cartesian(constant(0), ball(config.acceleration))) {
        (eq, noise: Point) => {
          (x: Point, v: Point) => {
            val acc = eq(x, v)
            acc + noise
          }
        }
      }

    def vibration(from: Point) = translate(
      uniformLinear(from, Point(config.speed, 0)),
      MotionEvolutions.solve2[Point](Point.zero, Point.zero)(accelerationEvo2).positional
    )

    sequenceParallel(
      Point.sequence(config.numberOfWaves, Point.zero, context.canvasSize.point.copy(x = 0)).map(vibration)
    ).flattenList
  }

  def component: ConfigComponent[Config] = ConfigComponent[Config]
}
