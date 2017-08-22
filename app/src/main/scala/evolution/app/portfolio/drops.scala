package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.Evolution
import paint.evolution.Evolution.{pure, sequenceParallel}
import paint.evolution.NumericEvolutions.double
import paint.evolution.PointEvolutions.ring
import paint.evolution.motion.{AccelerationLaw, MotionEvolutions}
import paint.geometry.Geometry.Point
import paint.evolution.implicits._
import evolution.app.react.component.config.instances._

object drops extends DrawingDefinition("drops") {

  case class Config(
    friction: Double,
    acceleration: Double,
    threshold: Double,
    randomForceProbability: Double,
    randomForceStrength: Double,
    numberOfDrops: Int
  )

  override def currentConfig: Config =
    Config(
      friction = 0.02,
      acceleration = 0.003,
      threshold = 0.1,
      randomForceProbability = 0.01,
      randomForceStrength = 0.1,
      numberOfDrops = 80
    )

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    val acc = Point(0, config.acceleration)
    val randomForces = double.flatMap { p =>
      if (p < config.randomForceProbability) ring(config.randomForceStrength).first else pure(Point.zero)
    }

    def accelerationEvolution: Evolution[AccelerationLaw[Point]] = randomForces map { randomAcc =>
      (position, velocity) =>
        if ((randomAcc + velocity + acc).norm() < config.threshold) -velocity
        else randomAcc + acc - velocity * config.friction
    }

    def pointEvo(from: Point): Evolution[Point] = {
      MotionEvolutions.solve2(from, Point.zero)(accelerationEvolution).positional
    }

    val evo = sequenceParallel(
      Point.sequence(
        config.numberOfDrops,
        context.canvasSize.point.copy(y = 0),
        Point(0, 0)).map(pointEvo
      )
    ).flattenList

    evo
  }

  override def component: ConfigComponent[Config] = ConfigComponent[Config]
}
