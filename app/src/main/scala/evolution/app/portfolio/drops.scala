package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.geometry.Geometry.Point
import evolution.app.react.component.config.instances._
import paint.evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import paint.evolution.algebra.{Evolution, FullAlgebra}
import paint.evolution.algebra.syntax.all._
import scala.collection.immutable.Queue

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

  private class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
      import config._
      import alg._

      val acc = Point(0, acceleration)
      val randomForces = double.flatMap { p =>
        if (p < randomForceProbability) ring(randomForceStrength).head else pure(Point.zero)
      }

      def accelerationEvolution: Evo[AccelerationLaw[Point]] = randomForces map { randomAcc =>
        (position, velocity) =>
          if ((randomAcc + velocity + acc).norm() < threshold) -velocity
          else randomAcc + acc - velocity * friction
      }

      def pointEvo(from: Point): Evo[Point] = {
        solve2(from, Point.zero)(accelerationEvolution).positional
      }

      sequenceParallel(
        Queue.apply(Point.sequence(
          numberOfDrops,
          context.canvasSize.point.copy(y = 0),
          Point(0, 0)
        ).map(pointEvo): _*)
      )
    }
  }

  protected def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  override def component: ConfigComponent[Config] = ConfigComponent[Config]
}
