package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import evolution.algebra.{Evolution, FullAlgebra}
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._

import scala.collection.immutable.Queue
import io.circe.generic.auto._

object drops extends DrawingDefinition[Point] {
  val name = "drops"

  case class Config(
    friction: Double,
    acceleration: Double,
    threshold: Double,
    randomForceProbability: Double,
    randomForceStrength: Double,
    numberOfDrops: Int
  )

  override def initialConfig: Config =
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

  def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  override val configComponent: ConfigComponent[Config] = ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
