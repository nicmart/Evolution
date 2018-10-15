package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.react.component.config.instances._
import evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import evolution.algebra.{LegacyEvolution, FullAlgebra}
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._

import scala.collection.immutable.Queue
import io.circe.generic.auto._

object dynamics extends DrawingDefinition[Point] {
  val name = "dynamics"

  case class Config(acceleration: Double, friction: Double, numberOfPoints: Int)

  def initialConfig =
    Config(acceleration = 0.001, friction = 0.0008, numberOfPoints = 1)

  class ThisEvolution(config: Config, context: DrawingContext) extends LegacyEvolution[Point] {
    import config._
    override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      val accelerationEvolution: Evo[AccelerationLaw[Point]] =
        rectangle2D(acceleration) map { randomAcc => (_, velocity) =>
          randomAcc - velocity * friction
        }
      val singleEvo = solve2(Point.zero, Point.zero)(accelerationEvolution).positional

      sequenceParallel(Queue.fill(config.numberOfPoints)(singleEvo))
    }
  }

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] =
    new ThisEvolution(config, context)

  val configComponent: ConfigComponent[Config] = ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
