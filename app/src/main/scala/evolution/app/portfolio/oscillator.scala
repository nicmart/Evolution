package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{DrawingDefinition, LegacyDrawingDefinition}
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import evolution.algebra.syntax.all._
import evolution.algebra.{FullAlgebra, LegacyEvolution}
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._

import scala.collection.immutable.Queue
import io.circe.generic.auto._

object oscillator extends LegacyDrawingDefinition[Point] {
  val name = "oscillator"

  case class Config(
    springConstant: Double,
    friction: Double,
    randomNoiseProbability: Double,
    randomNoiseStrength: Double
  )

  def initialConfig =
    Config(springConstant = 0.00001, friction = 0.0001, randomNoiseProbability = .001, randomNoiseStrength = .1)

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] = {
    import config._
    new LegacyEvolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._

        val accelerationEq: AccelerationLaw[Point] =
          (x, v) => Point(0, -springConstant * x.y) - v * friction
        // Note: AccelerationEvolution[Point] type alias causes problems with implicits: it thinks that the repr is AccEvo...
        val accelerationEvo: Evo[AccelerationLaw[Point]] = constant(accelerationEq)
        val random = doubleBetween(0, 1)
          .zipWith(ball(randomNoiseStrength)) { (p, y) =>
            if (p <= randomNoiseProbability) Point(0, y)
            else Point.zero
          }
        val accelerationEvo2 =
          accelerationEvo.zipWith[Point, AccelerationLaw[Point]](random) { (eq: AccelerationLaw[Point], noise: Point) =>
            { (x: Point, v: Point) =>
              {
                val acc = eq(x, v)
                acc + noise
              }
            }
          }

        solve2[Point](Point.zero, Point.zero)(accelerationEvo2).positional
      }
    }
  }

  val configComponent: ConfigComponent[Config] = ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
