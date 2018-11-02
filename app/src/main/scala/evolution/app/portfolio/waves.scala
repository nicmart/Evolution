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

object waves extends LegacyDrawingDefinition[Point] {
  val name = "waves"

  case class Config(springConstant: Double, friction: Double, speed: Double, acceleration: Double, numberOfWaves: Int)

  def initialConfig =
    Config(springConstant = 0.0004, friction = 0.0004, speed = 0.1, acceleration = 0.0025, numberOfWaves = 40)

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] = {
    import config._
    new LegacyEvolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._

        val accelerationEq: AccelerationLaw[Point] =
          (x, v) => Point(0, -springConstant * x.y) - v * friction
        // Note: AccelerationEvolution[Point] type alias causes problems with implicits: it thinks that the repr is AccEvo...
        val accelerationEvo: Evo[AccelerationLaw[Point]] = constant(accelerationEq)
        val accelerationEvo2 =
          accelerationEvo.zipWith[Point, AccelerationLaw[Point]](cartesian(constant(0.0), ball(acceleration))) {
            (eq: AccelerationLaw[Point], noise: Point) =>
              { (x: Point, v: Point) =>
                {
                  val acc = eq(x, v)
                  acc + noise
                }
              }
          }

        def wave(from: Point) = translate(
          uniformLinear(from, Point(config.speed, 0)),
          solve2[Point](Point.zero, Point.zero)(accelerationEvo2).positional
        )

        sequenceParallel(
          Queue(
            Point
              .sequence(config.numberOfWaves, Point(context.left, context.top), Point(context.left, context.bottom))
              .map(wave): _*
          )
        )
      }
    }
  }

  val configComponent: ConfigComponent[Config] = ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
