package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import evolution.algebra.syntax.all._
import evolution.algebra.{Evolution, FullAlgebra}
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._

import scala.collection.immutable.Queue
import io.circe.generic.auto._

object oscillator extends DrawingDefinition[Point] {
  val name = "oscillator"

  case class Config(
    springConstant: Double,
    friction: Double,
    speed: Double,
    randomNoise: Double
  )

  def initialConfig = Config(
    springConstant = 0.0004,
    friction = 0.0004,
    speed = 0.1,
    randomNoise = 0.0025
  )

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    import config._
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._

        val accelerationEq: AccelerationLaw[Point] =
          (x, v) => Point(0, -springConstant * x.y) - v * friction
        // Note: AccelerationEvolution[Point] type alias causes problems with implicits: it thinks that the repr is AccEvo...
        val accelerationEvo: Evo[AccelerationLaw[Point]] = constant(accelerationEq)
        val accelerationEvo2 =
          accelerationEvo.zipWith[Point, AccelerationLaw[Point]](cartesian(constant(0.0), ball(randomNoise))) {
            (eq: AccelerationLaw[Point], noise: Point) => {
              (x: Point, v: Point) => {
                val acc = eq(x, v)
                acc + noise
              }
            }
          }

        centeredIn(context.canvasSize.point / 2) {
          solve2[Point](Point.zero, Point.zero)(accelerationEvo2).positional
        }
      }
    }
  }

  val configComponent: ConfigComponent[Config] = ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
