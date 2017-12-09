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
import io.circe.generic.auto._

object segments extends DrawingDefinition[Point] {
  val name = "segments"

  case class Config(
    startingSpeed: Double,
    acceleration: Double,
    friction: Double,
    lengthOverSpeed: Int
  )

  def initialConfig = Config(
    startingSpeed = 3,
    acceleration = 0.1,
    friction = 0.00001,
    lengthOverSpeed = 100
  )

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    import config._
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._

        def accelerationEvolution: Evo[AccelerationLaw[Point]] = {
          rectangle2D(acceleration) map { randomAcc =>
            (position, velocity) =>
              randomAcc - velocity * friction
          }
        }

        solve2(Point(context.left, 0), Point(startingSpeed, 0))(
          accelerationEvolution
        ).flatMap { case (position, velocity) =>
          val rotatedVel = velocity.rotate(Math.PI / 2)
          segment(position - rotatedVel * lengthOverSpeed, position + rotatedVel * lengthOverSpeed, 1)
        }
      }
    }
  }

  val configComponent: ConfigComponent[Config] = ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
