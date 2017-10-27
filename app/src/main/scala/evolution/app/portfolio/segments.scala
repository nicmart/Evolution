package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.geometry.Geometry.Point
import evolution.app.react.component.config.instances._
import paint.evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import paint.evolution.algebra.{Evolution, FullAlgebra}
import paint.evolution.algebra.syntax.all._

object segments extends DrawingDefinition("segments") {

  case class Config(
    startingSpeed: Double,
    acceleration: Double,
    friction: Double,
    lengthOverSpeed: Int
  )

  protected def currentConfig = Config(
    startingSpeed = 3,
    acceleration = 0.1,
    friction = 0.00001,
    lengthOverSpeed = 100
  )

  protected def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
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

        solve2((context.canvasSize.point / 2).copy(x = 0), Point(startingSpeed, 0))(
          accelerationEvolution
        ).flatMap { case (position, velocity) =>
          val rotatedVel = velocity.rotate(Math.PI / 2)
          segment(position - rotatedVel * lengthOverSpeed, position + rotatedVel * lengthOverSpeed, 1)
        }
      }
    }
  }

  protected def component = ConfigComponent[Config]
}
