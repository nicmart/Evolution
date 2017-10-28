package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.react.component.config.instances._
import evolution.algebra.MotionEvolutionAlgebra.AccelerationLaw
import evolution.algebra.{Evolution, FullAlgebra}

import scala.collection.immutable.Queue

object dynamics extends DrawingDefinition("dynamics") {

  case class Config(
    acceleration: Double,
    friction: Double,
    initialSpeed: Point,
    numberOfPoints: Int
  )

  protected def currentConfig =
    Config(
      acceleration = 0.001,
      friction = 0.0008,
      initialSpeed = Point(0, 0),
      numberOfPoints = 1
    )

  class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
    import config._
    override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      val accelerationEvolution: Evo[AccelerationLaw[Point]] =
        rectangle2D(acceleration) map { randomAcc =>
          (position, velocity) =>
            randomAcc - velocity * friction
        }
      val singleEvo = solve2(context.canvasSize.point / 2, initialSpeed)(
        accelerationEvolution
      ).positional

      sequenceParallel(Queue.fill(config.numberOfPoints)(singleEvo)).take(3000).repeat(100)
    }
  }

  protected def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  protected def component: ConfigComponent[Config] = ConfigComponent[Config]
}
