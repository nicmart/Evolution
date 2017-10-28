package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.{Evolution, FullAlgebra}
import evolution.algebra.syntax.all._

object brownianWithRandomJumps extends DrawingDefinition("brownian with random jumps") {

  case class Config(
    radius: Double,
    jumpProbability: Double,
    jumpSize: Int
  )

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  def generateEvolution(config: Config, context: DrawingContext): Evolution[Point] = {
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        val slowDownEvo = double.map[Int] { d =>
          if (d < config.jumpProbability) config.jumpSize else 1
        }
        solveIndependent(context.canvasSize.point / 2)(
          rectangle2D(config.radius).slowDownBy(slowDownEvo)
        ).positional
      }
    }
  }

  val currentConfig =
    Config(
      1,
      0.0001,
      200
    )
}
