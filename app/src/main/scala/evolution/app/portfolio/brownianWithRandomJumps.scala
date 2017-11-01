package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.AbstractDrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.{Evolution, FullAlgebra}
import evolution.algebra.syntax.all._

object brownianWithRandomJumps extends AbstractDrawingDefinition("brownian with random jumps") {

  case class Config(
    radius: Double,
    jumpProbability: Double,
    jumpSize: Int
  )

  override def configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
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

  val initialConfig =
    Config(
      1,
      0.0001,
      200
    )
}
