package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.react.component.config.instances._
import evolution.algebra.{Evolution, FullAlgebra}


object brownianStraight extends DrawingDefinition[Point] {
  val name = "brownian straight"

  case class Config(
    maxLength: Int,
    minLength: Int,
    rotation: Double,
    n: Int
  )

  def initialConfig = Config(
    maxLength = 10,
    minLength = 5,
    rotation = 0,
    n = 4
  )

  class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
    import config._
    override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
      import alg._
        solveIndependent(context.canvasSize.point / 2) {
          choose(Point.regularPolygon(config.n))
            .slowDownBy(intBetween(minLength, maxLength))
        }.positional.rotate(
          context.canvasSize.point / 2,
          2 * Math.PI * (rotation / 360)
        )
    }
  }

  def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  def configComponent: ConfigComponent[Config] = ConfigComponent[Config]
}
