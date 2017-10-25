package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.{EvolutionLegacy, NumericEvolutions, algebra}
import paint.geometry.Geometry.Point
import paint.evolution.algebra.syntax.all._
import evolution.app.react.component.config.instances._
import paint.evolution.algebra.{Evolution, FullAlgebra}


object brownianStraight extends DrawingDefinition("brownian straight") {

  case class Config(
    maxLength: Int,
    minLength: Int,
    rotation: Double,
    n: Int
  )

  protected def currentConfig = Config(
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

  protected def evolution(config: Config, context: DrawingContext): EvolutionLegacy[Point] =
    new ThisEvolution(config, context).run

  protected def component: ConfigComponent[Config] = ConfigComponent[Config]
}
