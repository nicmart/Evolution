package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.EvolutionLegacy
import paint.evolution.algebra._
import paint.geometry.Geometry.Point
import paint.evolution.algebra.syntax.all._

object brownian extends DrawingDefinition("brownian") {

  case class Config(
    radius: Double
  )

  override def component: ConfigComponent[Config] =
    ConfigComponent[Config]

  // Example with new
  def evolution(config: Config, context: DrawingContext): EvolutionLegacy[Point] = {
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        solveIndependent(context.canvasSize.point / 2)(
          rectangle2D(config.radius)
        ).positional
      }
    }.run
  }

  val currentConfig = Config(2)
}
