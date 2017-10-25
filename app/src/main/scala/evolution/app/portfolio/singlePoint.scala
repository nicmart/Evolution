package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.algebra
import paint.evolution.algebra.Evolution
import paint.geometry.Geometry.Point

object singlePoint extends DrawingDefinition("single constant point") {
  type Config = Unit
  protected def currentConfig: Unit = ()

  class ThisEvolution(config: Unit, context: DrawingContext) extends Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
      alg.constant(Point.zero)
  }

  protected def evolution(config: Unit, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  protected def component: ConfigComponent[Unit] = ConfigComponent[Unit]
}
