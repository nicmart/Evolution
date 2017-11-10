package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import evolution.algebra
import evolution.algebra.Evolution
import evolution.geometry.Point
import evolution.algebra.syntax.all._

object singlePoint extends DrawingDefinition[Point] {
  val name = "single constant point"
  type Config = Unit
  def initialConfig: Unit = ()

  class ThisEvolution(config: Unit, context: DrawingContext) extends Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      //constant(Point.zero)
      int.map(n => Point(n, n))
    }
  }

  def evolution(config: Unit, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  def configComponent: ConfigComponent[Unit] = ConfigComponent[Unit]
}
