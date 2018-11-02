package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{DrawingDefinition, LegacyDrawingDefinition}
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import evolution.algebra
import evolution.algebra.LegacyEvolution
import evolution.geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import io.circe.generic.auto._

object singlePoint extends LegacyDrawingDefinition[Point] {
  val name = "single constant point"
  type Config = Unit
  def initialConfig: Unit = ()

  class ThisEvolution(config: Unit, context: DrawingContext) extends LegacyEvolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      constant(Point.zero)
    }
  }

  def evolution(config: Unit, context: DrawingContext): LegacyEvolution[Point] =
    new ThisEvolution(config, context)

  val configComponent: ConfigComponent[Unit] = ConfigComponent[Unit]

  override def configCodec: JsonCodec[Unit] =
    JsonCodec[Config]
}
