package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{DrawingDefinition, LegacyDrawingDefinition}
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import evolution.algebra._
import evolution.geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import io.circe.generic.auto._

object line extends LegacyDrawingDefinition[Point] {
  val name = "line"

  case class Config(speed: Double)

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] = {
    new LegacyEvolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        solveIndependentStatic(Point.zero)(Point(config.speed, 0)).positional
      }
    }
  }

  val initialConfig = Config(.01)

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
