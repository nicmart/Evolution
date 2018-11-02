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

object brownian extends LegacyDrawingDefinition[Point] {
  val name = "brownian"
  case class Config(radius: Double, order: Int)

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] = {
    new LegacyEvolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        val order = if (config.order > 0) config.order else 0

        val start: Evo[Point] = rectangle2D(config.radius)

        (1 to order).foldLeft[Evo[Point]](start) { (evoSoFar, _) =>
          solveIndependent(Point.zero)(evoSoFar).positional
        }
      }
    }
  }

  val initialConfig = Config(2, 1)

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
