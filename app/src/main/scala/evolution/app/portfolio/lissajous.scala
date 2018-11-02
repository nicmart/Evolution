package evolution.app.portfolio

import evolution.algebra._
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{DrawingDefinition, LegacyDrawingDefinition}
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import evolution.geometry.Point
import io.circe.generic.auto._
import cats.instances.double._

object lissajous extends LegacyDrawingDefinition[Point] {
  val name = "lissajous"

  case class Config(a: Double, b: Double, delta: Double, speed: Double)

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] = {
    new LegacyEvolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._, config._

        val t = progression(speed, 0)
        val radiants = (delta / 360) * 2 * Math.PI
        val size = Math.min(context.canvasSize.height, context.canvasSize.width) * 0.4

        progression(speed, 0).map { t =>
          Point(Math.sin(a * t + radiants), Math.sin(b * t)) * size
        }
      }
    }
  }

  val initialConfig =
    Config(a = 5, b = 4, delta = 90, speed = .01)

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
