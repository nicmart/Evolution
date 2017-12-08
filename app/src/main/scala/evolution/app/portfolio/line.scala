package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import evolution.algebra._
import evolution.geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import io.circe.generic.auto._

object line extends DrawingDefinition[Point] {
  val name = "line"

  case class Config(
    speed: Double
  )

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        centeredIn(context.canvasSize.point / 2) {
          solveIndependentStatic(Point.zero)(Point(config.speed, 0)).positional
        }
      }
    }
  }

  val initialConfig = Config(.01)

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}