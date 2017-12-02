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

object brownian extends DrawingDefinition[Point] {
  val name = "brownian"
  case class Config(
    radius: Double
  )

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): Evolution[Point] = {
    new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        solveIndependent(context.canvasSize.point / 2)(
          rectangle2D(config.radius)
        ).positional
      }
    }
  }

  val initialConfig = Config(2)

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
