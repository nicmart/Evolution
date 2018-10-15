package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.{LegacyEvolution, FullAlgebra}
import evolution.algebra.syntax.all._
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import io.circe.generic.auto._

object brownianWithRandomJumps extends DrawingDefinition[Point] {
  val name = "brownian with random jumps"

  case class Config(radius: Double, jumpProbability: Double, jumpSize: Int)

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] = {
    new LegacyEvolution[Point] {
      override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        val slowDownEvo = double.map[Int] { d =>
          if (d < config.jumpProbability) config.jumpSize else 1
        }
        solveIndependent(Point.zero)(rectangle2D(config.radius).slowDownBy(slowDownEvo)).positional
      }
    }
  }

  val initialConfig =
    Config(1, 0.0001, 200)

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
