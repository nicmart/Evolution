package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.syntax.all._
import evolution.algebra.LegacyEvolution
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import io.circe.generic.auto._

object circlesOnCircles extends DrawingDefinition[Point] {
  val name = "circles on circles"

  case class Config(
    bigRadius: Double,
    bigRadialSpeed: Double,
    mediumRadius: Double,
    mediumRadialSpeed: Double,
    smallRadius: Double,
    smallRadialSpeed: Double,
    lastRadius: Double,
    lastRadialSpeed: Double
  )

  val initialConfig = Config(
    bigRadius = 500,
    bigRadialSpeed = 0.002,
    mediumRadius = 104,
    mediumRadialSpeed = 1,
    smallRadius = 39,
    smallRadialSpeed = 1.01,
    lastRadius = 10,
    lastRadialSpeed = 1.02
  )

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  class ThisEvolution(config: Config, context: DrawingContext) extends LegacyEvolution[Point] {
    import config._
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      translate(
        uniformRadial(Point(0, bigRadius), bigRadialSpeed),
        translate(
          uniformRadial(Point(0, mediumRadius), mediumRadialSpeed),
          translate(
            uniformRadial(Point(0, smallRadius), smallRadialSpeed),
            uniformRadial(Point(0, lastRadius), lastRadialSpeed)
          )
        )
      )
    }
  }

  override def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] =
    new ThisEvolution(config, context)

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
