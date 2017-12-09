package evolution.app.portfolio

import evolution.algebra
import evolution.algebra.Evolution
import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{CompositeDefinitionConfig, DrawingDefinition}
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.app.codec.JsonCodec._
import evolution.app.data.PointedSeq
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._

class CompositeDrawingDefinition(
  drawings: PointedSeq[DrawingDefinition[Point]]
) extends DrawingDefinition[Point] {

    val name = "combined drawings"

    case class Config(
      drawing1: CompositeDefinitionConfig[Point],
      drawing2: CompositeDefinitionConfig[Point]
    )

    override def initialConfig =
      Config(
        CompositeDefinitionConfig(drawings.selected.initialConfig, drawings.selected),
        CompositeDefinitionConfig(drawings.selected.initialConfig, drawings.selected)
      )

    class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
        import alg._
        centeredIn(- context.canvasSize.point / 2) {
          translate(
            config.drawing1.definition.evolution(config.drawing1.config, context).run,
            config.drawing2.definition.evolution(config.drawing2.config, context).run
          )
        }

        drawOnEvolution(
          toPhaseSpace(config.drawing1.definition.evolution(config.drawing1.config, context).run),
          centeredIn(- context.canvasSize.point / 2) {
            config.drawing2.definition.evolution(config.drawing2.config, context).run
          }
        )
      }
    }

    override def evolution(config: Config, context: DrawingContext): Evolution[Point] =
      new ThisEvolution(config, context)

    override lazy val configComponent: ConfigComponent[Config] = {
      implicit val drawingListComponent: ConfigComponent[CompositeDefinitionConfig[Point]] =
        CompositeConfigComponent(drawings)
      ConfigComponent[Config]
    }

    override def configCodec: JsonCodec[Config] = {
      val drawingListCodec: JsonCodec[CompositeDefinitionConfig[Point]] =
        CompositeDefinitionConfig.jsonCodec[Point](drawings)
      implicit val encoder: Encoder[CompositeDefinitionConfig[Point]] = JsonCodec.toCirceEncoder(drawingListCodec)
      implicit val decoder: Decoder[CompositeDefinitionConfig[Point]] = JsonCodec.toCirceDecoder(drawingListCodec)
      JsonCodec[Config]
    }
}
