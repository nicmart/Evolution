package evolution.app.model.definition

import evolution.app.model.configured.{ConfiguredDrawing, ConfiguredEvolution}
import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import paint.evolution.EvolutionLegacy
import paint.evolution.algebra.impl.RNGEvolutionAlgebra
import paint.geometry.Geometry.Point

abstract class DrawingDefinition(val name: String) {
  implicit val interpreter: RNGEvolutionAlgebra = new RNGEvolutionAlgebra
  protected type Config
  protected def currentConfig: Config
  protected def evolution(config: Config, context: DrawingContext): EvolutionLegacy[Point]
  protected def component: ConfigComponent[Config]
  def drawing(context: DrawingContext): ConfiguredDrawing[Point] =
    ConfiguredDrawing(
      name,
      ConfiguredEvolution(evolution, context, currentConfig),
      ConfiguredComponent(component, currentConfig)
    )
}

final case class DrawingListWithSelection(
  list: List[DrawingDefinition],
  current: DrawingDefinition
)