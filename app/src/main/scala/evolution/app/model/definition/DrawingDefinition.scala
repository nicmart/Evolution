package evolution.app.model.definition

import evolution.app.model.configured.{ConfiguredDrawing, ConfiguredEvolution}
import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import evolution.algebra.Evolution
import evolution.geometry.Geometry.Point

abstract class DrawingDefinition(val name: String) {
  protected type Config
  protected def currentConfig: Config
  protected def generateEvolution(config: Config, context: DrawingContext): Evolution[Point]
  protected def component: ConfigComponent[Config]
  def drawing(context: DrawingContext): ConfiguredDrawing[Point] =
    ConfiguredDrawing(
      name,
      ConfiguredEvolution(generateEvolution, context, currentConfig),
      ConfiguredComponent(component, currentConfig)
    )
}

final case class DrawingListWithSelection(
  list: List[DrawingDefinition],
  current: DrawingDefinition
)