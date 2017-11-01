package evolution.app.model.definition

import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import evolution.algebra.Evolution
import evolution.app.model.configured.ConfiguredDrawing
import evolution.app.model.definition.DrawingDefinition.Aux
import evolution.geometry.Point

trait DrawingDefinition[T] {
  type Config
  def name: String
  def initialConfig: Config
  def evolution(config: Config, context: DrawingContext): Evolution[T]
  def configComponent: ConfigComponent[Config]

  def configure(config: Config, context: DrawingContext): ConfiguredDrawing[T, Config] =
    // @todo what???
    ConfiguredDrawing[T, Config](this.asInstanceOf[Aux[T, Config]], context, config)
}

object DrawingDefinition {
  type Aux[T, C] = DrawingDefinition[T] { type Config = C }
}

// @todo remove
abstract class AbstractDrawingDefinition(val name: String) extends DrawingDefinition[Point]

final case class DrawingListWithSelection[T](
  list: List[DrawingDefinition[T]],
  current: DrawingDefinition[T]
)