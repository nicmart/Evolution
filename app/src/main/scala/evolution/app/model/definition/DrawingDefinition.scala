package evolution.app.model.definition

import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra.Evolution
import evolution.geometry.Point

trait DrawingDefinition[T] {
  type Config
  def name: String
  def initialConfig: Config
  def evolution(config: Config, context: DrawingContext): Evolution[T]
  def configComponent: ConfigComponent[Config]
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