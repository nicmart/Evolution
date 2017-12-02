package evolution.app.model.definition

import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra.Evolution
import evolution.app.codec.JsonCodec

trait DrawingDefinition[T] {
  type Config
  def name: String
  def initialConfig: Config
  def evolution(config: Config, context: DrawingContext): Evolution[T]
  def configComponent: ConfigComponent[Config]
  def configCodec: JsonCodec[Config]
}

object DrawingDefinition {
  type Aux[T, C] = DrawingDefinition[T] { type Config = C }
}

final case class DrawingListWithSelection[T](
  list: List[DrawingDefinition[T]],
  current: DrawingDefinition[T]
) {
  type CurrentConfig = current.Config
  def byName(name: String): DrawingDefinition[T] =
    list.find(_.name == name).getOrElse(current)
}

object DrawingListWithSelection {
  type Aux[T, C] = DrawingDefinition[T] { type CurrentConfig = C }
}