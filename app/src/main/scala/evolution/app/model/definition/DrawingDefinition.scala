package evolution.app.model.definition

import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra.LegacyEvolution
import evolution.app.codec.JsonCodec

trait DrawingDefinition[T] {
  type Config
  def name: String
  def initialConfig: Config
  def evolution(config: Config, context: DrawingContext): LegacyEvolution[T]
  def configComponent: ConfigComponent[Config]
  def configCodec: JsonCodec[Config]
}

object DrawingDefinition {
  type Aux[T, C] = DrawingDefinition[T] { type Config = C }
}
