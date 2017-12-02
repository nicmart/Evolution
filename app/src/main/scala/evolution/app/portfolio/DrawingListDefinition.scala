package evolution.app.portfolio

import evolution.algebra.Evolution
import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{CompositeDefinitionConfig, DrawingDefinition, DrawingListWithSelection}
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances.CompositeComponent

class DrawingListDefinition[T](drawingList: DrawingListWithSelection[T]) extends DrawingDefinition[T] {

  type Config = CompositeDefinitionConfig[T]

  override def name: String = "All drawings"

  override def initialConfig: Config =
    CompositeDefinitionConfig[T, drawingList.current.Config](
      drawingList.current.initialConfig,
      drawingList.current
    )

  override def evolution(config: Config, context: DrawingContext): Evolution[T] =
    config.definition.evolution(config.config, context)

  override val configComponent: ConfigComponent[Config] =
    CompositeComponent[T](drawingList)

  override def configCodec: JsonCodec[Config] =
    CompositeDefinitionConfig.jsonCodec[T](drawingList)
}