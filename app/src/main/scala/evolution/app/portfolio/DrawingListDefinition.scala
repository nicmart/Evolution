package evolution.app.portfolio

import evolution.app.codec.JsonCodec
import evolution.app.data.PointedSeq
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{ CompositeDefinitionConfig, DrawingDefinition }
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.{ CompositeConfigComponent, ConfigComponent }

class DrawingListDefinition[T](drawingList: PointedSeq[DrawingDefinition[T]]) extends DrawingDefinition[T] {

  type Config = CompositeDefinitionConfig[T]

  override def name: String = "All drawings"

  override def initialConfig: Config =
    CompositeDefinitionConfig[T, drawingList.selected.Config](drawingList.selected.initialConfig, drawingList.selected)

  override def stream(ctx: DrawingContext, state: DrawingState[CompositeDefinitionConfig[T]]): Iterator[T] =
    state.config.definition.stream(ctx, DrawingState(state.seed, state.config.config))

  override val configComponent: ConfigComponent[Config] =
    CompositeConfigComponent[T](drawingList)

  override def configCodec: JsonCodec[Config] =
    CompositeDefinitionConfig.jsonCodec[T](drawingList)
}
