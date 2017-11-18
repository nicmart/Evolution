package evolution.app.model.configured

import evolution.algebra.materializer.Materializer
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition

trait DefinitionToComponent[S, T] {
  // @todo change definition to use a configured drawing
  def toComponentWithInitialConfig(
    definition: DrawingDefinition[T],
    context: DrawingContext
  ): DrawingComponent[S, T] =
    toComponent(definition, context)(definition.initialConfig)

  def toComponent(
    definition: DrawingDefinition[T],
    context: DrawingContext)(
    config: definition.Config
  ): DrawingComponent[S, T]
}

class MaterializerDefinitionToComponent[S, T](
  materializer: Materializer[S]
) extends DefinitionToComponent[S, T] {

  override def toComponent(
    definition: DrawingDefinition[T],
    context: DrawingContext)(
    config: definition.Config
  ): DrawingComponent[S, T] =
    new EvolutionDrawingComponent[S, T, definition.Config](
      materializer,
      ConfiguredDrawing[T, definition.Config](
        definition,
        context,
        config
      )
    )
}
