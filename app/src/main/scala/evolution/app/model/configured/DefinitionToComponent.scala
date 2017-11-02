package evolution.app.model.configured

import evolution.algebra.materializer.Materializer
import evolution.app.conf.Conf.materializer
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition

trait DefinitionToComponent[S, T] {
  def toComponent(
    definition: DrawingDefinition[T],
    context: DrawingContext
  ): DrawingComponent[S, T]
}

class MaterializerDefinitionToComponent[S, T](
  materializer: Materializer[S]
) extends DefinitionToComponent[S, T] {

  override def toComponent(
    definition: DrawingDefinition[T],
    context: DrawingContext
  ): DrawingComponent[S, T] = {
    new EvolutionDrawingComponent[S, T, definition.Config](
      materializer,
      ConfiguredDrawing[T, definition.Config](
        // @todo find a better solution
        definition.asInstanceOf[DrawingDefinition.Aux[T, definition.Config]],
        context,
        definition.initialConfig
      )
    )
  }
}
