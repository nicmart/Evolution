package evolution.app.model.configured

import evolution.app.model.context.DrawingContext
import paint.evolution.algebra.Evolution

final case class ConfiguredEvolution[T, Config](
  generator: (Config, DrawingContext) => Evolution[T],
  drawingContext: DrawingContext,
  config: Config
) {
  def evolution: Evolution[T] = generator(config, drawingContext)
  def withConfig(config: Config): ConfiguredEvolution[T, Config] =
    copy(config = config)
  def withContext(drawingContext: DrawingContext): ConfiguredEvolution[T, Config] =
    copy(drawingContext = drawingContext)
}