package evolution.app.model.definition

import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra.LegacyEvolution
import evolution.algebra.interpreter.RNGInterpreter
import evolution.app.codec.JsonCodec
import evolution.app.model.state.DrawingState
import evolution.random.RNG

trait Drawer[T] {
  def drawPoint(x: Double, y: Double): T
}

trait DrawingDefinition[T] {
  type Config
  def name: String
  def initialConfig: Config
  def stream(ctx: DrawingContext, state: DrawingState[Config]): Iterator[T]
  def configComponent: ConfigComponent[Config]
  def configCodec: JsonCodec[Config]
}

object DrawingDefinition {
  type Aux[T, C] = DrawingDefinition[T] { type Config = C }
}

trait LegacyDrawingDefinition[T] extends DrawingDefinition[T] {
  override def stream(ctx: DrawingContext, state: DrawingState[Config]): Iterator[T] =
    evolution(state.config, ctx).run(interpreter).iterator(RNG(state.seed))
  def evolution(config: Config, context: DrawingContext): LegacyEvolution[T]
  private val interpreter = new RNGInterpreter
}

object LegacyDrawingDefinition {
  type Aux[T, C] = LegacyDrawingDefinition[T] { type Config = C }
}
