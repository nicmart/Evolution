package evolution.app.model.definition

import evolution.app.model.context.DrawingContext
import evolution.app.react.component.config.ConfigComponent
import evolution.algebra.LegacyEvolution
import evolution.algebra.interpreter.RNGInterpreter
import evolution.algebra.materializer.RNGMaterializer
import evolution.app.codec.JsonCodec
import evolution.app.conf.Conf.{DrawingConfig, drawingDefinition, materializer}
import evolution.app.model.state.DrawingState
import evolution.geometry.Point

trait DrawingDefinition[T] {
  type Config
  def name: String
  def initialConfig: Config
  def stream(ctx: DrawingContext, state: DrawingState[Config]): Stream[T]
  def configComponent: ConfigComponent[Config]
  def configCodec: JsonCodec[Config]
}

object DrawingDefinition {
  type Aux[T, C] = DrawingDefinition[T] { type Config = C }
}

trait LegacyDrawingDefinition[T] extends DrawingDefinition[T] {
  override def stream(ctx: DrawingContext, state: DrawingState[Config]): Stream[T] =
    LegacyDrawingDefinition.materializer.materialize(state.seed, evolution(state.config, ctx))
  def evolution(config: Config, context: DrawingContext): LegacyEvolution[T]
}

object LegacyDrawingDefinition {
  type Aux[T, C] = LegacyDrawingDefinition[T] { type Config = C }
  val materializer = RNGMaterializer(new RNGInterpreter)
}
