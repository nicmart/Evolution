package evolution.app.model.definition

import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.ConfigComponent

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
