package evolution.app.model.definition

import evolution.app.codec.JsonCodec
import evolution.app.model.context.DrawingContext
import evolution.app.model.state.DrawingState
import evolution.app.react.component.config.ConfigComponent
import evolution.app.portfolio.dsl
import evolution.geometry.Point

// TODO do we still need this as a trait?
trait DrawingDefinition {
  def name: String
  def initialConfig: dsl.Config
  def materialize(ctx: DrawingContext, state: DrawingState): Iterator[Point]
  def configComponent: ConfigComponent[dsl.Config]
  def configCodec: JsonCodec[dsl.Config]
}
