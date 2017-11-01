package evolution.app.model.configured

import evolution.algebra.Evolution
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement

case class ConfiguredDrawing[T, Config](
  drawing: DrawingDefinition.Aux[T, Config],
  context: DrawingContext,
  config: Config
) {
  def configElement(onChange: Config => Callback): VdomElement =
    ConfiguredComponent(drawing.configComponent, config).element(onChange)

  def evolution: Evolution[T] =
    drawing.evolution(config, context)
}
