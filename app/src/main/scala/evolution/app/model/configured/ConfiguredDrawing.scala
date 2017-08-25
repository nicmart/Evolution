package evolution.app.model.configured

import evolution.app.react.component.config.ConfiguredComponent
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import paint.evolution.Evolution

sealed trait ConfiguredDrawing[T] {
  val name: String
  def evolution: Evolution[T]
  def configElement(onChange: ConfiguredDrawing[T] => Callback): VdomElement
}

object ConfiguredDrawing {
  def apply[T, Config](
    _name: String,
    _configuredEvolution: ConfiguredEvolution[T, Config],
    _configuredComponent: ConfiguredComponent[Config]
  ): ConfiguredDrawing[T] = new ConfiguredDrawing[T] {
    private val configuredEvolution: ConfiguredEvolution[T, Config] = _configuredEvolution
    private val configuredComponent: ConfiguredComponent[Config] = _configuredComponent
    override val name: String = _name

    override def evolution: Evolution[T] = configuredEvolution.evolution
    override def configElement(callback: ConfiguredDrawing[T] => Callback): VdomElement = {
      configuredComponent.element(config => callback(withConfig(config)))
    }

    private def withConfig(config: Config): ConfiguredDrawing[T] =
      ConfiguredDrawing(
        name,
        configuredEvolution.withConfig(config),
        configuredComponent.withConfig(config)
      )
  }
}



