package evolution.app.model.configured

import evolution.app.react.component.config.ConfiguredComponent
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import evolution.algebra.materializer.Materializer

trait MaterializableDrawing[S, T] {
  def name: String
  def materialize(seed: S): Stream[T]
  def configElement(onChange: MaterializableDrawing[S, T] => Callback): VdomElement
}

class EvolutionMaterializableDrawing[S, T, Config](
  val name: String,
  materializer: Materializer[S],
  configuredEvolution: ConfiguredEvolution[T, Config],
  configuredComponent: ConfiguredComponent[Config]
) extends MaterializableDrawing[S,T] {

  override def configElement(callback: MaterializableDrawing[S, T] => Callback): VdomElement = {
    configuredComponent.element(config => callback(withConfig(config)))
  }

  override def materialize(seed: S): Stream[T] =
    materializer.materialize(seed, configuredEvolution.evolution)

  private def withConfig(config: Config): MaterializableDrawing[S, T] =
    new EvolutionMaterializableDrawing[S, T, Config](
      name,
      materializer,
      configuredEvolution.withConfig(config),
      configuredComponent.withConfig(config)
    )
}



