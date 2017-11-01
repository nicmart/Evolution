package evolution.app.model.configured

import evolution.app.react.component.config.ConfiguredComponent
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import evolution.algebra.materializer.Materializer

trait DrawingComponent[S, T] {
  def name: String
  def materialize(seed: S): Stream[T]
  def configElement(onChange: DrawingComponent[S, T] => Callback): VdomElement
}

class EvolutionDrawingComponent[S, T, Config](
  materializer: Materializer[S],
  configuredDrawing: ConfiguredDrawing[T, Config]
) extends DrawingComponent[S,T] {

  override def name: String = configuredDrawing.drawing.name

  override def configElement(callback: DrawingComponent[S, T] => Callback): VdomElement = {
    configuredDrawing.configElement(config => callback(withConfig(config)))
  }

  override def materialize(seed: S): Stream[T] =
    materializer.materialize(seed, configuredDrawing.evolution)

  private def withConfig(config: Config): DrawingComponent[S, T] =
    new EvolutionDrawingComponent[S, T, Config](
      materializer,
      configuredDrawing.copy(config = config)
    )
}



