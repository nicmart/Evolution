package evolution.app.model

import evolution.app.react.component.config.ConfigComponent
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import paint.evolution.Evolution
import paint.evolution.generator.EvolutionGenerator

sealed trait Drawing[T] {
  type Config
  val name: String
  val generator: EvolutionGenerator[T, Config]
  val configComponent: ConfigComponent[Config]
  val config: Config

  def evolution: Evolution[T] = generator.evolution(config)

  def configElement(callback: Drawing[T] => Callback): VdomElement = {
    configComponent.component(ConfigComponent.Props(
      config,
      config => callback(withConfig(config))
    ))
  }

  def withConfig(config: Config): Drawing.Aux[T, Config] =
    Drawing(name, generator, configComponent, config)
}

object Drawing {
  type Aux[T, C] = Drawing[T] { type Config = C }

  def apply[T, C](
                   _name: String,
                   _generator: EvolutionGenerator[T, C],
                   _component: ConfigComponent[C],
                   _config: C
  ): Aux[T, C] = new Drawing[T] {
    type Config = C
    override val name = _name
    override val generator = _generator
    override val config = _config
    override val configComponent = _component
  }
}

case class DrawingList[T](drawings: List[Drawing[T]]) {
  def drawing(name: String): Option[Drawing[T]] =
    drawings.dropWhile(_.name != name).headOption
}

case class DrawingListWithSelection[T](
  list: DrawingList[T],
  current: Drawing[T]
) {
  def select(drawingName: String): DrawingListWithSelection[T] = {
    val newCurrent = list.drawing(drawingName).getOrElse(current)
    copy(current = newCurrent)
  }
}