package evolution.app.model

import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import paint.evolution.Evolution
import paint.evolution.generator.ConfiguredEvolution

sealed trait Drawing[T] {
  type Config
  val name: String
  def evolution: Evolution[T]
  def configElement(onChange: Drawing[T] => Callback): VdomElement
}

object Drawing {
  type Aux[T, C] = Drawing[T] { type Config = C }

  def apply[T, C](
    _name: String,
    _configuredEvolution: ConfiguredEvolution[T, C],
    _configuredComponent: ConfiguredComponent[C]
  ): Aux[T, C] = new Drawing[T] {
    type Config = C
    private val configuredEvolution: ConfiguredEvolution[T, Config] = _configuredEvolution
    private val configuredComponent: ConfiguredComponent[Config] = _configuredComponent
    override val name = _name

    override def evolution: Evolution[T] = configuredEvolution.evolution
    override def configElement(callback: Drawing[T] => Callback): VdomElement = {
      configuredComponent.element(config => callback(withConfig(config)))
    }

    private def withConfig(config: Config): Drawing.Aux[T, Config] =
      Drawing(
        name,
        configuredEvolution.withConfig(config),
        configuredComponent.withConfig(config)
      )
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