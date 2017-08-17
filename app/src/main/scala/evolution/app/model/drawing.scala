package evolution.app.model

import evolution.app.react.component.settings.SettingsComponent
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import paint.evolution.Evolution
import paint.evolution.generator.EvolutionGenerator

sealed trait Drawing[T] {
  type Settings
  val name: String
  val generator: EvolutionGenerator[T, Settings]
  val settingsComponent: SettingsComponent[Settings]
  val settings: Settings

  def evolution: Evolution[T] = generator.evolution(settings)

  def settingsElement(callback: Drawing[T] => Callback): VdomElement = {
    settingsComponent.component(SettingsComponent.Props(
      settings,
      settings => callback(withSettings(settings))
    ))
  }

  def withSettings(settings: Settings): Drawing.Aux[T, Settings] =
    Drawing(name, generator, settingsComponent, settings)
}

object Drawing {
  type Aux[T, C] = Drawing[T] { type Settings = C }

  def apply[T, S](
    _name: String,
    _generator: EvolutionGenerator[T, S],
    _component: SettingsComponent[S],
    _settings: S
  ): Aux[T, S] = new Drawing[T] {
    type Settings = S
    val name = _name
    val generator = _generator
    val settings = _settings
    val settingsComponent = _component
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