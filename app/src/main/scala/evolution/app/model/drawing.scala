package evolution.app.model

import evolution.app.portfolio.DrawingPortfolio.DrawingDefinition
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.VdomElement
import paint.evolution.Evolution
import paint.geometry.Geometry.Point

sealed trait ConfiguredDrawing[T] {
  type Config
  val name: String
  def evolution: Evolution[T]
  def configElement(onChange: ConfiguredDrawing[T] => Callback): VdomElement
}

object ConfiguredDrawing {
  type Aux[T, C] = ConfiguredDrawing[T] { type Config = C }

  def apply[T, C](
    _name: String,
    _configuredEvolution: ConfiguredEvolution[T, C],
    _configuredComponent: ConfiguredComponent[C]
  ): Aux[T, C] = new ConfiguredDrawing[T] {
    type Config = C
    private val configuredEvolution: ConfiguredEvolution[T, Config] = _configuredEvolution
    private val configuredComponent: ConfiguredComponent[Config] = _configuredComponent
    override val name: String = _name

    override def evolution: Evolution[T] = configuredEvolution.evolution
    override def configElement(callback: ConfiguredDrawing[T] => Callback): VdomElement = {
      configuredComponent.element(config => callback(withConfig(config)))
    }

    private def withConfig(config: Config): ConfiguredDrawing.Aux[T, Config] =
      ConfiguredDrawing(
        name,
        configuredEvolution.withConfig(config),
        configuredComponent.withConfig(config)
      )
  }
}

final case class DrawingDefinitionList(drawings: List[DrawingDefinition]) {
  def drawing(name: String): Option[DrawingDefinition] =
    drawings.dropWhile(_.name != name).headOption
}

final case class DrawingListWithSelection(
  list: DrawingDefinitionList,
  current: DrawingDefinition
) {
  def select(drawingName: String): DrawingListWithSelection = {
    val newCurrent = list.drawing(drawingName).getOrElse(current)
    copy(current = newCurrent)
  }
}

final case class DrawingContext(
  canvasSize: DrawingContext.CanvasSize
)

object DrawingContext {
  final case class CanvasSize(width: Int, height: Int) {
    def point: Point = Point(width.toDouble, height.toDouble)
  }
}