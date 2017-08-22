package evolution.app.portfolio

import evolution.app.model._
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import paint.evolution.Evolution
import paint.geometry.Geometry.Point

object DrawingPortfolio {

  abstract class DrawingDefinition(val name: String) {
    protected type Config // <: WithCanvasSize
    protected def evolution(config: Config, context: DrawingContext): Evolution[Point]
    protected def currentConfig: Config
    protected def component: ConfigComponent[Config]
    def drawing(context: DrawingContext): ConfiguredDrawing[Point] =
      ConfiguredDrawing(
        name,
        ConfiguredEvolution(evolution, context, currentConfig),
        ConfiguredComponent(component, currentConfig)
      )
  }

  def listWithSelection: DrawingListWithSelection =
    DrawingListWithSelection(
      DrawingDefinitionList(List(
        brownian,
        brownianWithRandomJumps,
        drops,
        waves,
        curlyRing
      )),
      brownian
    )
}
