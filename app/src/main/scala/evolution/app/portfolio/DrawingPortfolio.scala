package evolution.app.portfolio

import evolution.app.model.{Drawing, DrawingList, DrawingListWithSelection}
import evolution.app.react.component.config.{ConfigComponent, ConfiguredComponent}
import paint.evolution.Evolution
import paint.evolution.generator.ConfiguredEvolution
import paint.geometry.Geometry.Point

object DrawingPortfolio {

  trait WithCanvasSize {
    def canvasSize: Point
  }

  abstract class DrawingDefinition(name: String) {
    type Config// <: WithCanvasSize
    def evolution(config: Config): Evolution[Point]
    def defaultConfig: Config
    def component: ConfigComponent[Config]
    def drawing: Drawing[Point] =
      Drawing(
        name,
        ConfiguredEvolution(evolution, defaultConfig),
        ConfiguredComponent(component, defaultConfig)
      )
  }

  def listWithSelection: DrawingListWithSelection[Point] =
    DrawingListWithSelection(
      DrawingList(List(
        brownian.drawing,
        brownianWithRandomJumps.drawing,
        drops.drawing,
        waves.drawing,
        curlyRing.drawing
      )),
      brownian.drawing
    )
}
