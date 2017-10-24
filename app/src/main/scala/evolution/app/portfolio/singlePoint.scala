package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.EvolutionLegacy
import paint.geometry.Geometry.Point

object singlePoint extends DrawingDefinition("single constant point") {
  type Config = Unit
  protected def currentConfig: Unit = ()

  protected def evolution(config: Unit, context: DrawingContext): EvolutionLegacy[Point] =
    EvolutionLegacy.constant(Point(0, 0))

  protected def component: ConfigComponent[Unit] = ConfigComponent[Unit]
}
