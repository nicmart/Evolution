package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.evolution.Evolution
import paint.geometry.Geometry.Point
import evolution.app.react.component.config.instances._

object singlePoint extends DrawingDefinition("single constant point") {
  type Config = Unit
  protected def currentConfig: Unit = ()

  protected def evolution(config: Unit, context: DrawingContext): Evolution[Point] =
    Evolution.constant(Point(0, 0))

  protected def component: ConfigComponent[Unit] = ConfigComponent[Unit]
}
