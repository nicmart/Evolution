package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.Evolution
import paint.evolution.PointEvolutions._
import paint.evolution.SemigroupEvolutions._
import paint.evolution.NumericEvolutions._
import cats.implicits._
import paint.evolution.motion.MotionEvolutions._
import paint.evolution.implicits._
import paint.geometry.Geometry.Point

object dynamicRotation extends DrawingDefinition("Dynamic Rotation") {
  case class Config(
    horizontalSpeed: Double,
    angleSpeed: Double,
    centerSpeed: Double
  )

  override protected def currentConfig =
    Config(
      horizontalSpeed = 0.01,
      angleSpeed = 0.01,
      centerSpeed = 0.01
    )

  override protected def evolution(config: Config, context: DrawingContext) = {
    import config._
    val line = solveIndependentStatic(Point.zero)(Point(horizontalSpeed, 0)).positional
    val angle = solveIndependentStatic(0.0)(angleSpeed).positional
    val center = uniformRadial(Point.zero, centerSpeed)

    def evo1 = evoRotate(center, angle)(line)

    centeredIn(context.canvasSize.point / 2) {
      evoRotate(evo1, angle)(line)
    }
  }

  override protected def component = ConfigComponent[Config]
}
