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
import paint.evolution.Evolution.constant
import paint.evolution.motion.MotionEvolutions._
import paint.evolution.implicits._
import paint.evolution.motion.{AccelerationEvolution, AccelerationLaw, MotionEvolutions}
import paint.geometry.Geometry.Point

object dynamicRotation extends DrawingDefinition("Dynamic Rotation") {
  case class Config(
    normSpeed: Double,
    angularSpeed: Double,
    omega: Double,
    amplitude: Double,
    omega2: Double,
    amplitude2: Double
  )

  override protected def currentConfig =
    Config(
      normSpeed = 15,
      angularSpeed = 0.1,
      omega = 0.01,
      amplitude = 20,
      omega2 = 0.1,
      amplitude2 = 10
    )

  override protected def evolution(config: Config, context: DrawingContext) = {
    import config._

    def vibration(om: Double, ampl: Double): Evolution[Point] =
      solveIndependentStatic(0.0)(om).positional.map( d => Point(0, Math.sin(d) * ampl))

    // Make the spiral go at constant speed
    val time = solveStatic(0.0){ t =>
      (angularSpeed / normSpeed) * Math.sqrt(1 / (1 + Math.pow(t, 2)))
    }.positional

    val spiral = toPhaseSpace(time.map { t =>
      Point.polar(normSpeed * t, t)
    })

    centeredIn(context.canvasSize.point / 2) {
      drawOnEvolution(
        toPhaseSpace(drawOnEvolution(
          spiral,
          vibration(omega, amplitude)
        )),
        vibration(omega2, amplitude2)
      )
    }
  }

  override protected def component = ConfigComponent[Config]
}
