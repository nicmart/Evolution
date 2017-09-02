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
      normSpeed = 0.01,
      angularSpeed = 0.001,
      omega = 0.01,
      amplitude = 30,
      omega2 = 0.2,
      amplitude2 = 10
    )

  override protected def evolution(config: Config, context: DrawingContext) = {
    import config._
    val spiral = toPhaseSpace(polar(
      solveIndependentStatic(0.0)(normSpeed).positional,
      solveIndependentStatic(0.0)(angularSpeed).positional
    ))

    val spiralNormOfSpeed: Evolution[Double] = spiral.map { case (pos, speed) => speed.norm() }
    solveIndependent(0.0)(spiralNormOfSpeed).positional

    def vibration(om: Double, ampl: Double): Evolution[Point] =
      solveIndependentStatic(0.0)(om).positional.map( d => Point(0, Math.sin(d) * ampl))

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
