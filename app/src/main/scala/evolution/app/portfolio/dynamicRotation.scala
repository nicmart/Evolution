package evolution.app.portfolio

import cats.implicits._
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.AbstractDrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.syntax.all._
import evolution.algebra.{Evolution, FullAlgebra}

object dynamicRotation extends AbstractDrawingDefinition("Dynamic Rotation") {

  case class Config(
    normSpeed: Double,
    angularSpeed: Double,
    omega: Double,
    amplitude: Double,
    omega2: Double,
    amplitude2: Double
  )

  override def initialConfig =
    Config(
      normSpeed = 15,
      angularSpeed = 0.1,
      omega = 0.01,
      amplitude = 20,
      omega2 = 0.1,
      amplitude2 = 10
    )

  class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      import config._

      def vibration(om: Double, ampl: Double): Evo[Point] =
        solveIndependentStatic(0.0)(om).positional.map(d => Point(0, Math.sin(d) * ampl))

      // Make the spiral go at constant speed
      val time = solveStatic(0.0) { t =>
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
          )
          ),
          vibration(omega2, amplitude2)
        )
      }
    }
  }

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  override def configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]
}
