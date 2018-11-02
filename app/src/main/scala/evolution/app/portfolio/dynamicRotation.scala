package evolution.app.portfolio

import cats.implicits._
import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.{DrawingDefinition, LegacyDrawingDefinition}
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.app.react.component.config.instances._
import evolution.algebra.syntax.all._
import evolution.algebra.{FullAlgebra, LegacyEvolution}
import evolution.app.codec.JsonCodec
import evolution.app.codec.JsonCodec._
import io.circe.generic.auto._

object dynamicRotation extends LegacyDrawingDefinition[Point] {
  val name = "Dynamic Rotation"

  case class Config(
    normSpeed: Double,
    angularSpeed: Double,
    omega: Double,
    amplitude: Double,
    omega2: Double,
    amplitude2: Double
  )

  override def initialConfig =
    Config(normSpeed = 15, angularSpeed = 0.1, omega = 0.01, amplitude = 20, omega2 = 0.1, amplitude2 = 10)

  class ThisEvolution(config: Config, context: DrawingContext) extends LegacyEvolution[Point] {
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

      drawOnEvolution(toPhaseSpace(drawOnEvolution(spiral, vibration(omega, amplitude))), vibration(omega2, amplitude2))
    }
  }

  override def evolution(config: Config, context: DrawingContext): LegacyEvolution[Point] =
    new ThisEvolution(config, context)

  override val configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]

  override def configCodec: JsonCodec[Config] =
    JsonCodec[Config]
}
