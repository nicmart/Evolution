package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import paint.geometry.Geometry.Point
import paint.evolution.algebra.syntax.all._
import evolution.app.react.component.config.instances._
import paint.evolution.{EvolutionLegacy, algebra}
import paint.evolution.algebra.Evolution

object primes extends DrawingDefinition("primes") {

  case class Config(
    p: Int,
    q: Int,
    size: Int
  )

  override protected def currentConfig =
    Config(
      p = 7,
      q = 101,
      size = 500
    )

  class ThisEvolution(config: Config, context: DrawingContext) extends Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = {
      import alg._
      def gcd(a: Int, b: Int): Int = {
        if (b == 0) a else gcd(b, a % b)
      }
      val gcdValue = gcd(config.p, config.q)
      val mod = config.p * config.q / gcdValue

      val start = (context.canvasSize.point - Point(config.size, config.size)) / 2

      centeredIn(start) {
        val integers = intBetween(0, mod)
        val value = integers
          .zipWith(integers)((n, m) => (config.p * n + config.q * m) % mod)
          .map(n => config.size.toDouble * n / mod)
        cartesian(value, value)
      }
    }
  }

  override protected def evolution(config: Config, context: DrawingContext): EvolutionLegacy[Point] =
    new ThisEvolution(config, context).run

  override protected def component: ConfigComponent[Config] =
    ConfigComponent[Config]
}
