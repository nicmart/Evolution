package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.AbstractDrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.geometry.Point
import evolution.algebra.syntax.all._
import evolution.app.react.component.config.instances._
import evolution.algebra
import evolution.algebra.Evolution

object primes extends AbstractDrawingDefinition("primes") {

  case class Config(
    p: Int,
    q: Int,
    size: Int
  )

  override def initialConfig =
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

  override def evolution(config: Config, context: DrawingContext): Evolution[Point] =
    new ThisEvolution(config, context)

  override def configComponent: ConfigComponent[Config] =
    ConfigComponent[Config]
}
