package evolution.app.portfolio

import evolution.app.model.context.DrawingContext
import evolution.app.model.definition.DrawingDefinition
import evolution.app.react.component.config.ConfigComponent
import evolution.app.react.component.config.instances._
import paint.evolution.Evolution
import paint.evolution.PointEvolutions._
import paint.evolution.SemigroupEvolutions._
import paint.evolution.NumericEvolutions._
import paint.evolution.implicits._
import paint.geometry.Geometry.Point

object primes extends DrawingDefinition("primes") {
  case class Config(
    p :Int,
    q: Int,
    size: Int
  )

  override protected def currentConfig =
    Config(
      p = 7,
      q = 101,
      size = 500
    )

  override protected def evolution(
    config: Config,
    context: DrawingContext
  ) = {
    def gcd(a: Int,b: Int): Int = {
      if(b ==0) a else gcd(b, a%b)
    }
    val gcdValue = gcd(config.p, config.q)
    val mod = config.p * config.q / gcdValue

    val start = (context.canvasSize.point - Point(config.size, config.size)) / 2

    centeredIn(start) {
      val integers = intRange(0, mod)
      val value = integers
        .zipWith(integers)((n, m) => (config.p * n + config.q * m) % mod)
        .map(n => config.size.toDouble * n / mod)
      cartesian(value, value)
    }
  }

  override protected def component = ConfigComponent[Config]
}
