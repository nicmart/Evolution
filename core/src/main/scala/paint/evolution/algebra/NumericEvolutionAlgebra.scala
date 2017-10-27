package paint.evolution.algebra

import paint.evolution.algebra.syntax.all._

trait NumericEvolutionAlgebra[Evo[+ _]] extends EvolutionAlgebra[Evo] {
  implicit private lazy val alg: EvolutionAlgebra[Evo] = this

  def int: Evo[Int]

  def intBetween(from: Int, to: Int): Evo[Int] =
    doubleBetween(from, to).map(_.toInt)

  val nonNegative: Evo[Int] =
    int mapCons { (n, int2) =>
      n match {
        case Int.MinValue => nonNegative
        case _ if n < 0 => -n :: nonNegative
        case _ => n :: nonNegative
      }
    }

  // @TODO quick fix to early init problem
  lazy val double: Evo[Double] =
    int.map { n =>
      (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
    }

  def doubleBetween(from: Double, to: Double): Evo[Double] =
    double.map(d => (to - from) * d + from)

  def ball(radius: Double): Evo[Double] =
    double.map(d => radius * (d * 2 - 1))

  // Gaussian / Normal distribution
  lazy val normal: Evo[Double] =
    ball(1.0)
      .zipWith(ball(1))((v1, v2) => (v1, v2, v1 * v1 + v2 * v2))
      .filter { case (_, _, s) => s > 0 && s < 1 }
      .map { case (v1, _, s) =>
        val multiplier = math.sqrt(-2 * math.log(s) / s)
        v1 * multiplier
      }
}
