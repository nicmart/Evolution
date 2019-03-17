package evolution.rng
import evolution.materialization.RNGRepr

class PerlinNoise(permutation256: Array[Int]) {
  def noise(x: Double, y: Double): Double = {
    val xi = Math.floor(x).toInt & 255
    val yi = Math.floor(y).toInt & 255
    val g1 = p(p(xi) + yi)
    val g2 = p(p(xi + 1) + yi)
    val g3 = p(p(xi) + yi + 1)
    val g4 = p(p(xi + 1) + yi + 1)
    val xf = x - Math.floor(x)
    val yf = y - Math.floor(y)
    val d1 = grad(g1, xf, yf)
    val d2 = grad(g2, xf - 1, yf)
    val d3 = grad(g3, xf, yf - 1)
    val d4 = grad(g4, xf - 1, yf - 1)
    val u = fade(xf)
    val v = fade(yf)
    val x1Inter = lerp(u, d1, d2)
    val x2Inter = lerp(u, d3, d4)
    val yInter = lerp(v, x1Inter, x2Inter)
    yInter
  }

  private val p = new Array[Int](512)

  (0 to 255).foreach { i: Int =>
    p(i) = permutation256(i)
    p(256 + i) = permutation256(i)
  }

  private def lerp(amount: Double, left: Double, right: Double): Double =
    (1 - amount) * left + amount * right

  private def fade(t: Double): Double = t * t * t * (t * (t * 6 - 15) + 10)

  private def grad(hash: Int, x: Double, y: Double): Double = hash & 3 match {
    case 0 =>
      x + y
    case 1 =>
      -x + y
    case 2 =>
      x - y
    case 3 =>
      -x - y
    case _ =>
      0
  }
}

object PerlinNoise {
  private val range: List[Int] = (0 to 255).toList

  def rngRepr: RNGRepr[Double => Double => Double] = {
    lazy val self: RNGRepr[Double => Double => Double] =
      RNGRepr.map(
        RNGRepr.shuffle(range),
        (permutation: List[Int]) => (new PerlinNoise(permutation.toArray).noise _).curried)
    self
  }
}