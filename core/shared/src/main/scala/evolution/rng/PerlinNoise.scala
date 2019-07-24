package evolution.rng

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

  // See https://flafla2.github.io/2014/08/09/perlinnoise.html
  def octaveNoise(octaves: Int, persistence: Double, x: Double, y: Double): Double = {
    var total: Double = 0
    var frequency = 1
    var amplitude: Double = 1
    var maxValue: Double = 0 // Used for normalizing result to 0.0 - 1.0
    (0 until octaves).foreach { _ =>
      total += noise(x * frequency, y * frequency) * amplitude
      maxValue += amplitude
      amplitude *= persistence
      frequency *= 2
    }

    total / maxValue
  }

  private val p = new Array[Int](512)

  (0 to 255).foreach { i: Int =>
    p(i) = permutation256(i)
    p(256 + i) = permutation256(i)
  }

  private def lerp(amount: Double, left: Double, right: Double): Double =
    (1 - amount) * left + amount * right

  private def fade(t: Double): Double = t * t * t * (t * (t * 6 - 15) + 10)

  private def grad(hash: Int, x: Double, y: Double): Double =
    x * Math.cos(2 * Math.PI * hash / 256) + y * Math.sin(2 * Math.PI * hash / 256)
}