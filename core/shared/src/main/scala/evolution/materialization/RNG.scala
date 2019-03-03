package evolution.materialization

final case class RNG(seed: Long) {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = RNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  // A double between 0 and 1
  def nextDouble: (Double, RNG) = {
    val (n, rng2) = nextInt
    val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
    (d, rng2)
  }
}

object RNG {
  def next(seed: Long): (Int, Long) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val n = (newSeed >>> 16).toInt
    (n, newSeed)
  }
}
