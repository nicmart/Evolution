package evolution.materialization

final case class RNG(seed: Long) {
  RNG.allocations += 1

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0X5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
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
  private var allocations: Int = 0
  def resetAllocationsCount(): Unit = allocations = 0
  def allocationsCount: Int = allocations

  def next(seed: Long): (Int, Long) = {
    val newSeed = (seed * 0X5DEECE66DL + 0XBL) & 0XFFFFFFFFFFFFL
    val n = (newSeed >>> 16).toInt
    (n, newSeed)
  }
}
