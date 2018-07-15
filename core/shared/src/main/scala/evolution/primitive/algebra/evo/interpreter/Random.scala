package evolution.primitive.algebra.evo.interpreter

import evolution.primitive.algebra.evo.EvoExpr

object Random {
  val b = new Builder[Long]

  def int: EvoExpr[Long, Int] = b.autonomous { seed =>
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val n = (newSeed >>> 16).toInt
    (newSeed, Some(n))
  }

  def double: EvoExpr[Long, Double] = b.map(int) { n =>
    (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
  }
}
