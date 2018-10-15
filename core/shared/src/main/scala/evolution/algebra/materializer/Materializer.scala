package evolution.algebra.materializer

import evolution.algebra.LegacyEvolution

trait Materializer[S] {
  def materialize[A](seed: S, evo: LegacyEvolution[A]): Stream[A]
}
