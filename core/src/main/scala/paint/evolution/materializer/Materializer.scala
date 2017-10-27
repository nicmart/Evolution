package paint.evolution.materializer

import paint.evolution.algebra.Evolution

trait Materializer[S] {
  def materialize[A](seed: S, evo: Evolution[A]): Stream[A]
}
