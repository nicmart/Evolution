package evolution.algebra.materializer

import evolution.algebra.Evolution

trait Materializer[S] {
  def materialize[A](seed: S, evo: Evolution[A]): Stream[A]
}
