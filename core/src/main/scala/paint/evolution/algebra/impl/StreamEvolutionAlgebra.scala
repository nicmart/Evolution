package paint.evolution.algebra.impl

import paint.evolution.algebra.MaterializableFullAlgebra

final class StreamEvolutionAlgebra extends MaterializableFullAlgebra[Stream, Any] {
  override val empty: Stream[Nothing] = Stream.empty
  override def run[A](evo: Stream[A], world: Any): Stream[A] = evo
  override def cons[A](head: A, tail: => Stream[A]): Stream[A] = head #:: tail
  override def mapCons[A, B](eva: Stream[A])(f: (A, Stream[A]) => Stream[B]): Stream[B] =
    if (eva.isEmpty) empty else {
      f(eva.head, eva.tail)
    }
  override def mapEmpty[A](eva: Stream[A])(eva2: => Stream[A]): Stream[A] =
    if (eva.isEmpty) eva2 else eva
  override def int: Stream[Int] = 0 #:: int
}
