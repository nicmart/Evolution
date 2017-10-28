package evolution.algebra.interpreter

import evolution.algebra.FullAlgebra

import scala.util.Random

final class StreamInterpreter extends FullAlgebra[Stream] {
  override val empty: Stream[Nothing] = Stream.empty
  override def cons[A](head: A, tail: => Stream[A]): Stream[A] = head #:: tail
  override def mapCons[A, B](eva: Stream[A])(f: (A, Stream[A]) => Stream[B]): Stream[B] =
    if (eva.isEmpty) empty else {
      f(eva.head, eva.tail)
    }
  override def mapEmpty[A](eva: Stream[A])(eva2: => Stream[A]): Stream[A] =
    if (eva.isEmpty) eva2 else eva
  override def int: Stream[Int] = Random.nextInt() #:: int
}