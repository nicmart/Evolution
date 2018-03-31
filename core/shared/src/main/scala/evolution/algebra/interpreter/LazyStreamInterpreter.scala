package evolution.algebra.interpreter

import evolution.algebra.interpreter.LazyStreamInterpreter.LazyStream
import evolution.algebra.FullAlgebra

import scala.util.Random

final class LazyStreamInterpreter extends FullAlgebra[LazyStream]
{
  override val empty: LazyStream[Nothing] = () => Stream.empty
  override def cons[A](head: A, tail: => LazyStream[A]): LazyStream[A] = () => head #:: tail()
  override def mapCons[A, B](eva: LazyStream[A])(f: (A, LazyStream[A]) => LazyStream[B]): LazyStream[B] =
    () => {
      val stream = eva()
      if (stream.isEmpty) empty() else {
        f(stream.head, () => stream.tail)()
      }
    }
  override def mapEmpty[A](eva: LazyStream[A])(eva2: => LazyStream[A]): LazyStream[A] =
    () => {
      val stream = eva()
      if (stream.isEmpty) eva2() else stream
    }
  override val int: LazyStream[Int] = () => Random.nextInt() #:: int()

  override def concat[A](
    evo1: LazyStream[A],
    evo2: => LazyStream[A]
  ): LazyStream[A] = () => evo1() append evo2()
  override def flatMap[A, B](eva: LazyStream[A])
    (f: (A) => LazyStream[B]): LazyStream[B] =
    () => eva().flatMap(x => f(x)())
  override def map[A, B](eva: LazyStream[A])
    (f: (A) => B): LazyStream[B] = () => eva().map(f)
  override def zipWith[A, B, C](
    eva: LazyStream[A],
    evb: LazyStream[B]
  )(f: (A, B) => C): LazyStream[C] = () =>
    eva().zip(evb()).map(f.tupled)
}

object LazyStreamInterpreter {
  type LazyStream[+A] = () => Stream[A]
}
