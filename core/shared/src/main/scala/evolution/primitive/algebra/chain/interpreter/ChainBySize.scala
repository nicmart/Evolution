package evolution.primitive.algebra.chain.interpreter

import cats.MonoidK
import evolution.primitive.algebra.Sized
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.evolution.parser.OrMonoid

class ChainBySize[F[_], R[_]](alg: Chain[F, R], val orMonoid: MonoidK[R])
    extends Chain[F, Sized[R, ?]]
    with OrMonoid[R] {
  override def empty[A]: Sized[R, F[A]] =
    size => if (size == 0) alg.empty else orMonoid.empty

  override def cons[A](head: Sized[R, A], tail: Sized[R, F[A]]): Sized[R, F[A]] =
    size => withSize(size - 1, head, tail, alg.cons)

  override def mapEmpty[A](eva: Sized[R, F[A]], eva2: Sized[R, F[A]]): Sized[R, F[A]] =
    size => withSize(size - 1, eva, eva2, alg.mapEmpty)

  override def mapCons[A, B](eva: Sized[R, F[A]])(f: Sized[R, A => F[A] => F[B]]): Sized[R, F[B]] =
    size => withSize[F[A], A => F[A] => F[B], F[B]](size - 1, eva, f, (a, b) => alg.mapCons(a)(b))

  private def withSize[T1, T2, T3](n: Int, a: Sized[R, T1], b: Sized[R, T2], f: (R[T1], R[T2]) => R[T3]): R[T3] = {
    val list = for {
      i <- (0 to n).toList
      sa = a(i)
      sb = b(n - i)
    } yield f(sa, sb)

    orSeq(list)
  }
}
