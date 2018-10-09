package evolution.primitive.algebra.list.generator
import evolution.primitive.algebra.{Composed, Generator}
import evolution.primitive.algebra.list.ListAlgebra
import org.scalacheck.Gen

class ListAlgebraGenerator[S[_], F[_], R[_]](alg: ListAlgebra[S, F, R]) extends ListAlgebra[S, F, Generator[R, ?]] {

  override def empty[A]: Generator[R, F[A]] =
    Gen.const(alg.empty)

  override def cons[A](genHead: Generator[R, S[A]], genTail: Generator[R, F[A]]): Generator[R, F[A]] =
    for {
      head <- genHead
      tail <- genTail
    } yield alg.cons(head, tail)

  override def mapEmpty[A](gen1: Generator[R, F[A]])(gen2: Generator[R, F[A]]): Generator[R, F[A]] =
    for {
      a1 <- gen1
      a2 <- gen2
    } yield alg.mapEmpty(a1)(a2)

  override def mapCons[A, B](
    genA: Generator[R, F[A]]
  )(genFunc: Generator[R, S[A] => F[A] => F[B]]): Generator[R, F[B]] =
    for {
      a <- genA
      f <- genFunc
    } yield alg.mapCons(a)(f)
}
