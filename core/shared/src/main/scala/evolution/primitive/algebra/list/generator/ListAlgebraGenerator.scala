package evolution.primitive.algebra.list.generator
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.list.ListAlgebra
import org.scalacheck.Gen

class ListAlgebraGenerator[S[_], F[_], R[_]](alg: ListAlgebra[S, F, R]) extends ListAlgebra[S, F, Composed[Gen, R, ?]] {

  override def empty[A]: Gen[R[F[A]]] =
    Gen.const(alg.empty)

  override def cons[A](genHead: Gen[R[S[A]]], genTail: Gen[R[F[A]]]): Gen[R[F[A]]] =
    for {
      head <- genHead
      tail <- genTail
    } yield alg.cons(head, tail)

  override def mapEmpty[A](gen1: Gen[R[F[A]]])(gen2: Gen[R[F[A]]]): Gen[R[F[A]]] =
    for {
      a1 <- gen1
      a2 <- gen2
    } yield alg.mapEmpty(a1)(a2)

  override def mapCons[A, B](genA: Gen[R[F[A]]])(genFunc: Gen[R[S[A] => F[A] => F[B]]]): Gen[R[F[B]]] =
    for {
      a <- genA
      f <- genFunc
    } yield alg.mapCons(a)(f)
}
