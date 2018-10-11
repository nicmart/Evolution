package evolution.primitive.algebra.list.generator
import evolution.generator.Generator
import evolution.primitive.algebra.{Composed, GenRepr}
import evolution.primitive.algebra.list.ListAlgebra
import org.scalacheck.Gen

class ListAlgebraGenerator[S[_], F[_], R[_]](alg: ListAlgebra[S, F, R]) extends ListAlgebra[S, F, GenRepr[R, ?]] {

  override def empty[A]: GenRepr[R, F[A]] =
    _ => Generator.pure[R[F[A]]](alg.empty)

  override def cons[A](genHead: GenRepr[R, S[A]], genTail: GenRepr[R, F[A]]): GenRepr[R, F[A]] =
    n =>
      for {
        head <- resized(genHead(n))
        tail <- resized(genTail(n))
      } yield alg.cons(head, tail)

  override def mapEmpty[A](gen1: GenRepr[R, F[A]])(gen2: GenRepr[R, F[A]]): GenRepr[R, F[A]] =
    n =>
      for {
        a1 <- resized(gen1(n))
        a2 <- resized(gen2(n))
      } yield alg.mapEmpty(a1)(a2)

  override def mapCons[A, B](genA: GenRepr[R, F[A]])(genFunc: GenRepr[R, S[A] => F[A] => F[B]]): GenRepr[R, F[B]] =
    n =>
      for {
        a <- resized(genA(n))
        f <- resized(genFunc(n))
      } yield alg.mapCons(a)(f)

  private def resized[T](gen: Generator[T]): Generator[T] = gen.resize(size => (size - 1) / 2)
}
