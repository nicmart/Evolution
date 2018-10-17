package evolution.primitive.algebra.chain
import cats.~>
import cats.instances.function._
import evolution.primitive.algebra.chain.interpreter.ChainApplicative

trait Chain[S[_], F[_], R[_]] {
  def empty[A]: R[F[A]]
  def cons[A](head: R[S[A]], tail: R[F[A]]): R[F[A]]
  def mapEmpty[A](eva: R[F[A]])(eva2: R[F[A]]): R[F[A]]
  def mapCons[A, B](eva: R[F[A]])(f: R[S[A] => F[A] => F[B]]): R[F[B]]
}

class ContextualChain[S[_], F[_], R[_], Ctx](alg: Chain[S, F, R]) extends ChainApplicative[S, F, R, Ctx => ?](alg)

class MappedChain[S[_], F[_], R1[_], R2[_]](alg: Chain[S, F, R1], to: R1 ~> R2, from: R2 ~> R1)
    extends Chain[S, F, R2] {
  def empty[A]: R2[F[A]] =
    to(alg.empty)
  def cons[A](head: R2[S[A]], tail: R2[F[A]]): R2[F[A]] =
    to(alg.cons(from(head), from(tail)))
  def mapEmpty[A](eva: R2[F[A]])(eva2: R2[F[A]]): R2[F[A]] =
    to(alg.mapEmpty(from(eva))(from(eva2)))
  def mapCons[A, B](eva: R2[F[A]])(f: R2[S[A] => F[A] => F[B]]): R2[F[B]] =
    to(alg.mapCons(from(eva))(from(f)))
}
