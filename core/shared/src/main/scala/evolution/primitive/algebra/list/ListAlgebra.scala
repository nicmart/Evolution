package evolution.primitive.algebra.list
import cats.~>

trait ListAlgebra[S[_], F[_], R[_]] {
  def empty[A]: R[F[A]]
  def cons[A](head: R[S[A]], tail: R[F[A]]): R[F[A]]
  def mapEmpty[A](eva: R[F[A]])(eva2: R[F[A]]): R[F[A]]
  def mapCons[A, B](eva: R[F[A]])(f: R[S[A] => F[A] => F[B]]): R[F[B]]
}

class ContextualListAlgebra[S[_], F[_], R[_], Ctx](alg: ListAlgebra[S, F, R])
    extends ListAlgebra[S, F, λ[α => Ctx => R[α]]] {
  override def empty[A]: Ctx => R[F[A]] = _ => alg.empty
  override def cons[A](head: Ctx => R[S[A]], tail: Ctx => R[F[A]]): Ctx => R[F[A]] =
    ctx => alg.cons(head(ctx), tail(ctx))
  override def mapEmpty[A](eva: Ctx => R[F[A]])(eva2: Ctx => R[F[A]]): Ctx => R[F[A]] =
    ctx => alg.mapEmpty(eva(ctx))(eva2(ctx))
  override def mapCons[A, B](eva: Ctx => R[F[A]])(f: Ctx => R[S[A] => F[A] => F[B]]): Ctx => R[F[B]] =
    ctx => alg.mapCons(eva(ctx))(f(ctx))
}

class MappedListAlgebra[S[_], F[_], R1[_], R2[_]](alg: ListAlgebra[S, F, R1], to: R1 ~> R2, from: R2 ~> R1)
    extends ListAlgebra[S, F, R2] {
  def empty[A]: R2[F[A]] =
    to(alg.empty)
  def cons[A](head: R2[S[A]], tail: R2[F[A]]): R2[F[A]] =
    to(alg.cons(from(head), from(tail)))
  def mapEmpty[A](eva: R2[F[A]])(eva2: R2[F[A]]): R2[F[A]] =
    to(alg.mapEmpty(from(eva))(from(eva2)))
  def mapCons[A, B](eva: R2[F[A]])(f: R2[S[A] => F[A] => F[B]]): R2[F[B]] =
    to(alg.mapCons(from(eva))(from(f)))
}
