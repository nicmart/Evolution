package evolution.primitive.algebra.binding.interpreter
import cats.Applicative
import cats.syntax._
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.Binding

class BindingApplicative[R[_], Var, F[_]: Applicative](alg: Binding[R, Var])
    extends Binding[Composed[F, R, ?], F[Var]] {
  override def var0[A]: Composed[F, R, A] =
    Applicative[F].pure(alg.var0)
  override def shift[A](expr: Composed[F, R, A]): Composed[F, R, A] =
    Applicative[F].map(expr)(alg.shift)
  override def let[A, B](name: F[Var], value: Composed[F, R, A], expr: Composed[F, R, B]): Composed[F, R, B] =
    Applicative[F].map3(name, value, expr)(alg.let)
  override def lambda[A, B](name: F[Var], expr: Composed[F, R, B]): Composed[F, R, A => B] =
    Applicative[F].map2(name, expr)(alg.lambda)
  override def app[A, B](f: Composed[F, R, A => B], a: Composed[F, R, A]): Composed[F, R, B] =
    Applicative[F].map2(f, a)(alg.app)
  override def fix[A](expr: Composed[F, R, A => A]): Composed[F, R, A] =
    Applicative[F].map(expr)(alg.fix)
}
