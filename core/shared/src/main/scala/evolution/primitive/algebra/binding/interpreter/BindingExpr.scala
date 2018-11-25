package evolution.primitive.algebra.binding.interpreter
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class BindingExpr[F[_]] extends Binding[Expr[F, ?], String, String] {

  override def v(name: String): String = name

  override def var0[A]: Expr[F, A] =
    new Expr[F, A] {
      override def run[R[_]](alg: Evolution[F, R, String, String]): R[A] =
        alg.bind.var0
    }

  override def shift[A](expr: Expr[F, A]): Expr[F, A] =
    new Expr[F, A] {
      override def run[R[_]](alg: Evolution[F, R, String, String]): R[A] =
        alg.bind.shift(expr.run(alg))
    }

  override def let[A, B](variable: String, value: Expr[F, A], expr: Expr[F, B]): Expr[F, B] =
    new Expr[F, B] {
      override def run[R[_]](alg: Evolution[F, R, String, String]): R[B] =
        alg.bind.let(variable, value.run(alg), expr.run(alg))
    }

  override def lambda[A, B](variable: String, expr: Expr[F, B]): Expr[F, A => B] =
    new Expr[F, A => B] {
      override def run[R[_]](alg: Evolution[F, R, String, String]): R[A => B] =
        alg.bind.lambda(variable, expr.run(alg))
    }

  override def app[A, B](f: Expr[F, A => B], a: Expr[F, A]): Expr[F, B] =
    new Expr[F, B] {
      override def run[R[_]](alg: Evolution[F, R, String, String]): R[B] =
        alg.bind.app(f.run(alg), a.run(alg))
    }

  override def fix[A](expr: Expr[F, A => A]): Expr[F, A] =
    new Expr[F, A] {
      override def run[R[_]](alg: Evolution[F, R, String, String]): R[A] =
        alg.bind.fix(expr.run(alg))
    }
}
