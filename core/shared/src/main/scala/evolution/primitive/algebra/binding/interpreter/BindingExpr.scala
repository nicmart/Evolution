package evolution.primitive.algebra.binding.interpreter
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class BindingExpr[S[_], F[_]] extends Binding[Expr[S, F, ?], String, String] {

  override def v(name: String): String = name

  override def var0[A]: Expr[S, F, A] =
    new Expr[S, F, A] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[A] =
        alg.bind.var0
    }

  override def shift[A](expr: Expr[S, F, A]): Expr[S, F, A] =
    new Expr[S, F, A] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[A] =
        alg.bind.shift(expr.run(alg))
    }

  override def let[A, B](variable: String, value: Expr[S, F, A], expr: Expr[S, F, B]): Expr[S, F, B] =
    new Expr[S, F, B] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[B] =
        alg.bind.let(variable, value.run(alg), expr.run(alg))
    }

  override def lambda[A, B](variable: String, expr: Expr[S, F, B]): Expr[S, F, A => B] =
    new Expr[S, F, A => B] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[A => B] =
        alg.bind.lambda(variable, expr.run(alg))
    }

  override def app[A, B](f: Expr[S, F, A => B], a: Expr[S, F, A]): Expr[S, F, B] =
    new Expr[S, F, B] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[B] =
        alg.bind.app(f.run(alg), a.run(alg))
    }

  override def fix[A](expr: Expr[S, F, A => A]): Expr[S, F, A] =
    new Expr[S, F, A] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[A] =
        alg.bind.fix(expr.run(alg))
    }
}
