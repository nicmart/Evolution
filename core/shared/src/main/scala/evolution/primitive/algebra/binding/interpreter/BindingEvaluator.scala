package evolution.primitive.algebra.binding.interpreter
import evolution.data.Result
import evolution.data.Result._
import evolution.data.EvaluationContextModule._
import evolution.primitive.algebra.binding.Binding

object BindingEvaluator extends Binding[Result, String] {
  override def var0[A](name: String): Result[A] = Var(0, name)

  override def shift[A](expr: Result[A]): Result[A] = expr match {
    case Var(n, name) => Var(n + 1, name)
    case _            => Value(ctx => expr.evaluate(ctx.pop))
  }
  override def let[A, B](name: String, value: Result[A], expr: Result[B]): Result[B] =
    app(lambda(name, expr), value)

  override def fix[A](expr: Result[A => A]): Result[A] =
    Fix(expr)

  override def lambda[A, B](name: String, expr: Result[B]): Result[A => B] =
    Lam(name, expr)

  override def app[A, B](f: Result[A => B], a: Result[A]): Result[B] =
    f match {
      case lambda @ Lam(_, _)                    => AppOfLambda(lambda, a)
      case AppOfLambda(Lam(_, Lam(_, inner)), b) => App2OfLambda[A, Any, B](inner, b, a)
      case _                                     => App(f, a)
    }
}
