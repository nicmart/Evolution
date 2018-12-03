package evolution.primitive.algebra.binding.interpreter
import evolution.data.Evaluation
import evolution.data.Evaluation._
import evolution.data.EvaluationContextModule._
import evolution.primitive.algebra.binding.Binding

object BindingEvaluator extends Binding[Evaluation, String] {
  override def var0[A](name: String): Evaluation[A] = Var(0, name)

  override def shift[A](expr: Evaluation[A]): Evaluation[A] = expr match {
    case Var(n, name) => Var(n + 1, name)
    case _            => Value(ctx => expr.evaluateWith(ctx.pop))
  }
  override def let[A, B](name: String, value: Evaluation[A], expr: Evaluation[B]): Evaluation[B] =
    app(lambda(name, expr), value)

  override def fix[A](expr: Evaluation[A => A]): Evaluation[A] =
    Fix(expr)

  override def lambda[A, B](name: String, expr: Evaluation[B]): Evaluation[A => B] =
    Lam(name, expr)

  override def app[A, B](f: Evaluation[A => B], a: Evaluation[A]): Evaluation[B] =
    f match {
      case lambda @ Lam(_, _)                    => AppOfLambda(lambda, a)
      case AppOfLambda(Lam(_, Lam(_, inner)), b) => App2OfLambda[A, Any, B](inner, b, a)
      case _                                     => App(f, a)
    }
}
