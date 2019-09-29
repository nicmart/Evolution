package evolution.compiler.phases.checkvars

import cats.data.Kleisli
import cats.implicits._
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.expression.Expr
import evolution.compiler.expression.Expr.Var
import evolution.compiler.expression.Expr.Let
import evolution.compiler.expression.Expr.Lambda

object CheckVars {
  def apply(expr: Expr[Any], ctx: VarContext): Either[String, Unit] =
    check(expr).run(ctx)

  private type Result[T] = Kleisli[Either[String, ?], VarContext, T]

  private def check(expr: Expr[Any]): Result[Unit] = expr match {
    case Var(name) =>
      varContext.flatMap { ctx =>
        if (ctx.has(name)) ().pure[Result]
        else s"Variable $name is not defined".raiseError[Result, Unit]
      }

    case Let(variable, value, expr) =>
      check(value).flatMap(_ => withVar(variable)(check(expr)))

    case Lambda(variable, expr) => withVar(variable)(check(expr))

    case _ => expr.children.traverse[Result, Unit](check).map(_ => ())
  }

  private def withVar[A](name: String)(ka: Result[A]): Result[A] =
    Kleisli.local[Either[String, ?], A, VarContext](ctx => ctx.push(name))(ka)

  private def varContext: Result[VarContext] = Kleisli((ctx: VarContext) => Right(ctx))
}
