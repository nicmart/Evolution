package evolution.app.model

import evolution.geometry.Point
import evolution.compiler.phases.{FullCompiler, ExprModule}
import evolution.compiler.types.Type
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.app.model.context.DrawingContext
import evolution.compiler.expression.Expr
import evolution.compiler.stdlib.StandardLibraryExprModule

final class CodeCompiler(fullCompiler: FullCompiler) {
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Iterator[Point]] =
    module(ctx).flatMap(
      mod =>
        fullCompiler
          .compile(
            code,
            Type.Evo(Type.Point),
            mod
          )
          .map { evolution =>
            evolution.asInstanceOf[Long => Iterator[Point]](seed)
          }
    )

  private def module(ctx: DrawingContext): Either[String, ExprModule] =
    StandardLibraryExprModule.module.map(ExprModule(TypingConfig.constantQualifiedTypes, loadVars(ctx)).compose)

  private def loadVars(ctx: DrawingContext)(expr: Expr[Any]): Expr[Any] = {
    val vars = List(
      "top" -> Expr.Dbl(ctx.top),
      "bottom" -> Expr.Dbl(ctx.bottom),
      "left" -> Expr.Dbl(ctx.left),
      "right" -> Expr.Dbl(ctx.right)
    )

    vars.foldLeft(expr) { case (accExpr, (varname, varbody)) => Expr.Let(varname, varbody, accExpr) }
  }
}
