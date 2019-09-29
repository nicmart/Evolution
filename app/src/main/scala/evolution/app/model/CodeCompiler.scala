package evolution.app.model

import evolution.geometry.Point
import evolution.compiler.phases.AllPhases
import evolution.compiler.types.TypeT
import evolution.compiler.phases.typing.config.TypingConfig
import evolution.app.model.context.DrawingContext
import evolution.compiler.module.Module
import evolution.compiler.expression.Expr

class CodeCompiler(allPhases: AllPhases) {
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Iterator[Point]] =
    allPhases
      .compile(
        code,
        TypeT.Evo(TypeT.Point),
        Module(TypingConfig.constantQualifiedTypes, loadVars(ctx))
      )
      .map { evolution =>
        evolution(seed)
      }

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
