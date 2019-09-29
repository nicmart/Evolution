package evolution.compiler.module
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.expression.Expr
import evolution.compiler.expression.Expr.Let

private[module] object ExtractVarContext {
  def apply(expr: Expr[Any]): VarContext = buildContext(VarContext.empty)(expr)

  private def buildContext(ctx: VarContext)(expr: Expr[Any]): VarContext =
    expr match {
      case Let(variable, value, body) => buildContext(ctx.push(variable))(body)
      case _                          => ctx
    }
}
