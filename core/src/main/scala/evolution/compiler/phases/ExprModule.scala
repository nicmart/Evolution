package evolution.compiler.phases

import evolution.compiler.expression.Expr
import evolution.compiler.phases.typer.model.Assumptions

final case class ExprModule(assumptions: Assumptions, load: Expr[Any] => Expr[Any]) {
  def compose(other: ExprModule): ExprModule =
    ExprModule(assumptions.merge(other.assumptions), other.load andThen load)
}

object ExprModule {
  val empty = ExprModule(Assumptions.empty, identity)
}
