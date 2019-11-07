package evolution.compiler.module
import evolution.compiler.types.Assumptions
import evolution.compiler.expression.Expr
import evolution.compiler.phases.checkvars.model.VarContext

final case class Module(assumptions: Assumptions, load: Expr[Any] => Expr[Any]) {
  def compose(other: Module): Module =
    Module(assumptions.merge(other.assumptions), other.load andThen load)

  def varContext: VarContext =
    ExtractVarContext(load(Expr.Var("dummy")))
}

object Module {
  val empty = Module(Assumptions.empty, identity)
}
