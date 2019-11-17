package evolution.compiler.module
import evolution.compiler.expression.Expr
import evolution.compiler.phases.typer.model.Assumptions

final case class Module(assumptions: Assumptions, load: Expr[Any] => Expr[Any]) {
  def compose(other: Module): Module =
    Module(assumptions.merge(other.assumptions), other.load andThen load)
}

object Module {
  val empty = Module(Assumptions.empty, identity)
}
