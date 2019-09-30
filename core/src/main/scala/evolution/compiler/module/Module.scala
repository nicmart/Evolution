package evolution.compiler.module
import evolution.compiler.types.TypeBindings
import evolution.compiler.expression.Expr
import evolution.compiler.phases.compiling.model.VarContext

final case class Module(typeBindings: TypeBindings, load: Expr[Any] => Expr[Any]) {
  def compose(other: Module): Module =
    Module(typeBindings.merge(other.typeBindings), other.load andThen load)

  def varContext: VarContext =
    ExtractVarContext(load(Expr.Var("dummy")))
}

object Module {
  val empty = Module(TypeBindings.empty, identity)
}
