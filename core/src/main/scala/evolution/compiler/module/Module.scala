package evolution.compiler.module
import evolution.compiler.types.TypeBindings
import evolution.compiler.expression.Expr

final case class Module(typeBindings: TypeBindings, load: Expr[Any] => Expr[Any]) {
  def compose(other: Module): Module =
    Module(typeBindings.merge(other.typeBindings), other.load andThen load)
}

object Module {
  val empty = Module(TypeBindings.empty, identity)
}
