package evolution.compiler.phases.typing

import evolution.compiler.types.Type

class TypeVarGenerator(total: Int) {
  def current: Type.Var = Type.Var(s"T$total")
  def next: TypeVarGenerator = new TypeVarGenerator(total + 1)
}

object TypeVarGenerator {
  val empty = new TypeVarGenerator(0)
}
