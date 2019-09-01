package evolution.compilertree.phases.typing.model

import evolution.compilertree.types.Type

class TypeVarGenerator(total: Int) {
  def current: Type.Var = Type.Var(s"T$total")
  def next: TypeVarGenerator = new TypeVarGenerator(total + 1)
}

object TypeVarGenerator {
  val empty = new TypeVarGenerator(0)
}
