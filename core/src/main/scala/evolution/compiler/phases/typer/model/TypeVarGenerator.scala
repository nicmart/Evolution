package evolution.compiler.phases.typer.model

import evolution.compiler.types.TypeT

class TypeVarGenerator(total: Int) {
  def current: TypeT.Var = TypeT.Var(s"T$total")
  def next: TypeVarGenerator = new TypeVarGenerator(total + 1)
}

object TypeVarGenerator {
  val empty = new TypeVarGenerator(0)
}
