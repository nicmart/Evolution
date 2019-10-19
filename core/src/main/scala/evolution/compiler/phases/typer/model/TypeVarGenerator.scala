package evolution.compiler.phases.typer.model

import evolution.compiler.types.TypeT

private[typer] class TypeVarGenerator(total: Int) {
  def current: TypeT.Var = TypeT.Var(s"T$total")
  def next: TypeVarGenerator = new TypeVarGenerator(total + 1)
}

private[typer] object TypeVarGenerator {
  val empty = new TypeVarGenerator(0)
}
