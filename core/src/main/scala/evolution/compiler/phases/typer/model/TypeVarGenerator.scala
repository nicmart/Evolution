package evolution.compiler.phases.typer.model

import evolution.compiler.types.Type

private[typer] class TypeVarGenerator(total: Int) {
  def current: Type.Var = Type.Var(s"T$total")
  def next: TypeVarGenerator = new TypeVarGenerator(total + 1)
}

private[typer] object TypeVarGenerator {
  val empty = new TypeVarGenerator(0)
}
