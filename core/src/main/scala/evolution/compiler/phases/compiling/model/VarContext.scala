package evolution.compiler.phases.compiling.model

case class VarContext(vars: List[String]) {
  def has(variable: String): Boolean = vars.contains(variable)
  def push(variable: String): VarContext = new VarContext(variable :: vars)
}

object VarContext {
  val empty: VarContext = new VarContext(List.empty)
}
