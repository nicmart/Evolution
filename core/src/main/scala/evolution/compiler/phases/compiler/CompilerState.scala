package evolution.compiler.phases.compiler

import evolution.compiler.tree.TypedTree

private[compiler] case class CompilerState(bindings: Map[String, TypedTree]):
  def withBinding(name: String, expr: TypedTree): CompilerState =
    CompilerState(bindings.updated(name, expr))

  def binding(name: String): Option[TypedTree] = bindings.get(name)

private[compiler] object CompilerState:
  val empty: CompilerState = CompilerState(Map.empty)
