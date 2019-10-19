package evolution.compiler.phases

import evolution.compiler.tree._
import evolution.compiler.expression.Expr
import evolution.compiler.module.Module

trait Compiler {
  def compile(tree: TypedTree, module: Module): Either[String, Expr[_]]
}
