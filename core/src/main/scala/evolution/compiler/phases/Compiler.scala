package evolution.compiler.phases

import evolution.compiler.tree._
import evolution.compiler.expression.Expr

trait Compiler {
  def compile(tree: TypedTree, module: Module): Either[String, Expr[_]]
}
