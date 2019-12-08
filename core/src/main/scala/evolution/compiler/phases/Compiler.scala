package evolution.compiler.phases

import evolution.compiler.tree._
import evolution.compiler.expression.Expr

trait Compiler {
  def compile(tree: TypedTree, module: ExprModule): Either[String, Expr[_]]
}
