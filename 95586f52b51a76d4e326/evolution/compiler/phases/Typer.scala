package evolution.compiler.phases

import evolution.compiler.phases.typer.model.Assumptions
import evolution.compiler.tree._
import evolution.compiler.types._

trait Typer {
  def typeTree(tree: Tree, expectedType: Option[Type], assumptions: Assumptions): Either[String, TypedTree]
}
