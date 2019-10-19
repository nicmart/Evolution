package evolution.compiler.phases

import evolution.compiler.tree._
import evolution.compiler.types._
import evolution.compiler.module.Module

trait Typer {
  def typeTree(tree: Tree, expectedType: Option[Type], module: Module): Either[String, TypedTree]
}
