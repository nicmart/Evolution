package evolution.compiler.phases

import evolution.compiler.phases.typer.model.Assumptions
import evolution.compiler.tree.*
import evolution.compiler.types.*

trait Typer:
  /**
   * Type a Tree, given an optional expected type and a set of assumptions
   */
  def typeTree(tree: Tree, expectedType: Option[Type], assumptions: Assumptions): Either[String, TypedTree]
