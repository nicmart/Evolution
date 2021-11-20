package evolution.compiler.phases.typer

import evolution.compiler.phases.Typer
import evolution.compiler.phases.typer.config.TypeclassConfig
import evolution.compiler.phases.typer.model.Assumptions
import evolution.compiler.phases.typer.predicates.UnifyPredicates
import evolution.compiler.tree.{Tree, TypedTree}
import evolution.compiler.types.Type

final class PredicatesSolverTyper(typer: Typer, solver: UnifyPredicates) extends Typer:
  override def typeTree(
      tree: Tree,
      expectedType: Option[Type],
      assumptions: Assumptions
  ): Either[String, TypedTree] =
    for
      typedTree <- typer.typeTree(tree, expectedType, assumptions)
      predicatesSubstitution <- solver.unify(TypeclassConfig.instancesPredicates, typedTree.annotation.predicates)
    yield predicatesSubstitution.substitute(typedTree)
