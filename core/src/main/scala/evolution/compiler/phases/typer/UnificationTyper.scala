package evolution.compiler.phases.typer

import cats.implicits._
import evolution.compiler.phases.Typer
import evolution.compiler.module.Module
import evolution.compiler.tree._
import evolution.compiler.types._
import evolution.logging.Logger
import evolution.compiler.phases.typer.model.Constraints
import evolution.compiler.phases.typer.config.TypingConfig

final class UnificationTyper(logger: Logger) extends Typer {
  import logger.log
  def typeTree(
    tree: Tree,
    expectedType: Option[Type],
    module: Module
  ): Either[String, TypedTree] = for {
    treeWithTypeVars <- AssignFreshTypeVars.assign(tree, module.typeBindings).asRight
    _ = log(s"Un-typed expression:")
    _ = log(PrettyPrintTypedTree(treeWithTypeVars))
    constraints <- FindConstraints.find(treeWithTypeVars)
    _ = log(constraints)
    _ = log("Done: Constraints generation")
    expectedTypeConstraint = expectedType
      .map(tp => Constraints(tp -> treeWithTypeVars.annotation.value))
      .getOrElse(Constraints.empty)
    constraintsWithExpectedType = constraints.merge(expectedTypeConstraint)
    unification <- UnifyTypes.unify(constraintsWithExpectedType)
    _ = log("Done: unification")
    _ = log(s"Partially typed AST:")
    _ = log(PrettyPrintTypedTree(unification.substitution.substitute(treeWithTypeVars)))
    start = System.currentTimeMillis()
    predicateSubst <- new UnifyPredicates(logger)
      .unify(TypingConfig.instancesPredicates, unification.substitutedPredicates)
    stop = System.currentTimeMillis()
    _ = log(s"Predicate unification time: ${(stop - start)}")
    subst = predicateSubst.compose(unification.substitution)
    typedTree = subst.substitute(treeWithTypeVars)
    _ = log("Done: substitution")
    _ = log(s"Typed expression:")
    _ = log(PrettyPrintTypedTree(typedTree))
  } yield typedTree
}
