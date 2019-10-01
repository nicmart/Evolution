package evolution.compiler.phases

import cats.implicits._
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing._
import evolution.compiler.phases.typing.model.Constraints
import evolution.compiler.types.Type
import evolution.compiler.phases.typing.config.TypingConfig

import evolution.logging.Logger
import evolution.compiler.tree._
import evolution.compiler.module.Module

final class TypedTreeCompiler(logger: Logger) {
  import logger.log

  def compile(
    serialisedExpr: String,
    expectedType: Option[Type],
    module: Module
  ): Either[String, TypedTree] =
    for {
      tree <- Parser.parse(serialisedExpr).leftMap(_.message)
      _ = log("Done: Parsing of AST")
      treeWithTypeVars <- AssignFreshTypeVars.assign(tree, module.typeBindings).asRight
      _ = log(s"Un-typed expression:")
      _ = log(PrettyPrintTypedTree(treeWithTypeVars))
      constraints <- FindConstraints.find(treeWithTypeVars)
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
