package evolution.compiler.phases

import cats.implicits._
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing._
import evolution.compiler.phases.compiling._
import evolution.compiler.phases.typing.model.Constraints
import evolution.compiler.phases.compiling.model.VarContext
import evolution.compiler.types.Type
import evolution.compiler.phases.typing.config.TypingConfig

import scala.collection.immutable.Nil
import evolution.materialization.Evolution
import evolution.geometry.Point
import evolution.compiler.tree.Tree
import evolution.logging.Logger
import evolution.compiler.tree.TreeF._
import evolution.compiler.tree.PrettyPrintTypedTree
import evolution.compiler.phases.materializing.Materializer
import evolution.compiler.expression.Expr
import evolution.compiler.module.Module
import evolution.compiler.phases.checkvars.CheckVars

class AllPhases(materializer: Materializer, logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(
    serialisedExpr: String,
    expectedType: Type,
    module: Module,
    varBindings: List[(String, Tree)]
  ): Either[String, Long => Iterator[Point]] =
    for {
      tree <- Parser.parse(serialisedExpr).leftMap(_.message)
      _ = log("Done: Parsing of AST")
      treeWithVars = addVars(tree, varBindings)
      allTypeBindings = module.typeBindings
      treeWithTypeVars <- AssignFreshTypeVars.assign(treeWithVars, allTypeBindings).asRight
      _ = log(s"Un-typed expression:")
      _ = log(PrettyPrintTypedTree(treeWithTypeVars))
      constraints <- FindConstraints.find(treeWithTypeVars)
      _ = log("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Constraints(expectedType -> treeWithTypeVars.annotation.value))
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
      expression <- Compile.compile(typedTree, varContext(varBindings))
      expressionWithModule = module.load(expression)
      _ <- CheckVars(expressionWithModule, varContext(varBindings))
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
      // TODO here we do not need to know about the existence of a varcontext
    } yield materializer.materialize(expressionWithModule.asInstanceOf[Expr[Evolution[Point]]])

  private def varContext(varBindings: List[(String, Tree)]): VarContext =
    new VarContext(varBindings.map(_._1))

  private def addVars(tree: Tree, varBindings: List[(String, Tree)]): Tree = varBindings match {
    case (name, value) :: tl => Let(name, value, addVars(tree, tl)).embed
    case Nil                 => tree
  }
}
