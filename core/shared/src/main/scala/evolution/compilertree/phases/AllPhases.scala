package evolution.compilertree.phases

import cats.implicits._
import evolution.compilertree.phases.parsing.Parser
import evolution.compilertree.phases.typing._
import evolution.compilertree.phases.compiling._
import evolution.compilertree.phases.typing.model.Constraints
import evolution.compilertree.phases.compiling.model.VarContext
import evolution.compilertree.types.Type
import evolution.compilertree.types.TypeBindings
import evolution.compilertree.phases.typing.config.TypingConfig
import scala.collection.immutable.Nil
import evolution.materialization.Evolution
import evolution.geometry.Point
import evolution.compilertree.phases.materializing.Materialize
import evolution.data.emptyCtx
import evolution.logging.Logger
import evolution.compilertree.ast.TreeF._
import evolution.compilertree.ast.TreeF

class AllPhases(logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(
    serialisedExpr: String,
    expectedType: Type,
    typeBindings: TypeBindings,
    varBindings: List[(String, Tree)]
  ): Either[String, Evolution[Point]] =
    for {
      ast <- Parser.parse(serialisedExpr).leftMap(_.message)
      _ = log("Done: Parsing of AST")
      astWithVars = addVars(ast, varBindings)
      treeWithTypeVars <- AssignFreshTypeVars.assign(astWithVars, typeBindings).asRight
      _ = log(s"Un-typed expression:")
      //_ = log(TreeF.prettyPrint(treeWithTypeVars))
      constraints <- FindConstraints.find(treeWithTypeVars)
      _ = log("Done: Constraints generation")
      constraintsWithExpectedType = constraints.merge(Constraints(expectedType -> treeWithTypeVars.value.value))
      unification <- UnifyTypes.unify(constraintsWithExpectedType)
      _ = log("Done: unification")
      _ = log(s"Partially typed AST:")
      //_ = log(TreeF.pprintTree(unification.substitution.substitute(treeWithTypeVars)))
      start = System.currentTimeMillis()
      predicateSubst <- new UnifyPredicates(logger).unify(TypingConfig.instances, unification.substitutedPredicates)
      stop = System.currentTimeMillis()
      _ = log(s"Predicate unification time: ${(stop - start)}")
      subst = predicateSubst.compose(unification.substitution)
      typedAst = subst.substitute(treeWithTypeVars)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      //_ = log(AST.prettyPrint(typedAst))
      expression <- Compile.compileTree(typedAst, varContext(varBindings))
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
      evolution = Materialize.materialize(expression).apply(emptyCtx)
      _ = log(s"Materialized to $evolution")
    } yield evolution.asInstanceOf[Evolution[Point]]

  private def varContext(varBindings: List[(String, Tree)]): VarContext =
    new VarContext(varBindings.map(_._1))

  private def addVars(tree: Tree, varBindings: List[(String, Tree)]): Tree = varBindings match {
    case (name, value) :: tl => Let(name, value, addVars(tree, tl)).embed
    case Nil                 => tree
  }
}