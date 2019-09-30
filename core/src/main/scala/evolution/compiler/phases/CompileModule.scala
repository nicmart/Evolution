package evolution.compiler.phases

import cats.implicits._
import evolution.compiler.phases.parsing.Parser
import evolution.compiler.phases.typing._
import evolution.compiler.phases.compiling._
import evolution.compiler.phases.typing.config.TypingConfig

import evolution.logging.Logger
import evolution.compiler.tree.PrettyPrintTypedTree
import evolution.compiler.expression.Expr
import evolution.compiler.module.Module
import evolution.compiler.phases.checkvars.CheckVars
import evolution.compiler.tree._
import evolution.compiler.types.TypeBindings
import evolution.compiler.tree.TreeF.Let

final class CompileModule(logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(serialisedExpr: String, initialModule: Module): Either[String, Module] =
    for {
      tree <- Parser.parse(serialisedExpr).leftMap(_.message)
      _ = log("Done: Parsing of AST")
      treeWithTypeVars <- AssignFreshTypeVars.assign(tree, initialModule.typeBindings).asRight
      _ = log(s"Un-typed expression:")
      _ = log(PrettyPrintTypedTree(treeWithTypeVars))
      constraints <- FindConstraints.find(treeWithTypeVars)
      _ = log("Done: Constraints generation")
      unification <- UnifyTypes.unify(constraints)
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
      typeBindings = extractTypeBindings(typedTree, initialModule.typeBindings)
      expression <- Compile.compile(typedTree, initialModule.varContext)
      expressionWithModule = initialModule.load(expression)
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
      loadModule = replaceVarExpr("export", expressionWithModule) _
    } yield Module(typeBindings, loadModule)

  // 1. Find type bindings
  private def extractTypeBindings(typedTree: TypedTree, currentBindings: TypeBindings): TypeBindings =
    typedTree.tree match {
      case Let(varName, expr, in) =>
        extractTypeBindings(expr, currentBindings.withVarBinding(varName, expr.annotation))
      case _ => currentBindings
    }

  // 2. Build load function
  private def replaceVarExpr(varName: String, expr: Expr[Any])(replaceWith: Expr[Any]): Expr[Any] = expr match {
    case Expr.Let(variable, value, body)   => Expr.Let(variable, value, replaceVarExpr(varName, body)(replaceWith))
    case Expr.Var(name) if name == varName => replaceWith
    case _                                 => expr
  }
}
