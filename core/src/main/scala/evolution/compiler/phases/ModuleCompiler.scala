package evolution.compiler.phases

import evolution.compiler.phases.compiling._

import evolution.logging.Logger
import evolution.compiler.tree.PrettyPrintTypedTree
import evolution.compiler.expression.Expr
import evolution.compiler.module.Module
import evolution.compiler.tree._
import evolution.compiler.types.TypeBindings
import evolution.compiler.tree.TreeF.Let

final class ModuleCompiler(typedTreeCompiler: TypedTreeCompiler, logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(serialisedExpr: String, initialModule: Module): Either[String, Module] =
    for {
      typedTree <- typedTreeCompiler.compile(serialisedExpr, None, initialModule)
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
