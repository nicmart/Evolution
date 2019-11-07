package evolution.compiler.phases

import cats.syntax.either._
import evolution.logging.Logger
import evolution.compiler.tree.PrettyPrintTypedTree
import evolution.compiler.expression.Expr
import evolution.compiler.module.Module
import evolution.compiler.tree._
import evolution.compiler.types.Assumptions
import evolution.compiler.tree.TreeF.Let
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.Assumption

final class ModuleCompiler(parser: Parser, typer: Typer, compiler: Compiler, logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(serialisedExpr: String, initialModule: Module): Either[String, Module] =
    for {
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      typedTree <- typer.typeTree(untypedTree, None, initialModule)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
      assumptions = extractAssumptions(typedTree, initialModule.assumptions)
      _ = log(s"Assumptions extracted")
      _ = log(assumptions.all)
      expression <- compiler.compile(typedTree, initialModule)
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
      loadModule = replaceVarExpr("export", expression) _
    } yield Module(assumptions, loadModule)

  // 1. Find assumptions
  private def extractAssumptions(typedTree: TypedTree, currentAssumptions: Assumptions): Assumptions =
    typedTree.tree match {
      case Let(varName, expr, in) =>
        val qualifiedScheme = Qualified(expr.annotation.predicates, Scheme(expr.annotation.value))
        extractAssumptions(in, currentAssumptions.withAssumption(Assumption(varName, qualifiedScheme, false)))
      case _ => currentAssumptions
    }

  // 2. Build load function
  private def replaceVarExpr(varName: String, expr: Expr[Any])(replaceWith: Expr[Any]): Expr[Any] = expr match {
    case Expr.Let(variable, value, body)   => Expr.Let(variable, value, replaceVarExpr(varName, body)(replaceWith))
    case Expr.Var(name) if name == varName => replaceWith
    case _                                 => expr
  }
}
