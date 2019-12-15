package evolution.compiler.phases

import cats.syntax.either._
import evolution.compiler.phases.typer.model
import evolution.compiler.phases.typer.model.{Assumption, Assumptions}
import evolution.compiler.term.{
  Module,
  RegisterBasedInterpreter,
  Term,
  TermInterpreter,
  TermOptimizer,
  TreeToTermCompiler
}
import evolution.compiler.tree.TreeF.Let
import evolution.compiler.tree.{PrettyPrintTypedTree, _}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.Logger

final class ModuleCompiler(parser: Parser, typer: Typer, compiler: TreeToTermCompiler, logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(serialisedExpr: String, initialModule: Module): Either[String, Module] =
    for {
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      exportAssumption = Assumption("export", Qualified(Scheme(Type.Var("X"))), primitive = false) // TODO think more about this
      typedTree <- typer.typeTree(untypedTree, None, initialModule.assumptions.withAssumption(exportAssumption))
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
      assumptions = extractAssumptions(typedTree, initialModule.assumptions)
      _ = log(s"Assumptions extracted")
      _ = log(assumptions.all)
      termWithoutModule <- compiler.compile(typedTree)
      //term = optimizer.optimize(initialModule.load(termWithoutModule))
      term = initialModule.load(termWithoutModule)
      _ = log(s"Compiled to $term")
      _ = log("Done: compilation")
    } yield Module(assumptions, replaceVarInTerm("export", term))

  // 1. Find assumptions
  @scala.annotation.tailrec
  private def extractAssumptions(typedTree: TypedTree, currentAssumptions: Assumptions): Assumptions =
    typedTree.tree match {
      case Let(varName, expr, in) =>
        val qualifiedScheme = Qualified(expr.annotation.predicates, Scheme(expr.annotation.value))
        extractAssumptions(
          in,
          currentAssumptions.withAssumption(model.Assumption(varName, qualifiedScheme, primitive = false))
        )
      case _ => currentAssumptions
    }

  // 2. Build load function
  private def replaceVarInTerm(varName: String, term: Term)(replaceWith: Term): Term = term match {
    case Term.Let(variable, value, body)  => Term.Let(variable, value, replaceVarInTerm(varName, body)(replaceWith))
    case Term.Id(name) if name == varName => replaceWith
    case _                                => term
  }
}
