package evolution.compiler.phases

import cats.syntax.either._
import evolution.compiler.phases.typer.model
import evolution.compiler.phases.typer.model.{Assumption, Assumptions}
import evolution.compiler.term.{Definition, Module, RegisterBasedInterpreter, Term, TermOptimizer, TreeToTermCompiler}
import evolution.compiler.tree.TreeF.Let
import evolution.compiler.tree.{PrettyPrintTypedTree, _}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.Logger
import scala.annotation.tailrec

final class ModuleCompiler(parser: Parser, typer: Typer, compiler: TreeToTermCompiler, logger: Logger) {
  import logger.log

  private val optimizer: TermOptimizer = new TermOptimizer(new RegisterBasedInterpreter)

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(serialisedExpr: String, initialModule: Module): Either[String, Module] =
    for {
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      exportAssumption = Assumption("export", Qualified(Scheme(Type.Var("X")))) // TODO think more about this
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
      defs = extractDefs(assumptions, term, initialModule.definitions.toVector)
      optimizedDefs = optimizeDefinitions(defs)
      _ = log(s"Compiled to $term")
      _ = log("Done: compilation")
    } yield Module(optimizedDefs.toList)

  // 1. Find assumptions
  @tailrec
  private def extractAssumptions(typedTree: TypedTree, currentAssumptions: Assumptions): Assumptions =
    typedTree.tree match {
      case Let(varName, expr, in) =>
        val qualifiedScheme = Qualified(expr.annotation.predicates, Scheme(expr.annotation.value))
        extractAssumptions(
          in,
          currentAssumptions.withAssumption(model.Assumption(varName, qualifiedScheme))
        )
      case _ => currentAssumptions
    }

  // 2. Extract definitions
  @tailrec
  private def extractDefs(
      assumptions: Assumptions,
      term: Term,
      currentDefinitions: Vector[Definition]
  ): Vector[Definition] =
    term match {
      case Term.Let(varName, expr, in) =>
        val definition = Definition(varName, expr, assumptions.get(varName).get.qualifiedScheme)
        extractDefs(
          assumptions,
          in,
          currentDefinitions.appended(definition)
        )
      case _ => currentDefinitions
    }

  private def optimizeDefinitions(definitions: Vector[Definition]): Vector[Definition] =
    optimizeDefinitionsRec(definitions, Vector.empty)

  @tailrec
  private def optimizeDefinitionsRec(
      toOptimize: Vector[Definition],
      optimized: Vector[Definition]
  ): Vector[Definition] =
    toOptimize.headOption match {
      case Some(definition) =>
        val defMap: Map[String, Term] = Map.from(optimized.map(definition => definition.name -> definition.term))
        val optimizedDef = Definition(definition.name, optimizer.optimize(definition.term, defMap), definition.tpe)
        optimizeDefinitionsRec(toOptimize.drop(1), optimized.appended(optimizedDef))
      case None => optimized
    }

}
