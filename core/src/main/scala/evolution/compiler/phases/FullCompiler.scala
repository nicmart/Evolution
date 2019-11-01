package evolution.compiler.phases

import cats.syntax.either._
import evolution.compiler.types.Type
import evolution.materialization.Evolution
import evolution.geometry.Point
import evolution.logging.Logger
import evolution.compiler.tree._
import evolution.compiler.phases.Materializer
import evolution.compiler.expression.Expr
import evolution.compiler.module.Module
import evolution.compiler.phases.checkvars.CheckVars

final class FullCompiler(parser: Parser, typer: Typer, compiler: Compiler, materializer: Materializer, logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(
    serialisedExpr: String,
    expectedType: Type,
    module: Module
  ): Either[String, Long => Iterator[Point]] =
    for {
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      typedTree <- typer.Typeree(untypedTree, Some(expectedType), module)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
      expression <- compiler.compile(typedTree, module)
      _ <- CheckVars(expression, module.varContext)
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
      // TODO here we do not need to know about the existence of a varcontext
    } yield materializer.materialize(expression.asInstanceOf[Expr[Evolution[Point]]])
}
