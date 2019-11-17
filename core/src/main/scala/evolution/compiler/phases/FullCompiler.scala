package evolution.compiler.phases

import cats.syntax.either._
import evolution.compiler.expression.Expr
import evolution.compiler.tree._
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.logging.Logger
import evolution.materialization.Evolution

final class FullCompiler(parser: Parser, typer: Typer, compiler: Compiler, materializer: Materializer, logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(serialisedExpr: String, expectedType: Type, module: Module): Either[String, Long => Iterator[Point]] =
    for {
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      typedTree <- typer.typeTree(untypedTree, Some(expectedType), module.assumptions)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
      expression <- compiler.compile(typedTree, module)
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
    } yield materializer.materialize(expression.asInstanceOf[Expr[Evolution[Point]]])
}
