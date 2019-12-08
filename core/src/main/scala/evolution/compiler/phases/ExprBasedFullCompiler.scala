package evolution.compiler.phases

import cats.syntax.either._
import evolution.compiler.expression.Expr
import evolution.compiler.tree._
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.logging.Logger
import evolution.materialization.Evolution

final class ExprBasedFullCompiler(
    parser: Parser,
    typer: Typer,
    compiler: Compiler,
    materializer: Materializer,
    logger: Logger
) {
  import logger.log

  def compile(serialisedExpr: String, expectedType: Type, module: ExprModule): Either[String, Any] =
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
