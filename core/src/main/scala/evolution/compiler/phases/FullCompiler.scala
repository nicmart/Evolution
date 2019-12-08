package evolution.compiler.phases

import cats.syntax.either._
import evolution.compiler.expression.Expr
import evolution.compiler.term.{Module, TermInterpreter, TreeToTermCompiler}
import evolution.compiler.tree._
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.logging.Logger
import evolution.materialization.Evolution

final class FullCompiler(
    parser: Parser,
    typer: Typer,
    compiler: TreeToTermCompiler,
    interpreter: TermInterpreter,
    logger: Logger
) {
  import logger.log

  def compile(serialisedExpr: String, expectedType: Type, module: Module): Either[String, Any] =
    for {
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      typedTree <- typer.typeTree(untypedTree, Some(expectedType), module.assumptions)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
      term <- compiler.compile(typedTree)
      termWithModule = module.load(term)
      _ = log(s"Compiled to $termWithModule")
      _ = log("Done: compilation")
    } yield interpreter.interpret(termWithModule)
}
