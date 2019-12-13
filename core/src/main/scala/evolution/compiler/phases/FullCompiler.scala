package evolution.compiler.phases

import cats.syntax.either._
import evolution.compiler.term.{Module, TermInterpreter, TermOptimizer, TreeToTermCompiler, UniqueIdRenamer}
import evolution.compiler.tree._
import evolution.compiler.types.Type
import evolution.logging.Logger
import pprint.PPrinter

final class FullCompiler(
    parser: Parser,
    typer: Typer,
    compiler: TreeToTermCompiler,
    interpreter: TermInterpreter,
    logger: Logger
) {
  import logger.log
  private val optimizer = new TermOptimizer(interpreter)

  def compile(serialisedExpr: String, expectedType: Type, module: Module): Either[String, Any] =
    for {
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      typedTree <- typer.typeTree(untypedTree, Some(expectedType), module.assumptions)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
//      _ = println(PrettyPrintTree(untypedTree))
//      _ = println(PrettyPrintTypedTree(typedTree))
      term <- printTime("treeToTerm", compiler.compile(typedTree))
      //_ = PPrinter.BlackWhite.pprintln(term, height = Int.MaxValue)
      termWithModule = module.load(term)
      termWithUniqueNames = printTime("term renaming", renamer.rename(termWithModule))
      //_ = PPrinter.BlackWhite.pprintln(termWithUniqueNames, height = Int.MaxValue, indent = 0)
      optimizedTerm = printTime("optimization", optimizer.optimize(termWithUniqueNames))
      //_ = PPrinter.BlackWhite.pprintln(optimizedTerm, height = Int.MaxValue, indent = 0)
      _ = log(s"Compiled to $termWithModule")
      _ = log("Done: compilation")
    } yield printTime("interpretation", interpreter.interpret(optimizedTerm))

  private def printTime[T](label: String, t: => T): T = {
    val start = System.currentTimeMillis()
    val result = t
    val end = System.currentTimeMillis()
    println(s"$label: ${end - start}ms")
    result
  }

  private val renamer = new UniqueIdRenamer
}
