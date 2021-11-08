package evolution.compiler.phases

import cats.syntax.either.*
import evolution.compiler.term.*
import evolution.compiler.tree.*
import evolution.compiler.types.Type
import evolution.logging.Logger

final class FullCompiler(
    parser: Parser,
    typer: Typer,
    compiler: TreeToTermCompiler,
    interpreter: TermInterpreter,
    logger: Logger
):
  import logger.log
  private val optimizer = TermOptimizer(interpreter)

  def compile(serialisedExpr: String, expectedType: Type, module: Module): Either[String, Any] =
    for
      untypedTree <- parser.parse(serialisedExpr).leftMap(_.message)
      typedTree <- printTime("typing", typer.typeTree(untypedTree, Some(expectedType), module.assumptions))
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
//      _ = println(PrettyPrintTree(untypedTree))
//      _ = println(PrettyPrintTypedTree(typedTree))
      term <- printTime("treeToTerm", compiler.compile(typedTree))
      //_ = PPrinter.BlackWhite.pprintln(term, height = Int.MaxValue)
      optimizedTerm = printTime("optimization", optimizer.optimize(term, module.terms))
//      optimizedTerm = term
//      _ = PPrinter.BlackWhite.pprintln(optimizedTerm, height = Int.MaxValue)
      termWithModule = printTime("module load", module.load(optimizedTerm))
      //_ = PPrinter.BlackWhite.pprintln(optimizedTerm, height = Int.MaxValue)
      _ = log(s"Compiled to $termWithModule")
      _ = log("Done: compilation")
    yield printTime("interpretation", interpreter.interpret(termWithModule))

  private def printTime[T](label: String, t: => T): T =
    val start = System.currentTimeMillis()
    val result = t
    val end = System.currentTimeMillis()
    println(s"$label: ${end - start}ms")
    result
