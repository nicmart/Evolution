package evolution.compiler.phases

import evolution.compiler.phases.compiling._
import evolution.compiler.types.Type
import evolution.materialization.Evolution
import evolution.geometry.Point
import evolution.logging.Logger
import evolution.compiler.tree._
import evolution.compiler.phases.materializing.Materializer
import evolution.compiler.expression.Expr
import evolution.compiler.module.Module
import evolution.compiler.phases.checkvars.CheckVars

final class AllPhases(typedTreeCompiler: TypedTreeCompiler, materializer: Materializer, logger: Logger) {
  import logger.log

  // TODO here we are assuming the the expected type can be anything, but that the output is Evolution[Point]???
  def compile(
    serialisedExpr: String,
    expectedType: Type,
    module: Module
  ): Either[String, Long => Iterator[Point]] =
    for {
      typedTree <- typedTreeCompiler.compile(serialisedExpr, Some(expectedType), module)
      _ = log("Done: substitution")
      _ = log(s"Typed expression:")
      _ = log(PrettyPrintTypedTree(typedTree))
      expression <- Compile.compile(typedTree, module.varContext)
      expressionWithModule = module.load(expression)
      _ <- CheckVars(expressionWithModule, module.varContext)
      _ = log(s"Compiled to $expression")
      _ = log("Done: compilation")
      // TODO here we do not need to know about the existence of a varcontext
    } yield materializer.materialize(expressionWithModule.asInstanceOf[Expr[Evolution[Point]]])
}
