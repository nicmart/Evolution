package evolution.compiler.stdlib

import evolution.compiler.phases.{ExprModule, ExprModuleCompiler}
import evolution.logging.NoOpLogger
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.compiler.DefaultCompiler
import evolution.compiler.phases.typer.{PredicatesSolverTyper, RecursiveTyper, model}
import evolution.compiler.phases.typer.model.Assumption
import evolution.compiler.phases.typer.predicates.UnifyPredicates

object StandardLibraryExprModule {
  val module: Either[String, ExprModule] = moduleCompiler.compile(code, initialModule)

  private lazy val initialModule = ExprModule(
    TypingConfig.constantQualifiedTypes
      .withAssumption(model.Assumption("top", Qualified(Scheme(Type.Double)), false))
      .withAssumption(model.Assumption("bottom", Qualified(Scheme(Type.Double)), false))
      .withAssumption(model.Assumption("left", Qualified(Scheme(Type.Double)), false))
      .withAssumption(model.Assumption("right", Qualified(Scheme(Type.Double)), false)),
    identity
  )

  //private lazy val typer = new RecursiveTyper
  private lazy val typer = new PredicatesSolverTyper(new RecursiveTyper, new UnifyPredicates(NoOpLogger))

  private lazy val moduleCompiler =
    new ExprModuleCompiler(FastParseParser, typer, DefaultCompiler, NoOpLogger)

  private lazy val code = StdLib.code
}
