package evolution.compiler.stdlib

import evolution.compiler.phases.ModuleCompiler
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.typer.predicates.UnifyPredicates
import evolution.compiler.phases.typer.{PredicatesSolverTyper, RecursiveTyper}
import evolution.compiler.term.{Definition, Module, Term, TreeToTermCompiler}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.NoOpLogger

object StandardLibraryModule {
  val module: Either[String, Module] = moduleCompiler.compile(code, initialModule)

  private lazy val borderVarsModule = Module(
    List(
      Definition("top", Term.Value(100), Qualified(Scheme(Type.Double))),
      Definition("bottom", Term.Value(100), Qualified(Scheme(Type.Double))),
      Definition("left", Term.Value(100), Qualified(Scheme(Type.Double))),
      Definition("right", Term.Value(100), Qualified(Scheme(Type.Double)))
    )
  )

  private lazy val initialModule =
    TypingConfig.constantsModule.compose(borderVarsModule)

  //private lazy val typer = new RecursiveTyper
  private lazy val typer = new PredicatesSolverTyper(new RecursiveTyper, new UnifyPredicates(NoOpLogger))

  private lazy val moduleCompiler =
    new ModuleCompiler(FastParseParser, typer, new TreeToTermCompiler, NoOpLogger)

  private lazy val code = StdLib.code
}
