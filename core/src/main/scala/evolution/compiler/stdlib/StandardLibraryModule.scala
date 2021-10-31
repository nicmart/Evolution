package evolution.compiler.stdlib

import evolution.compiler.phases.ModuleCompiler
import evolution.compiler.phases.parser.CatsParseParser
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.typer.predicates.UnifyPredicates
import evolution.compiler.phases.typer.{PredicatesSolverTyper, RecursiveTyper}
import evolution.compiler.term.{Definition, Module, TreeToTermCompiler}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.NoOpLogger

object StandardLibraryModule:
  val module: Either[String, Module] = moduleCompiler.compile(code, initialModule)

  // TODO: we need to find a solution to this, here we do not have the values yet
  private lazy val borderVarsModule = Module(
    List(
      Definition("top", None, Qualified(Scheme(Type.Double))),
      Definition("bottom", None, Qualified(Scheme(Type.Double))),
      Definition("left", None, Qualified(Scheme(Type.Double))),
      Definition("right", None, Qualified(Scheme(Type.Double)))
    )
  )

  private lazy val initialModule =
    TypingConfig.constantsModule.compose(borderVarsModule)

  private lazy val typer = PredicatesSolverTyper(RecursiveTyper(), UnifyPredicates(NoOpLogger))

  private lazy val moduleCompiler =
    ModuleCompiler(CatsParseParser, typer, TreeToTermCompiler(), NoOpLogger)

  private lazy val code = StdLib.code
