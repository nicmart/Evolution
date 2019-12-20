package evolution.compiler.stdlib

import evolution.compiler.phases.ModuleCompiler
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.phases.typer.model.{Assumption, Assumptions}
import evolution.compiler.phases.typer.predicates.UnifyPredicates
import evolution.compiler.phases.typer.{PredicatesSolverTyper, RecursiveTyper, model}
import evolution.compiler.term.{Module, TreeToTermCompiler}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.Qualified
import evolution.logging.NoOpLogger

object StandardLibraryModule {
  val module: Either[String, Module] = moduleCompiler.compile(code, initialModule)

  private lazy val initialModule = Module(
    Assumptions(ConstConfig.constants.map(const => Assumption(const.name, const.tpe)))
      .withAssumption(model.Assumption("top", Qualified(Scheme(Type.Double))))
      .withAssumption(model.Assumption("bottom", Qualified(Scheme(Type.Double))))
      .withAssumption(model.Assumption("left", Qualified(Scheme(Type.Double))))
      .withAssumption(model.Assumption("right", Qualified(Scheme(Type.Double)))),
    identity
  )

  //private lazy val typer = new RecursiveTyper
  private lazy val typer = new PredicatesSolverTyper(new RecursiveTyper, new UnifyPredicates(NoOpLogger))

  private lazy val moduleCompiler =
    new ModuleCompiler(FastParseParser, typer, new TreeToTermCompiler, NoOpLogger)

  private lazy val code = StdLib.code
}
