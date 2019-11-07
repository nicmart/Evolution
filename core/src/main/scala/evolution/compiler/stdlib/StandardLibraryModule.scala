package evolution.compiler.stdlib

import evolution.compiler.phases.ModuleCompiler
import evolution.logging.NoOpLogger
import evolution.compiler.module.Module
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.UnificationTyper
import evolution.compiler.phases.compiler.DefaultCompiler
import evolution.compiler.types.Assumption

object StandardLibraryModule {
  val module: Either[String, Module] = moduleCompiler.compile(code, initialModule)

  private lazy val initialModule = Module(
    TypingConfig.constantQualifiedTypes
      .withAssumption(Assumption("top", Qualified(Scheme(Type.Double)), false))
      .withAssumption(Assumption("bottom", Qualified(Scheme(Type.Double)), false))
      .withAssumption(Assumption("left", Qualified(Scheme(Type.Double)), false))
      .withAssumption(Assumption("right", Qualified(Scheme(Type.Double)), false)),
    identity
  )

  private lazy val moduleCompiler =
    new ModuleCompiler(FastParseParser, new UnificationTyper(NoOpLogger), DefaultCompiler, NoOpLogger)

  private lazy val code = StdLib.code
}
