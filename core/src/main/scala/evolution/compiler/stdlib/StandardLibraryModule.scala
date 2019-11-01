package evolution.compiler.stdlib

import evolution.compiler.phases.ModuleCompiler
import evolution.logging.NoOpLogger
import evolution.compiler.module.Module
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.Type
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.parser.FastParseParser
import evolution.compiler.phases.typer.UnificationTyper
import evolution.compiler.phases.compiler.DefaultCompiler

object StandardLibraryModule {
  val module: Either[String, Module] = moduleCompiler.compile(code, initialModule)

  private lazy val initialModule = Module(
    TypingConfig.constantQualifiedTypes
      .withVarBinding("top", Qualified(Type.Double))
      .withVarBinding("bottom", Qualified(Type.Double))
      .withVarBinding("left", Qualified(Type.Double))
      .withVarBinding("right", Qualified(Type.Double)),
    identity
  )

  private lazy val moduleCompiler =
    new ModuleCompiler(FastParseParser, new UnificationTyper(NoOpLogger), DefaultCompiler, NoOpLogger)

  private lazy val code = StdLib.code
}
