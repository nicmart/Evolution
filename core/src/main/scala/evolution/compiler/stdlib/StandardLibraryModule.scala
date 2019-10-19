package evolution.compiler.stdlib

import evolution.compiler.phases.ModuleCompiler
import evolution.logging.NoOpLogger
import evolution.compiler.module.Module
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.TypeT
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.parsing.FastParseParser
import evolution.compiler.phases.typer.UnificationTyper
import evolution.compiler.phases.compiling.DefaultCompiler

object StandardLibraryModule {
  val module: Either[String, Module] = moduleCompiler.compile(code, initialModule)

  private lazy val initialModule = Module(
    TypingConfig.constantQualifiedTypes
      .withVarBinding("top", Qualified(TypeT.Double))
      .withVarBinding("bottom", Qualified(TypeT.Double))
      .withVarBinding("left", Qualified(TypeT.Double))
      .withVarBinding("right", Qualified(TypeT.Double)),
    identity
  )

  private lazy val moduleCompiler =
    new ModuleCompiler(FastParseParser, new UnificationTyper(NoOpLogger), DefaultCompiler, NoOpLogger)

  private lazy val code = StdLib.code
}
