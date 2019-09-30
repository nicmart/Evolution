package evolution.compiler.stdlib
import evolution.compiler.phases.CompileModule
import evolution.logging.NoOpLogger
import evolution.compiler.module.Module
import evolution.compiler.types.TypeClasses.Qualified
import evolution.compiler.types.TypeT
import evolution.compiler.phases.typing.config.TypingConfig

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

  private lazy val moduleCompiler = new CompileModule(NoOpLogger)

  private lazy val code = s"""

        grid(gridSize) = product(
            y <- range(top, bottom, -gridSize),
            x <- range(left, right, gridSize)
        ) in point(x, y)
        
        in export

    """
}
