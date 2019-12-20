package evolution.app.model

import evolution.app.model.context.DrawingContext
import evolution.compiler.phases.FullCompiler
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.stdlib.StandardLibraryModule
import evolution.compiler.term.Module
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

final class TermBasedCodeCompiler(fullCompiler: FullCompiler) extends CodeCompiler {
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Iterator[Point]] =
    module(ctx).flatMap(
      mod =>
        fullCompiler
          .compile(
            code,
            Type.Evo(Type.Point),
            mod
          )
          .map { evolution =>
            Evolution.runWithSeed(seed, evolution.asInstanceOf[Evolution[Point]])
          }
    )

  private def module(ctx: DrawingContext): Either[String, Module] = {
    val constantsModule = TypingConfig.constantsModule
    val drawingContextModule = DrawingContextModule(ctx)

    StandardLibraryModule.module.map { stdModule =>
      constantsModule.compose(drawingContextModule).compose(stdModule)
    }
  }
}
