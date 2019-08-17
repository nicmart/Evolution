package evolution.app.model

import evolution.geometry.Point
import evolution.compiler.phases.AllPhases
import evolution.compiler.types.Type
import evolution.compiler.phases.typing.config.TypingConfig
import evolution.app.model.context.DrawingContext
import evolution.compiler.ast.AST
import evolution.materialization.Evolution
import evolution.logging.NoOpLogger

object CodeCompiler {
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Iterator[Point]] =
    new AllPhases(NoOpLogger)
      .compile(
        code,
        Type.Evo(Type.Point),
        TypingConfig.constantQualifiedTypes,
        varBindings(ctx)
      )
      .map { evolution =>
        Evolution.setSeed(seed)
        evolution.run
      }

  private def varBindings(ctx: DrawingContext): List[(String, AST)] = List(
    "top" -> AST.DoubleLiteral(ctx.top),
    "bottom" -> AST.DoubleLiteral(ctx.bottom),
    "left" -> AST.DoubleLiteral(ctx.left),
    "right" -> AST.DoubleLiteral(ctx.right)
  )
}
