package evolution.app.model

import evolution.geometry.Point
import evolution.compiler.phases.AllPhases
import evolution.compiler.types.TypeT
import evolution.compiler.phases.typing.config.TypingConfig
import evolution.app.model.context.DrawingContext
import evolution.compiler.tree.TreeF
import evolution.compiler.tree.Tree
import evolution.compiler.module.Module

class CodeCompiler(allPhases: AllPhases) {
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Iterator[Point]] =
    allPhases
      .compile(
        code,
        TypeT.Evo(TypeT.Point),
        Module.empty,
        TypingConfig.constantQualifiedTypes,
        varBindings(ctx)
      )
      .map { evolution =>
        evolution(seed)
      }

  private def varBindings(ctx: DrawingContext): List[(String, Tree)] = List(
    "top" -> TreeF.DoubleLiteral(ctx.top).embed,
    "bottom" -> TreeF.DoubleLiteral(ctx.bottom).embed,
    "left" -> TreeF.DoubleLiteral(ctx.left).embed,
    "right" -> TreeF.DoubleLiteral(ctx.right).embed
  )
}
