package evolution.app.model
import evolution.app.model.context.DrawingContext
import evolution.geometry.Point

trait CodeCompiler:
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Iterator[Point]]
