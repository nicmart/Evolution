package evolution.app.model
import evolution.app.model.context.DrawingContext
import evolution.geometry.Point
import evolution.materialization.Evolution

trait CodeCompiler {
  def compile(code: String, seed: Long, ctx: DrawingContext): Either[String, Evolution[Point]]
}
