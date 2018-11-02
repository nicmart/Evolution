package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.binding.interpreter.EvaluationResult._
import evolution.primitive.algebra.constants.Constants
import cats.syntax.semigroup._

object ConstantsEvaluator extends Constants[EvaluationResult, Double] {
  override def double(d: Double): EvaluationResult[Double] = Value(_ => d)
  override def point(x: EvaluationResult[Double], y: EvaluationResult[Double]): EvaluationResult[Point] =
    Value(ctx => Point(x.get(ctx), y.get(ctx)))
  override def add[T: Semigroup](a: EvaluationResult[T], b: EvaluationResult[T]): EvaluationResult[T] =
    Value(ctx => a.get(ctx) |+| b.get(ctx))
}
