package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.binding.interpreter.{EvaluationResult, Value}
import evolution.primitive.algebra.constants.ConstantsAlgebra
import cats.syntax.semigroup._

object ConstantsAlgebraEvaluator extends ConstantsAlgebra[EvaluationResult] {
  override def double(d: Double): EvaluationResult[Double] = Value(_ => d)
  override def point(x: Double, y: Double): EvaluationResult[Point] = Value(_ => Point(x, y))
  override def add[T: Semigroup](a: EvaluationResult[T], b: EvaluationResult[T]): EvaluationResult[T] =
    Value(ctx => a.get(ctx) |+| b.get(ctx))
}
