package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.binding.interpreter.EvaluationResult._
import evolution.primitive.algebra.constants.Constants
import cats.syntax.semigroup._
import evolution.typeclass.VectorSpace

object ConstantsEvaluator extends Constants[EvaluationResult] {
  override def double(d: Double): EvaluationResult[Double] =
    Constant(d)
  override def point(evalX: EvaluationResult[Double], evalY: EvaluationResult[Double]): EvaluationResult[Point] =
    (evalX, evalY) match {
      case (Constant(x, _), Constant(y, _)) => Constant(Point(x, y), s"constant-point($evalX, $evalY)")
      case _                                => Value(ctx => Point(evalX.get(ctx), evalY.get(ctx)), s"non-constant-point($evalX, $evalY)")
    }

  override def add[T: VectorSpace](a: EvaluationResult[T], b: EvaluationResult[T]): EvaluationResult[T] =
    Value(ctx => VectorSpace[T].monoid.combine(a.get(ctx), b.get(ctx)), s"add($a, $b)")
  override def sin(d: EvaluationResult[Double]): EvaluationResult[Double] =
    Value(ctx => Math.sin(d.get(ctx)))
  override def cos(d: EvaluationResult[Double]): EvaluationResult[Double] =
    Value(ctx => Math.cos(d.get(ctx)))
  override def multiply[T: VectorSpace](k: EvaluationResult[Double], t: EvaluationResult[T]): EvaluationResult[T] =
    Value(ctx => VectorSpace[T].mult(k.get(ctx), t.get(ctx)))
}
