package evolution.primitive.algebra.constants.interpreter

import evolution.geometry.Point
import evolution.data.Result
import evolution.data.Result._
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

object ConstantsEvaluator extends Constants[Result] {
  override def double(d: Double): Result[Double] =
    Constant(d)
  override def point(evalX: Result[Double], evalY: Result[Double]): Result[Point] =
    (evalX, evalY) match {
      case (Constant(x, _), Constant(y, _)) => Constant(Point(x, y), s"constant-point($evalX, $evalY)")
      case _ =>
        Value(ctx => Point(evalX.evaluateWith(ctx), evalY.evaluateWith(ctx)), s"non-constant-point($evalX, $evalY)")
    }

  override def add[T: VectorSpace](a: Result[T], b: Result[T]): Result[T] =
    Value(ctx => VectorSpace[T].monoid.combine(a.evaluateWith(ctx), b.evaluateWith(ctx)), s"add($a, $b)")
  override def sin(d: Result[Double]): Result[Double] =
    Value(ctx => Math.sin(d.evaluateWith(ctx)))
  override def cos(d: Result[Double]): Result[Double] =
    Value(ctx => Math.cos(d.evaluateWith(ctx)))
  override def multiply[T: VectorSpace](k: Result[Double], t: Result[T]): Result[T] =
    Value(ctx => VectorSpace[T].mult(k.evaluateWith(ctx), t.evaluateWith(ctx)))
}
