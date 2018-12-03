package evolution.primitive.algebra.constants.interpreter

import evolution.geometry.Point
import evolution.data.Evaluation
import evolution.data.Evaluation._
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

object ConstantsEvaluator extends Constants[Evaluation] {
  override def double(d: Double): Evaluation[Double] =
    Constant(d)
  override def point(evalX: Evaluation[Double], evalY: Evaluation[Double]): Evaluation[Point] =
    (evalX, evalY) match {
      case (Constant(x, _), Constant(y, _)) => Constant(Point(x, y), s"constant-point($evalX, $evalY)")
      case _ =>
        Value(ctx => Point(evalX.evaluateWith(ctx), evalY.evaluateWith(ctx)), s"non-constant-point($evalX, $evalY)")
    }

  override def add[T: VectorSpace](a: Evaluation[T], b: Evaluation[T]): Evaluation[T] =
    Value(ctx => VectorSpace[T].monoid.combine(a.evaluateWith(ctx), b.evaluateWith(ctx)), s"add($a, $b)")
  override def sin(d: Evaluation[Double]): Evaluation[Double] =
    Value(ctx => Math.sin(d.evaluateWith(ctx)))
  override def cos(d: Evaluation[Double]): Evaluation[Double] =
    Value(ctx => Math.cos(d.evaluateWith(ctx)))
  override def multiply[T: VectorSpace](k: Evaluation[Double], t: Evaluation[T]): Evaluation[T] =
    Value(ctx => VectorSpace[T].mult(k.evaluateWith(ctx), t.evaluateWith(ctx)))
}
