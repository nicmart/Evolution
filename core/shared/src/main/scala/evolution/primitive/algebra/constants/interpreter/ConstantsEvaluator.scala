package evolution.primitive.algebra.constants.interpreter

import cats.Group
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.data.Evaluation
import evolution.data.Evaluation._
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

object ConstantsEvaluator extends Constants[Evaluation] {
  override def int(n: Int): Evaluation[Int] =
    Constant(n)
  override def double(d: Double): Evaluation[Double] =
    Constant(d)
  override def floor(d: Evaluation[Double]): Evaluation[Int] =
    d.map(_.toInt)
  override def point(evalX: Evaluation[Double], evalY: Evaluation[Double]): Evaluation[Point] =
    Evaluation.map2(evalX, evalY) { (x, y) =>
      Point(x, y)
    }
  override def x(evalPoint: Evaluation[Point]): Evaluation[Double] =
    evalPoint.map(_.x)
  override def y(evalPoint: Evaluation[Point]): Evaluation[Double] =
    evalPoint.map(_.y)
  override def add[T: Semigroup](a: Evaluation[T], b: Evaluation[T]): Evaluation[T] =
    Evaluation.map2(a, b)(Semigroup[T].combine)
  override def div(a: Evaluation[Double], b: Evaluation[Double]): Evaluation[Double] =
    Evaluation.map2(a, b)(_ / _)
  override def exp(evalA: Evaluation[Double], evalB: Evaluation[Double]): Evaluation[Double] =
    Evaluation.map2(evalA, evalB) { (a, b) =>
      Math.pow(a, b)
    }
  override def inverse[T: Group](a: Evaluation[T]): Evaluation[T] =
    a.map(Group[T].inverse)
  override def sin(eval: Evaluation[Double]): Evaluation[Double] =
    eval.map(d => Math.sin(d))
  override def cos(eval: Evaluation[Double]): Evaluation[Double] =
    eval.map(d => Math.cos(d))
  override def multiply[T: VectorSpace](evalK: Evaluation[Double], evalT: Evaluation[T]): Evaluation[T] =
    Evaluation.map2(evalK, evalT)(VectorSpace[T].mult)
  override def eq[T: Eq](a: Evaluation[T], b: Evaluation[T]): Evaluation[Boolean] =
    Evaluation.map2(a, b)(Eq[T].eqv)
  override def ifThen[T](condition: Evaluation[Boolean], a: Evaluation[T], b: Evaluation[T]): Evaluation[T] =
    Value(ctx => if (condition.evaluateWith(ctx)) a.evaluateWith(ctx) else b.evaluateWith(ctx))
}
