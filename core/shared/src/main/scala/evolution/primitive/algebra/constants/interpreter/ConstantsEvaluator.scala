package evolution.primitive.algebra.constants.interpreter

import evolution.geometry.Point
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace
import evolution.data.AnnotationModule._

object ConstantsEvaluator extends Constants[R] {
  override def double(d: Double): R[Double] =
    Annotation(Set.empty, builder.constants.double(d))

  override def point(evalX: R[Double], evalY: R[Double]): R[Point] =
    Annotation(evalX.vars ++ evalY.vars, builder.constants.point(evalX.expr, evalY.expr))

  override def add[T: VectorSpace](a: R[T], b: R[T]): R[T] =
    Annotation(a.vars ++ b.vars, builder.constants.add(a.expr, b.expr))

  override def sin(d: R[Double]): R[Double] =
    Annotation(d.vars, builder.constants.sin(d.expr))

  override def cos(d: R[Double]): R[Double] =
    Annotation(d.vars, builder.constants.cos(d.expr))

  override def multiply[T: VectorSpace](k: R[Double], t: R[T]): R[T] =
    Annotation(k.vars ++ t.vars, builder.constants.multiply(k.expr, t.expr))

}
