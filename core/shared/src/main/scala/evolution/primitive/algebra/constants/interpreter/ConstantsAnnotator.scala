package evolution.primitive.algebra.constants.interpreter

import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation
import evolution.geometry.Point
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr
import evolution.typeclass.VectorSpace

object ConstantsAnnotator extends Constants[Annotation] {
  private val builder = new EvolutionExpr[RNGRepr]

  override def double(d: Double): Annotation[Double] =
    Annotation(Set.empty, builder.constants.double(d))

  override def point(evalX: Annotation[Double], evalY: Annotation[Double]): Annotation[Point] =
    Annotation(evalX.vars ++ evalY.vars, builder.constants.point(evalX.expr, evalY.expr))

  override def add[T: VectorSpace](a: Annotation[T], b: Annotation[T]): Annotation[T] =
    Annotation(a.vars ++ b.vars, builder.constants.add(a.expr, b.expr))

  override def sin(d: Annotation[Double]): Annotation[Double] =
    Annotation(d.vars, builder.constants.sin(d.expr))

  override def cos(d: Annotation[Double]): Annotation[Double] =
    Annotation(d.vars, builder.constants.cos(d.expr))

  override def multiply[T: VectorSpace](k: Annotation[Double], t: Annotation[T]): Annotation[T] =
    Annotation(k.vars ++ t.vars, builder.constants.multiply(k.expr, t.expr))

}
