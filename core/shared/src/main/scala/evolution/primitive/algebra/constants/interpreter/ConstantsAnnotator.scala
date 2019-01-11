package evolution.primitive.algebra.constants.interpreter

import cats.Group
import cats.kernel.{ Eq, Semigroup }
import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation
import evolution.data.Annotation.Info.Unknown
import evolution.geometry.Point
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr
import evolution.typeclass.VectorSpace

object ConstantsAnnotator extends Constants[Annotation] {
  private val builder = new EvolutionExpr[RNGRepr]

  override def int(n: Int): Annotation[Int] =
    Annotation(Set.empty, Unknown(builder.constants.int(n)))

  override def double(d: Double): Annotation[Double] =
    Annotation(Set.empty, Unknown(builder.constants.double(d)))

  override def point(evalX: Annotation[Double], evalY: Annotation[Double]): Annotation[Point] =
    Annotation(evalX.vars ++ evalY.vars, Unknown(builder.constants.point(evalX.expr, evalY.expr)))

  override def add[T: Semigroup](a: Annotation[T], b: Annotation[T]): Annotation[T] =
    Annotation(a.vars ++ b.vars, Unknown(builder.constants.add(a.expr, b.expr)))

  override def inverse[T: Group](a: Annotation[T]): Annotation[T] =
    Annotation(a.vars, Unknown(builder.constants.inverse(a.expr)))

  override def sin(d: Annotation[Double]): Annotation[Double] =
    Annotation(d.vars, Unknown(builder.constants.sin(d.expr)))

  override def cos(d: Annotation[Double]): Annotation[Double] =
    Annotation(d.vars, Unknown(builder.constants.cos(d.expr)))

  override def multiply[T: VectorSpace](k: Annotation[Double], t: Annotation[T]): Annotation[T] =
    Annotation(k.vars ++ t.vars, Unknown(builder.constants.multiply(k.expr, t.expr)))

  override def eq[T: Eq](a: Annotation[T], b: Annotation[T]): Annotation[Boolean] =
    Annotation(a.vars ++ b.vars, Unknown(builder.constants.eq(a.expr, b.expr)))

  override def ifThen[T](condition: Annotation[Boolean], a: Annotation[T], b: Annotation[T]): Annotation[T] =
    Annotation(condition.vars ++ a.vars ++ b.vars, Unknown(builder.constants.ifThen(condition.expr, a.expr, b.expr)))
}
