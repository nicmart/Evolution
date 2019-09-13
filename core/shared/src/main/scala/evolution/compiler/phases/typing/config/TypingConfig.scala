package evolution.compiler.phases.typing.config

import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{ Type, TypeBinding, TypeBindings }
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types.TypeClassInstance._
import evolution.compiler.expression.typeclass._
import evolution.compiler.expression.typeclass.Multiplicative.LiftRight
import evolution.compiler.expression.typeclass.Multiplicative.LiftLeft
import evolution.compiler.expression.typeclass.Multiplicative.LiftBoth

object TypingConfig {
  val constantQualifiedTypes: TypeBindings =
    new TypeBindings(
      Constant.values
        .map(constant => constant.entryName -> TypeBinding.Scheme(constant.entryName, constant.qualifiedType))
        .toMap
    )

  /**
   * TODO A lot of coupling between this, All the instances, and Typeclass extraction in Types Module
   */
  import Type.{ Double => Dbl, _ }
  val instances: List[Instance] = List(
    Instance(NumericInst(Numeric.Double), List(Dbl)),
    Instance(NumericInst(Numeric.Int), List(Integer)),
    Instance(MultiplicativeInst(Multiplicative.DoubleDoubleDouble), List(Dbl, Dbl, Dbl)),
    Instance(MultiplicativeInst(Multiplicative.DoublePointPoint), List(Dbl, Point, Point)),
    Instance(MultiplicativeInst(Multiplicative.PointDoublePoint), List(Point, Dbl, Point)),
    Instance(MultiplicativeInst(Multiplicative.IntIntInt), List(Integer, Integer, Integer)),
    Instance(MultiplicativeInst(Multiplicative.IntDoubleDouble), List(Integer, Dbl, Dbl)),
    Instance(MultiplicativeInst(Multiplicative.DoubleIntDouble), List(Dbl, Integer, Dbl)),
    Instance(MultiplicativeInst(Multiplicative.IntPointPoint), List(Integer, Point, Point)),
    Instance(MultiplicativeInst(LiftRight(Multiplicative.DoubleDoubleDouble)), List(Dbl, Evo(Dbl), Evo(Dbl))),
    Instance(MultiplicativeInst(LiftLeft(Multiplicative.DoubleDoubleDouble)), List(Evo(Dbl), Dbl, Evo(Dbl))),
    Instance(MultiplicativeInst(LiftRight(Multiplicative.DoublePointPoint)), List(Dbl, Evo(Point), Evo(Point))),
    Instance(MultiplicativeInst(LiftLeft(Multiplicative.PointDoublePoint)), List(Evo(Point), Dbl, Evo(Point))),
    Instance(MultiplicativeInst(LiftBoth(Multiplicative.DoubleDoubleDouble)), List(Evo(Dbl), Evo(Dbl), Evo(Dbl))),
    Instance(MultiplicativeInst(LiftBoth(Multiplicative.PointDoublePoint)), List(Evo(Point), Evo(Dbl), Evo(Point))),
    Instance(MultiplicativeInst(LiftBoth(Multiplicative.DoublePointPoint)), List(Evo(Dbl), Evo(Point), Evo(Point))),
    Instance(AdditiveInst(Additive.DoubleDoubleDouble), List(Dbl, Dbl, Dbl)),
    Instance(AdditiveInst(Additive.IntIntInt), List(Integer, Integer, Integer)),
    Instance(AdditiveInst(Additive.IntDoubleDouble), List(Integer, Dbl, Dbl)),
    Instance(AdditiveInst(Additive.DoubleIntDouble), List(Dbl, Integer, Dbl)),
    Instance(AdditiveInst(Additive.PointPointPoint), List(Point, Point, Point)),
    Instance(AdditiveInst(Additive.Pointwise(Additive.DoubleDoubleDouble)), List(Evo(Dbl), Evo(Dbl), Evo(Dbl))),
    Instance(AdditiveInst(Additive.Pointwise(Additive.PointPointPoint)), List(Evo(Point), Evo(Point), Evo(Point))),
    Instance(InvertibleInst(Invertible.Int), List(Integer)),
    Instance(InvertibleInst(Invertible.Double), List(Dbl)),
    Instance(InvertibleInst(Invertible.Point), List(Point)),
    Instance(InvertibleInst(Invertible.Lift(Invertible.Double)), List(Evo(Dbl))),
    Instance(InvertibleInst(Invertible.Lift(Invertible.Point)), List(Evo(Point)))
  )

  val instancesPredicates: List[Predicate] = instances.map(_.predicate)
}
