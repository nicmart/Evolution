package evolution.compiler.phases.typing.config

import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{ Type, TypeT, TypeBinding, TypeBindings }
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
  import TypeT.{ Double => Dbl, _ }
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
    Instance(InvertibleInst(Invertible.Lift(Invertible.Point)), List(Evo(Point))),
    Instance(EquableInst(Equable.Boolean), List(TypeT.Bool)),
    Instance(EquableInst(Equable.Double), List(TypeT.Double)),
    Instance(EquableInst(Equable.Point), List(TypeT.Point)),
    Instance(EquableInst(Equable.Int), List(TypeT.Integer)),
    Instance(ComparableInst(Comparable.Int), List(TypeT.Integer)),
    Instance(ComparableInst(Comparable.Double), List(TypeT.Double))
  )

  val instancesPredicates: List[Predicate] = instances.map(_.predicate)

  def numeric(tpe: Type): Either[String, Numeric[_]] =
    instances.collectFirst {
      case Instance(NumericInst(num), types) if List(tpe) == types => num
    }.toRight(s"No Numeric instance found for $tpe")

  def additive(tpe1: Type, tpe2: Type, tpe3: Type): Either[String, Additive[_, _, _]] =
    instances.collectFirst {
      case Instance(AdditiveInst(num), types) if List(tpe1, tpe2, tpe3) == types => num
    }.toRight(s"No Additive instance found for $tpe1, $tpe2, $tpe3")

  def multiplicative(tpe1: Type, tpe2: Type, tpe3: Type): Either[String, Multiplicative[_, _, _]] =
    instances.collectFirst {
      case Instance(MultiplicativeInst(num), types) if List(tpe1, tpe2, tpe3) == types => num
    }.toRight(s"No Multiplicative instance found for $tpe1, $tpe2, $tpe3")

  def invertible(tpe1: Type): Either[String, Invertible[_]] =
    instances.collectFirst {
      case Instance(InvertibleInst(num), types) if List(tpe1) == types => num
    }.toRight(s"No invertible instance found for $tpe1")

  def equable(tpe1: Type): Either[String, Equable[_]] =
    instances.collectFirst {
      case Instance(EquableInst(eq), types) if List(tpe1) == types => eq
    }.toRight(s"No equable instance found for $tpe1")

  def comparable(tpe1: Type): Either[String, Comparable[_]] =
    instances.collectFirst {
      case Instance(ComparableInst(cmp), types) if List(tpe1) == types => cmp
    }.toRight(s"No comparable instance found for $tpe1")
}
