package evolution.compiler.phases.typing.config

import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{ Type, TypeBinding, TypeBindings }
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types.TypeClassInstance._
import evolution.compiler.expression.typeclass._
import evolution.compiler.expression.typeclass.Multiplicative.LiftRight
import evolution.compiler.expression.typeclass.Multiplicative.LiftLeft
import evolution.compiler.expression.typeclass.Multiplicative.LiftBoth
import evolution.compiler.types.TypeClassInstance

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
  val instances: List[TypeClassInstance] = List(
    // Numeric
    NumericInst(Numeric.Double),
    NumericInst(Numeric.Int),
    // Multiplicative
    MultiplicativeInst(Multiplicative.DoubleDoubleDouble),
    MultiplicativeInst(Multiplicative.DoublePointPoint),
    MultiplicativeInst(Multiplicative.PointDoublePoint),
    MultiplicativeInst(Multiplicative.IntIntInt),
    MultiplicativeInst(Multiplicative.IntDoubleDouble),
    MultiplicativeInst(Multiplicative.DoubleIntDouble),
    MultiplicativeInst(Multiplicative.IntPointPoint),
    MultiplicativeInst(LiftRight(Multiplicative.DoubleDoubleDouble)),
    MultiplicativeInst(LiftLeft(Multiplicative.DoubleDoubleDouble)),
    MultiplicativeInst(LiftRight(Multiplicative.DoublePointPoint)),
    MultiplicativeInst(LiftLeft(Multiplicative.PointDoublePoint)),
    MultiplicativeInst(LiftBoth(Multiplicative.DoubleDoubleDouble)),
    MultiplicativeInst(LiftBoth(Multiplicative.PointDoublePoint)),
    MultiplicativeInst(LiftBoth(Multiplicative.DoublePointPoint)),
    // Additive
    AdditiveInst(Additive.DoubleDoubleDouble),
    AdditiveInst(Additive.IntIntInt),
    AdditiveInst(Additive.IntDoubleDouble),
    AdditiveInst(Additive.DoubleIntDouble),
    AdditiveInst(Additive.PointPointPoint),
    AdditiveInst(Additive.LiftBoth(Additive.DoubleDoubleDouble)),
    AdditiveInst(Additive.LiftBoth(Additive.PointPointPoint)),
    // Invertible
    InvertibleInst(Invertible.Int),
    InvertibleInst(Invertible.Double),
    InvertibleInst(Invertible.Point),
    InvertibleInst(Invertible.Lift(Invertible.Double)),
    InvertibleInst(Invertible.Lift(Invertible.Point)),
    // Equable
    EquableInst(Equable.Boolean),
    EquableInst(Equable.Double),
    EquableInst(Equable.Point),
    EquableInst(Equable.Int),
    // Comparable
    ComparableInst(Comparable.Int),
    ComparableInst(Comparable.Double)
  )

  val instancesPredicates: List[Predicate] = instances.map(instance => Predicate(instance.id, instance.types))

  // TODO it would be nice to returned typed typeclasses
  def numeric(tpe: Type): Either[String, Numeric[Any]] =
    instances.collectFirst {
      case inst @ NumericInst(num) if List(tpe) == inst.types => num
    }.toRight(s"No Numeric instance found for $tpe")

  def additive(tpe1: Type, tpe2: Type, tpe3: Type): Either[String, Additive[Any, Any, Any]] =
    instances.collectFirst {
      case inst @ AdditiveInst(num) if List(tpe1, tpe2, tpe3) == inst.types => num
    }.toRight(s"No Additive instance found for $tpe1, $tpe2, $tpe3")

  def multiplicative(tpe1: Type, tpe2: Type, tpe3: Type): Either[String, Multiplicative[Any, Any, Any]] =
    instances.collectFirst {
      case inst @ MultiplicativeInst(num) if List(tpe1, tpe2, tpe3) == inst.types => num
    }.toRight(s"No Multiplicative instance found for $tpe1, $tpe2, $tpe3")

  def invertible(tpe1: Type): Either[String, Invertible[Any]] =
    instances.collectFirst {
      case inst @ InvertibleInst(num) if List(tpe1) == inst.types => num
    }.toRight(s"No invertible instance found for $tpe1")

  def equable(tpe1: Type): Either[String, Equable[Any]] =
    instances.collectFirst {
      case inst @ EquableInst(eq) if List(tpe1) == inst.types => eq
    }.toRight(s"No equable instance found for $tpe1")

  def comparable(tpe1: Type): Either[String, Comparable[Any]] =
    instances.collectFirst {
      case inst @ ComparableInst(cmp) if List(tpe1) == inst.types => cmp
    }.toRight(s"No comparable instance found for $tpe1")
}
