package evolution.compiler.phases.typer.config

import evolution.compiler.expression.typeclass.Multiplicative.{LiftBoth, LiftLeft, LiftRight}
import evolution.compiler.expression.typeclass.*
import evolution.compiler.phases.typer.model.Assumptions
import evolution.compiler.term.{Definition, Module, Term}
import evolution.compiler.types.TypeClassInstance.*
import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{Type, TypeClassInstance}

object TypeclassConfig:
  enum TypeClassId:
    case Num
    case Comp
    case Eq
    case Invertible
    case Mult
    case Add

  val constantsModule: Module = Module(NativeSymbolsConfig.symbols.map { const =>
    Definition(const.symbol, Some(Term.Value(const.value)), const.tpe)
  })

  val constantQualifiedTypes: Assumptions = constantsModule.assumptions

  /**
   * TODO A lot of coupling between this, All the instances, and Typeclass extraction in Types Module
   */
  val instances: List[TypeClassInstance] = List(
    // Numeric
    Numeric.Double.instance,
    Numeric.Int.instance,
    // Multiplicative
    Multiplicative.IntIntInt.instance,
    Multiplicative.DoubleDoubleDouble.instance,
    Multiplicative.IntPointPoint.instance,
    Multiplicative.DoublePointPoint.instance,
    Multiplicative.PointDoublePoint.instance,
    Multiplicative.IntDoubleDouble.instance,
    Multiplicative.DoubleIntDouble.instance,
    LiftRight(Multiplicative.DoubleDoubleDouble).instance,
    LiftLeft(Multiplicative.DoubleDoubleDouble).instance,
    LiftRight(Multiplicative.DoublePointPoint).instance,
    LiftLeft(Multiplicative.PointDoublePoint).instance,
    LiftBoth(Multiplicative.DoubleDoubleDouble).instance,
    LiftBoth(Multiplicative.PointDoublePoint).instance,
    LiftBoth(Multiplicative.DoublePointPoint).instance,
    // Additive
    Additive.IntIntInt.instance,
    Additive.DoubleIntDouble.instance,
    Additive.DoubleDoubleDouble.instance,
    Additive.IntDoubleDouble.instance,
    Additive.PointPointPoint.instance,
    Additive.LiftBoth(Additive.DoubleDoubleDouble).instance,
    Additive.LiftBoth(Additive.PointPointPoint).instance,
    Additive.LiftRight(Additive.DoubleDoubleDouble).instance,
    Additive.LiftLeft(Additive.DoubleDoubleDouble).instance,
    Additive.LiftRight(Additive.PointPointPoint).instance,
    Additive.LiftLeft(Additive.PointPointPoint).instance,
    // Invertible
    Invertible.Int.instance,
    Invertible.Double.instance,
    Invertible.Point.instance,
    Invertible.Lift(Invertible.Double).instance,
    Invertible.Lift(Invertible.Point).instance,
    // Equable
    Equable.Boolean.instance,
    Equable.Double.instance,
    Equable.Point.instance,
    Equable.Int.instance,
    // Comparable
    Comparable.Int.instance,
    Comparable.Double.instance
  )

  val instancesPredicates: List[Predicate] = instances.map(instance => Predicate(instance.id, instance.types))

  def instance(typeClassId: TypeClassId, types: Type*): Either[String, TypeClassInstance] =
    instances
      .find(instance => instance.id == typeClassId.toString && instance.types == types.toList)
      .toRight(s"No $typeClassId instance found for $types")

  def instance(predicate: Predicate): Either[String, TypeClassInstance] =
    instance(TypeClassId.valueOf(predicate.id), predicate.types*).left.map(_ =>
      s"No instance found for predicate $predicate"
    )
