package evolution.compiler.types
import evolution.compiler.expression.typeclass._

// Just a union types of typeclasses
sealed abstract class TypeClassInstance(val id: String)

object TypeClassInstance {
  case class AdditiveInst[A, B, C](add: Additive[A, B, C]) extends TypeClassInstance("Add")
  case class MultiplicativeInst[A, B, C](add: Multiplicative[A, B, C]) extends TypeClassInstance("Mult")
  case class InvertibleInst[A](inv: Invertible[A]) extends TypeClassInstance("Invertible")
  case class NumericInst[A](num: Numeric[A]) extends TypeClassInstance("Num")
  case class EquableInst[A](eq: Equable[A]) extends TypeClassInstance("Eq")
  case class ComparableInst[A](cmp: Comparable[A]) extends TypeClassInstance("Comp")
}

object TypeClasses {

  case class Predicate(id: String, types: List[Type])
  case class Instance(typeclass: TypeClassInstance, types: List[Type]) {
    def predicate: Predicate = Predicate(typeclass.id, types)
  }

  case class Qualified[T](predicates: List[Predicate], value: T)
  object Qualified {
    def apply[T](t: T): Qualified[T] = Qualified(Nil, t)
  }
  case class Default(id: String, tpe: Type)
}
