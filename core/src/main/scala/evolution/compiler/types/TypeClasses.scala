package evolution.compiler.types
import evolution.compiler.expression.typeclass._
import evolution.compiler.types.TypeClasses.Predicate

// Just a union types of typeclasses
sealed abstract class TypeClassInstance(val id: String, val types: List[Type]) {
  def predicate: Predicate = Predicate(id, types)
}

object TypeClassInstance {
  case class AdditiveInst[A, B, C](add: Additive[A, B, C])
      extends TypeClassInstance("Add", List(add.t1, add.t2, add.t3))

  case class MultiplicativeInst[A, B, C](mult: Multiplicative[A, B, C])
      extends TypeClassInstance("Mult", List(mult.t1, mult.t2, mult.t3))

  case class InvertibleInst[A](inv: Invertible[A]) extends TypeClassInstance("Invertible", List(inv.t))

  case class NumericInst[A](num: Numeric[A]) extends TypeClassInstance("Num", List(num.t))

  case class EquableInst[A](eq: Equable[A]) extends TypeClassInstance("Eq", List(eq.t))

  case class ComparableInst[A](cmp: Comparable[A]) extends TypeClassInstance("Comp", List(cmp.t))
}

object TypeClasses {

  case class Predicate(id: String, types: List[Type]) {
    override def toString: String = s"$id(${types.mkString(", ")})"
    def typeVars: Set[String] =
      types.collect {
        case Type.Var(varname) => varname
      }.toSet
  }
  case class Qualified[+T](predicates: List[Predicate], value: T) {
    def map[S](f: T => S): Qualified[S] = Qualified(predicates, f(value))
    def predicatesTypeVars: Set[String] = predicates.flatMap(_.typeVars).toSet
    override def toString: String =
      if (predicates.isEmpty) value.toString
      else
        s"${predicates.mkString(", ")} => $value"
  }

  object Predicate {
    def fromVars(id: String, typeVars: List[String]): Predicate =
      Predicate(id, typeVars.map(Type.Var))
  }

  object Qualified {
    def apply[T](t: T): Qualified[T] = Qualified(Nil, t)
  }
}
