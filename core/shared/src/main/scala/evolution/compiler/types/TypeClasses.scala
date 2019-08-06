package evolution.compiler.types

// Not everything defined here is used yet, but let's keep it, since it is a valid modeling
// of the data types in the paper "Typing Haskell in Haskell"
object TypeClasses {
  case class Predicate(id: String, types: List[Type])
  case class Qualified[T](predicates: List[Predicate], t: T)
  object Qualified {
    def apply[T](t: T): Qualified[T] = Qualified(Nil, t)
  }
  case class Default(id: String, tpe: Type)
}
