package evolution.compilertree.types

object TypeClasses {
  case class Predicate(id: String, types: List[Type])
  case class Qualified[T](predicates: List[Predicate], value: T)
  object Qualified {
    def apply[T](t: T): Qualified[T] = Qualified(Nil, t)
  }
  case class Default(id: String, tpe: Type)
}
