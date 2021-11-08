package evolution.compiler.types
import evolution.compiler.expression.typeclass.*
import evolution.compiler.types.TypeClasses.Predicate

// Just a union types of typeclasses
case class TypeClassInstance(id: String, types: List[Type], value: Any):
  def predicate: Predicate = Predicate(id, types)

object TypeClasses:
  case class Predicate(id: String, types: List[Type]):
    override def toString: String = s"$id(${types.mkString(", ")})"
    def typeVars: Set[String] =
      types.collect { case Type.Var(varname) =>
        varname
      }.toSet

    def hasTypeVars: Boolean = typeVars.nonEmpty
  case class Qualified[+T](predicates: List[Predicate], value: T):
    def map[S](f: T => S): Qualified[S] = Qualified(predicates, f(value))
    def predicatesTypeVars: Set[String] = predicates.flatMap(_.typeVars).toSet
    override def toString: String =
      if predicates.isEmpty then value.toString
      else s"${predicates.mkString(", ")} => $value"

  object Predicate:
    def apply(id: String): Predicate = Predicate(id, Nil)
    def apply(id: String, vars: String*): Predicate = Predicate(id, vars.toList.map(Type.Var.apply))
    def fromVars(id: String, typeVars: List[String]): Predicate =
      Predicate(id, typeVars.map(Type.Var.apply))

  object Qualified:
    def apply[T](t: T): Qualified[T] = Qualified(Nil, t)

    extension [T](t: T) def ==>:(predicate: Predicate): Qualified[T] = Qualified(List(predicate), t)
