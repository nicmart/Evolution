package evolution.compiler.types

final class TypeBindings(bindings: Map[String, Type]) {
  def has(name: String): Boolean = bindings.isDefinedAt(name)
  def get(name: String): Option[Type] = bindings.get(name)
  def put(name: String, tpe: Type): TypeBindings = new TypeBindings(bindings.updated(name, tpe))
  def nextTypeVar: Type = Type.Var("X" + bindings.size)
}

object TypeBindings {
  val empty: TypeBindings = new TypeBindings(Map.empty)
}
