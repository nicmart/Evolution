package evolution.compiler.types

import evolution.compiler.types.TypeClasses.Qualified

final class TypeBindings(private val bindings: Map[String, TypeBinding]) {

  def allBindings = bindings

  def getBinding(name: String): Option[TypeBinding] = bindings.get(name)

  def merge(other: TypeBindings): TypeBindings =
    new TypeBindings(bindings ++ other.bindings)

  def withBinding(name: String, binding: TypeBinding): TypeBindings =
    new TypeBindings(bindings.updated(name, binding))

  def withVarBinding(name: String, tpe: Qualified[Type]): TypeBindings =
    withBinding(name, TypeBinding.Fixed(name, tpe))
}

object TypeBindings {
  val empty: TypeBindings = new TypeBindings(Map.empty)
}
