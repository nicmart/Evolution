package evolution.compiler.types

import evolution.compiler.ast.AST.Identifier
import evolution.compiler.phases.typing.TypeInference

final class TypeBindings(private val bindings: Map[String, TypeBinding]) {
  def getIdentifier[M[_]](name: String)(implicit TI: TypeInference[M]): M[Identifier] = {
    bindings.get(name) match {
      case None      => TI.E.raise(s"Unable to find type binding for variable $name")
      case Some(tis) => tis.get[M]
    }
  }

  def merge(other: TypeBindings): TypeBindings =
    new TypeBindings(bindings ++ other.bindings)

  def withBinding(name: String, binding: TypeBinding): TypeBindings =
    new TypeBindings(bindings.updated(name, binding))
}

object TypeBindings {
  val empty: TypeBindings = new TypeBindings(Map.empty)
}
