package evolution.compiler.types

import cats.implicits._
import evolution.compiler.ast.AST.Identifier
import evolution.compiler.phases.typing.{ Assignment, Substitution, TypeInference }
import evolution.compiler.types.TypeClasses.Qualified

sealed abstract class TypeBinding(val name: String, val qt: Qualified[Type]) {
  import TypeInference._
  def get[M[_]](implicit TI: TypeInference[M]): M[Identifier] = {
    this match {
      case TypeBinding.Variable(_, _) => Identifier(name, qt).pure[M]
      case TypeBinding.Predefined(_, _) =>
        val varsInScheme = qt.t.typeVars.toList
        for {
          assignments <- varsInScheme.traverse(
            schemeVar => newTypeVar.map(typeVar => Assignment(schemeVar.name, typeVar.t))
          )
          substitution = Substitution(assignments)
        } yield Identifier(name, substitution.substitute(qt), primitive = true)
    }
  }
}
object TypeBinding {
  case class Variable(override val name: String, override val qt: Qualified[Type]) extends TypeBinding(name, qt)
  case class Predefined(override val name: String, override val qt: Qualified[Type]) extends TypeBinding(name, qt)
}
