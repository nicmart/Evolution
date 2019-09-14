package evolution.compiler.expression.typeclass
import evolution.compiler.types.TypeT

sealed abstract class Invertible[T](val t: TypeT[T])

object Invertible {
  case object Int extends Invertible(TypeT.Integer)
  case object Double extends Invertible(TypeT.Double)
  case object Point extends Invertible(TypeT.Point)
  case class Lift[T](inv: Invertible[T]) extends Invertible(TypeT.Evo(inv.t))
}
