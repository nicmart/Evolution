package evolution.compiler.expression.typeclass
import evolution.geometry.Point
import evolution.compiler.types.TypeT

sealed abstract class Equable[T](val t: TypeT[T])

object Equable {
  case object Double extends Equable(TypeT.Double)
  case object Int extends Equable(TypeT.Integer)
  case object Point extends Equable(TypeT.Point)
  case object Boolean extends Equable(TypeT.Bool)
}
