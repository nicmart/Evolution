package evolution.compiler.expression.typeclass
import evolution.compiler.types.TypeT

sealed abstract class Comparable[T](val t: TypeT[T])

object Comparable {
  case object Int extends Comparable(TypeT.Integer)
  case object Double extends Comparable(TypeT.Double)
}
