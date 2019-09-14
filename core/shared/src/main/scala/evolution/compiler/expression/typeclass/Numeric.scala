package evolution.compiler.expression.typeclass
import evolution.compiler.types.TypeT

sealed abstract class Numeric[T](t: TypeT[T])

object Numeric {
  case object Int extends Numeric(TypeT.Integer)
  case object Double extends Numeric(TypeT.Double)
}
