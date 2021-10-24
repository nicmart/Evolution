package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type

sealed abstract class Numeric[T](val t: Type)

object Numeric:
  case object Int extends Numeric[Int](Type.Integer)
  case object Double extends Numeric[Double](Type.Double)
