package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type

sealed abstract class Comparable[T](val t: Type)

object Comparable:
  case object Int extends Comparable[Int](Type.Integer)
  case object Double extends Comparable[Double](Type.Double)
