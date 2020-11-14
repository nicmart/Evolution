package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed abstract class Invertible[T](val t: Type)

object Invertible {
  case object Int extends Invertible[Int](Type.Integer)
  case object Double extends Invertible[Double](Type.Double)
  case object Point extends Invertible[Point](Type.Point)
  case class Lift[T](inv: Invertible[T]) extends Invertible[Evolution[T]](Type.Evo(inv.t))
}
