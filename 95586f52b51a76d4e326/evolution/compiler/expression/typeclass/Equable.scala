package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point

sealed abstract class Equable[T](val t: Type)

object Equable {
  case object Double extends Equable[Double](Type.Double)
  case object Int extends Equable[Int](Type.Integer)
  case object Point extends Equable[Point](Type.Point)
  case object Boolean extends Equable[Boolean](Type.Bool)
}
