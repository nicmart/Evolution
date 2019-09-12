package evolution.compiler.expression.typeclass
import evolution.geometry.Point

sealed trait Equable[T]

object Equable {
  case object Double extends Equable[Double]
  case object Int extends Equable[Int]
  case object Point extends Equable[Point]
  case object Boolean extends Equable[Boolean]
}
